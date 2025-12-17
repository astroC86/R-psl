.tinydsl_state <- new.env(parent = emptyenv())
.tinydsl_state$ctx <- NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

td_type <- function(kind, ...) {
  structure(list(kind = kind, ...), class = "td_type")
}

cxx_type <- function(type) {
  stopifnot(inherits(type, "td_type"))
  if (type$kind == "prim") return(type$cxx)
  if (type$kind == "ptr") {
    if (is.null(type$elem)) return("void*")
    return(paste0(cxx_type(type$elem), "*"))
  }
  if (type$kind == "rcpp_vector") {
    if (type$elem$name == "i32") return("Rcpp::IntegerVector")
    if (type$elem$name == "f64") return("Rcpp::NumericVector")
    stop("Unsupported rcpp_vector element type: ", type$elem$name, call. = FALSE)
  }
  if (type$kind == "rcpp_matrix") {
    if (type$elem$name == "i32") return("Rcpp::IntegerMatrix")
    if (type$elem$name == "f64") return("Rcpp::NumericMatrix")
    stop("Unsupported rcpp_matrix element type: ", type$elem$name, call. = FALSE)
  }
  stop("Unsupported type kind: ", type$kind, call. = FALSE)
}

ty <- new.env(parent = emptyenv())
ty$i32 <- td_type("prim", name = "i32", cxx = "int32_t")
ty$u32 <- td_type("prim", name = "u32", cxx = "uint32_t")
ty$f64 <- td_type("prim", name = "f64", cxx = "double")
ty$bool <- td_type("prim", name = "bool", cxx = "bool")

ty$ptr_mut <- function(elem) td_type("ptr", mut = "mut", elem = elem)
ty$ptr_const <- function(elem) td_type("ptr", mut = "const", elem = elem)
ty$rcpp_vector_mut <- function(elem) td_type("rcpp_vector", mut = "mut", elem = elem)
ty$rcpp_vector_const <- function(elem) td_type("rcpp_vector", mut = "const", elem = elem)
ty$rcpp_matrix_mut <- function(elem) td_type("rcpp_matrix", mut = "mut", elem = elem)
ty$rcpp_matrix_const <- function(elem) td_type("rcpp_matrix", mut = "const", elem = elem)

td_expr <- function(code, type = NULL) {
  structure(list(code = code, type = type), class = "td_expr")
}

td_code <- function(x) {
  if (inherits(x, "td_expr")) return(x$code)
  if (inherits(x, "td_ptr")) return(x$base)
  if (inherits(x, "td_rcpp_vector") || inherits(x, "td_rcpp_matrix")) return(x$name)
  if (is.numeric(x) && length(x) == 1) return(as.character(x))
  if (is.character(x) && length(x) == 1) return(x)
  stop("Unsupported value in codegen: ", paste(class(x), collapse = "/"), call. = FALSE)
}

td_const <- function(value, type) {
  stopifnot(length(value) == 1)
  if (!inherits(type, "td_type") || type$kind != "prim") stop("const() requires a primitive type", call. = FALSE)
  if (type$name == "u32")  return(td_expr(paste0(as.integer(value), "U"), type))
  if (type$name == "i32")  return(td_expr(as.character(as.integer(value)), type))
  if (type$name == "bool") return(td_expr(if (isTRUE(value)) "true" else "false", type))
  td_expr(as.character(value), type)
}

Ops.td_expr <- function(e1, e2 = NULL) {
  op <- .Generic

  if (missing(e2)) {
    if (op %in% c("+", "-")) {
      return(td_expr(paste0("(", op, td_code(e1), ")"), e1$type))
    }
    stop("Unsupported unary op for td_expr: ", op, call. = FALSE)
  }

  if (op %in% c("+", "-", "*", "/", "%/%", "%%", "<", "<=", ">", ">=", "==", "!=")) {
    left <- td_code(e1)
    right <- td_code(e2)
    out_type <- e1$type %||% (if (inherits(e2, "td_expr")) e2$type else NULL)
    if (op %in% c("<", "<=", ">", ">=", "==", "!=")) out_type <- ty$bool
    cxx_op <- switch(op, "%/%" = "/", "%%" = "%", op)
    return(td_expr(paste0("(", left, " ", cxx_op, " ", right, ")"), out_type))
  }

  if (op %in% c("&", "|")) {
    left <- td_code(e1)
    right <- td_code(e2)
    cxx_op <- if (op == "&") "&&" else "||"
    return(td_expr(paste0("(", left, " ", cxx_op, " ", right, ")"), ty$bool))
  }

  stop("Unsupported op for td_expr: ", op, call. = FALSE)
}

td_ptr <- function(name, type) {
  stopifnot(inherits(type, "td_type"), type$kind == "ptr")
  structure(list(name = name, base = name, type = type), class = "td_ptr")
}

td_ptr_base <- function(base, type) {
  stopifnot(inherits(type, "td_type"), type$kind == "ptr")
  structure(list(name = NULL, base = base, type = type), class = "td_ptr")
}

`[.td_ptr` <- function(x, i) {
  elem <- x$type$elem
  if (is.null(elem)) stop("Cannot index void* pointer", call. = FALSE)
  td_expr(paste0(x$base, "[", td_code(i), "]"), elem)
}

`[<-.td_ptr` <- function(x, i, value) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("Pointer assignment used outside tinydsl render context", call. = FALSE)
  ctx$emit(paste0(x$base, "[", td_code(i), "] = ", td_code(value), ";"))
  x
}

td_rcpp_vector <- function(name, type) {
  stopifnot(inherits(type, "td_type"), type$kind == "rcpp_vector")
  structure(list(name = name, type = type), class = "td_rcpp_vector")
}

td_rcpp_matrix <- function(name, type) {
  stopifnot(inherits(type, "td_type"), type$kind == "rcpp_matrix")
  structure(list(name = name, type = type), class = "td_rcpp_matrix")
}

cast <- function(x, target_type) {
  if (!inherits(target_type, "td_type")) stop("target_type must be a tinydsl type", call. = FALSE)
  if ((inherits(x, "td_rcpp_vector") || inherits(x, "td_rcpp_matrix")) && target_type$kind == "ptr") {
    elem <- target_type$elem
    elem_cxx <- if (is.null(elem)) "void" else cxx_type(elem)
    return(td_ptr_base(paste0("reinterpret_cast<", elem_cxx, "*>(", x$name, ".begin())"), target_type))
  }
  if (inherits(x, "td_expr") && target_type$kind == "prim") {
    return(td_expr(paste0("(", cxx_type(target_type), ")(", td_code(x), ")"), target_type))
  }
  stop("Unsupported cast() combination", call. = FALSE)
}

len <- function(x) {
  if (inherits(x, "td_rcpp_vector") || inherits(x, "td_rcpp_matrix")) {
    return(td_expr(paste0(x$name, ".size()"), ty$u32))
  }
  stop("len() only supports Rcpp vectors/matrices", call. = FALSE)
}

raw_expr <- function(fmt, out_type, ...) {
  args <- list(...)
  out <- fmt
  nms <- names(args)
  if (!is.null(nms) && any(nzchar(nms))) {
    for (nm in nms) {
      out <- gsub(paste0("$", nm), td_code(args[[nm]]), out, fixed = TRUE)
    }
  }
  for (idx in seq_along(args)) {
    out <- gsub(paste0("$", idx - 1L), td_code(args[[idx]]), out, fixed = TRUE)
  }
  td_expr(out, out_type)
}

td_ctx <- function() {
  ctx <- new.env(parent = emptyenv())
  ctx$lines <- character()
  ctx$indent <- 1L
  ctx$emit <- function(line = "") {
    prefix <- paste(rep("    ", ctx$indent), collapse = "")
    ctx$lines <- c(ctx$lines, paste0(prefix, line))
    invisible(NULL)
  }
  ctx$with_indent <- function(expr) {
    ctx$indent <- ctx$indent + 1L
    on.exit({ ctx$indent <- ctx$indent - 1L }, add = TRUE)
    force(expr)
    invisible(NULL)
  }
  ctx
}

let <- function(sym, value, type = NULL) {
  sym <- substitute(sym)
  if (!is.symbol(sym)) stop("let() first argument must be a symbol", call. = FALSE)
  name <- as.character(sym)
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("let() used outside tinydsl render context", call. = FALSE)
  if (is.null(type)) {
    if (is.null(value$type)) stop("let() requires a type or a typed value", call. = FALSE)
    type <- value$type
  }
  ctx$emit(paste0(cxx_type(type), " ", name, " = ", td_code(value), ";"))
  if (type$kind == "prim") return(td_expr(name, type))
  if (type$kind == "ptr") return(td_ptr(name, type))
  if (type$kind == "rcpp_vector") return(td_rcpp_vector(name, type))
  if (type$kind == "rcpp_matrix") return(td_rcpp_matrix(name, type))
  stop("Unsupported let() type kind: ", type$kind, call. = FALSE)
}

if_ <- function(cond, expr) {
  expr_quoted <- substitute(expr)
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("if_() used outside tinydsl render context", call. = FALSE)
  ctx$emit(paste0("if (", td_code(cond), ") {"))
  ctx$with_indent(eval(expr_quoted, envir = parent.frame()))
  ctx$emit("}")
  invisible(NULL)
}

for_ <- function(sym, lo, hi, step = NULL, expr) {
  expr_quoted <- substitute(expr)
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("for_() used outside tinydsl render context", call. = FALSE)
  sym <- substitute(sym)
  if (!is.symbol(sym)) stop("for_() first argument must be a symbol", call. = FALSE)
  name <- as.character(sym)

  if (is.null(step)) step <- td_const(1, ty$u32)

  ctx$emit(paste0("for (uint32_t ", name, " = ", td_code(lo), "; ", name, " < ", td_code(hi), "; ", name, " += ", td_code(step), ") {"))
  ctx$with_indent({
    env <- new.env(parent = parent.frame())
    env[[name]] <- td_expr(name, ty$u32)
    eval(expr_quoted, envir = env)
  })
  ctx$emit("}")
  invisible(NULL)
}

ret <- function(value = NULL) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("ret() used outside tinydsl render context", call. = FALSE)
  if (is.null(value)) {
    ctx$emit("return;")
  } else {
    ctx$emit(paste0("return ", td_code(value), ";"))
  }
  invisible(NULL)
}

raw_stmt <- function(fmt, ...) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("raw_stmt() used outside tinydsl render context", call. = FALSE)
  args <- list(...)
  out <- fmt
  for (idx in seq_along(args)) {
    out <- gsub(paste0("$", idx - 1L), td_code(args[[idx]]), out, fixed = TRUE)
  }
  ctx$emit(out)
  invisible(NULL)
}

td_func <- function(kind, params, body, host = FALSE, device = FALSE, force_inline = FALSE, ret = NULL) {
  stopifnot(inherits(params, "td_params"))
  if (!is.function(body)) stop("body must be a function", call. = FALSE)
  structure(
    list(
      kind = kind,
      params = params,
      body = body,
      host = isTRUE(host),
      device = isTRUE(device),
      force_inline = isTRUE(force_inline),
      ret = ret
    ),
    class = "td_func"
  )
}

Params <- function(...) {
  types <- list(...)
  if (length(types) == 0) stop("Params(...) requires at least one parameter", call. = FALSE)
  if (is.null(names(types)) || any(names(types) == "")) stop("Params(...) requires named arguments", call. = FALSE)
  for (nm in names(types)) if (!inherits(types[[nm]], "td_type")) stop("Param '", nm, "' must be a tinydsl type", call. = FALSE)
  structure(types, class = "td_params")
}

kernel <- function(params, body = NULL, raw_attrs = NULL) {
  if (is.null(body)) return(function(body) kernel(params, body = body, raw_attrs = raw_attrs))
  td_func("kernel", params, body)
}

fn <- function(params, body = NULL, host = FALSE, device = FALSE, force_inline = FALSE, ret = NULL, raw_attrs = NULL) {
  if (is.null(body)) return(function(body) fn(params, body = body, host = host, device = device, force_inline = force_inline, ret = ret, raw_attrs = raw_attrs))
  td_func("fn", params, body, host = host, device = device, force_inline = force_inline, ret = ret)
}

compile_func_body <- function(func, export_name) {
  stopifnot(inherits(func, "td_func"))
  ctx <- td_ctx()
  old <- .tinydsl_state$ctx
  .tinydsl_state$ctx <- ctx
  on.exit({ .tinydsl_state$ctx <- old }, add = TRUE)

  params <- func$params
  arg_objs <- list()
  for (nm in names(params)) {
    pty <- params[[nm]]
    if (pty$kind == "prim") {
      arg_objs[[nm]] <- td_expr(nm, pty)
    } else if (pty$kind == "ptr") {
      arg_objs[[nm]] <- td_ptr(nm, pty)
    } else if (pty$kind == "rcpp_vector") {
      arg_objs[[nm]] <- td_rcpp_vector(nm, pty)
    } else if (pty$kind == "rcpp_matrix") {
      arg_objs[[nm]] <- td_rcpp_matrix(nm, pty)
    } else {
      stop("Unsupported param kind: ", pty$kind, call. = FALSE)
    }
  }

  do.call(func$body, arg_objs)
  ctx$lines
}

func_signature <- function(func, code_name, dialect = c("cpp", "cuda")) {
  dialect <- match.arg(dialect)
  stopifnot(inherits(func, "td_func"))
  name <- code_name
  ret <- if (func$kind == "kernel") "void" else if (is.null(func$ret)) "void" else cxx_type(func$ret)

  attrs <- character()
  if (dialect == "cuda") {
    if (func$kind == "kernel") attrs <- c(attrs, "__global__")
    if (func$kind == "fn" && func$host) attrs <- c(attrs, "__host__")
    if (func$kind == "fn" && func$device) attrs <- c(attrs, "__device__")
    if (func$kind == "fn" && func$force_inline) attrs <- c(attrs, "__forceinline__")
  }

  params <- func$params
  param_lines <- character()
  for (nm in names(params)) {
    param_lines <- c(param_lines, paste0("    ", cxx_type(params[[nm]]), " ", nm))
  }
  list(attrs = attrs, ret = ret, name = name, param_lines = param_lines)
}

render_one_decl <- function(sig, with_semicolon = TRUE) {
  lines <- character()
  for (a in sig$attrs) lines <- c(lines, a)
  lines <- c(lines, paste0(sig$ret, " ", sig$name, "("))
  if (length(sig$param_lines) > 0) {
    lines <- c(lines, paste0(sig$param_lines, ifelse(seq_along(sig$param_lines) == length(sig$param_lines), "", ",")))
  }
  lines <- c(lines, paste0(")", if (with_semicolon) ";" else " {"))
  lines
}

render <- function(entry_points, headers = character(), bind = FALSE, bind_name = NULL, bind_style = c("none", "rcpp_module", "rcpp_export"), dialect = c("cpp", "cuda")) {
  dialect <- match.arg(dialect)
  bind_style <- match.arg(bind_style)
  if (inherits(entry_points, "td_func")) {
    entry_points <- list(entry_points)
  }
  if (!is.list(entry_points) || length(entry_points) == 0) stop("entry_points must be a non-empty list", call. = FALSE)

  export_names <- names(entry_points)
  if (is.null(export_names)) export_names <- rep(NA_character_, length(entry_points))

  entries <- vector("list", length(entry_points))
  for (i in seq_along(entry_points)) {
    func <- entry_points[[i]]
    name <- export_names[[i]]
    if (!inherits(func, "td_func")) stop("All entry points must be tinydsl functions", call. = FALSE)
    export_name <- if (!is.na(name) && nzchar(name)) name else NULL
    code_name <- export_name %||% paste0(if (func$kind == "kernel") "kernel" else "fn", "_", i)
    entries[[i]] <- list(func = func, export_name = export_name, code_name = code_name)
  }

  out <- character()
  for (h in headers) out <- c(out, paste0("#include <", h, ">"))
  if (length(headers) > 0) out <- c(out, "")

  out <- c(out, "// prototypes", "")

  exported <- Filter(function(e) !is.null(e$export_name), entries)
  internal <- Filter(function(e) is.null(e$export_name), entries)

  for (e in exported) {
    sig <- func_signature(e$func, e$code_name, dialect = dialect)
    out <- c(out, render_one_decl(sig, with_semicolon = TRUE), "")
  }

  if (length(internal) > 0) {
    out <- c(out, "namespace internal {", "")
    for (e in internal) {
      sig <- func_signature(e$func, e$code_name, dialect = dialect)
      out <- c(out, render_one_decl(sig, with_semicolon = TRUE), "")
    }
    out <- c(out, "} // namespace internal", "")
  }

  out <- c(out, "// definitions", "")

  for (e in exported) {
    sig <- func_signature(e$func, e$code_name, dialect = dialect)
    if (isTRUE(bind) && bind_style == "rcpp_export") {
      out <- c(out, "// [[Rcpp::export]]")
    }
    out <- c(out, render_one_decl(sig, with_semicolon = FALSE))
    body_lines <- compile_func_body(e$func, e$export_name)
    out <- c(out, body_lines, "}", "")
  }

  if (length(internal) > 0) {
    out <- c(out, "namespace internal {", "")
    for (e in internal) {
      sig <- func_signature(e$func, e$code_name, dialect = dialect)
      out <- c(out, render_one_decl(sig, with_semicolon = FALSE))
      body_lines <- compile_func_body(e$func, e$export_name)
      out <- c(out, body_lines, "}", "")
    }
    out <- c(out, "} // namespace internal", "")
  }

  if (isTRUE(bind) && bind_style == "rcpp_module") {
    if (is.null(bind_name) || !nzchar(bind_name)) stop("bind_name is required when bind_style='rcpp_module'", call. = FALSE)
    if (!(bind_name %in% vapply(exported, function(e) e$code_name, character(1)))) {
      stop("bind_name must match a named entry point in entry_points", call. = FALSE)
    }
    out <- c(out, "#include <Rcpp.h>", "", "RCPP_MODULE(tinydsl_module){", paste0("    Rcpp::function(\"", bind_name, "\", &", bind_name, ", \"\");"), "}", "")
  }

  paste0(out, collapse = "\n")
}

compile_cpp <- function(entry_points, headers = c("Rcpp.h", "cstdint"), env = parent.frame(), verbose = FALSE, rebuild = FALSE) {
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Rcpp is required for compile_cpp()", call. = FALSE)
  code <- render(entry_points, headers = headers, bind = TRUE, bind_style = "rcpp_export", dialect = "cpp")
  Rcpp::sourceCpp(code = code, env = env, verbose = verbose, rebuild = rebuild)
  exported_names <- names(entry_points)
  exported_names <- exported_names[!is.na(exported_names) & nzchar(exported_names)]
  if (length(exported_names) == 1) return(get(exported_names[[1]], envir = env))
  invisible(lapply(exported_names, function(nm) get(nm, envir = env)))
}

split_flags <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(character())
  pieces <- unlist(strsplit(x, "\\s+"))
  pieces[nzchar(pieces)]
}

cpp_type_for <- function(type) {
  stopifnot(inherits(type, "td_type"))
  if (type$kind != "prim") stop("cpp_type_for only supports primitive types", call. = FALSE)
  if (type$name == "i32") return("int32_t")
  if (type$name == "u32") return("uint32_t")
  if (type$name == "f64") return("double")
  if (type$name == "bool") return("bool")
  stop("Unsupported primitive type: ", type$name, call. = FALSE)
}

rcpp_sexp_to_cpp <- function(sexp_name, param_name, type) {
  stopifnot(inherits(type, "td_type"))
  var_name <- paste0(param_name, "_")
  if (type$kind == "prim") {
    cpp_ty <- cpp_type_for(type)
    return(list(
      decl = paste0(cpp_ty, " ", var_name, " = Rcpp::as<", cpp_ty, ">(", sexp_name, ");"),
      var = var_name
    ))
  }
  if (type$kind == "rcpp_vector") {
    cpp_ty <- cxx_type(type)
    return(list(
      decl = paste0(cpp_ty, " ", var_name, "(", sexp_name, ");"),
      var = var_name
    ))
  }
  if (type$kind == "rcpp_matrix") {
    cpp_ty <- cxx_type(type)
    return(list(
      decl = paste0(cpp_ty, " ", var_name, "(", sexp_name, ");"),
      var = var_name
    ))
  }
  stop("Unsupported param type in CUDA .Call wrapper: ", type$kind, call. = FALSE)
}

rcpp_wrap_return <- function(value_expr, ret_type) {
  if (is.null(ret_type)) return(c(paste0(value_expr, ";"), "return R_NilValue;"))
  stopifnot(inherits(ret_type, "td_type"))
  return(c(paste0("auto res = ", value_expr, ";"), "return Rcpp::wrap(res);"))
}


compile_cuda <- function(entry_points, headers = c("Rcpp.h", "cuda_runtime.h", "cstdint"), wrapper_prefix = "td_call_", verbose = FALSE) {
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Rcpp is required for compile_cuda()", call. = FALSE)

  nvcc <- Sys.which("nvcc")
  if (!nzchar(nvcc)) stop("nvcc not found on PATH; cannot compile CUDA", call. = FALSE)

  if (!is.list(entry_points) || length(entry_points) == 0) stop("entry_points must be a non-empty list", call. = FALSE)
  export_names <- names(entry_points)
  if (is.null(export_names)) export_names <- rep(NA_character_, length(entry_points))
  exported <- export_names[!is.na(export_names) & nzchar(export_names)]
  if (length(exported) == 0) stop("compile_cuda() requires named exported entry points", call. = FALSE)

  code <- render(entry_points, headers = headers, bind = FALSE, dialect = "cuda")

  wrapper_lines <- c(
    "",
    "// ---- tinydsl .Call wrappers ----",
    "#include <Rcpp.h>",
    "extern \"C\" {"
  )

  callable <- character()
  for (nm in exported) {
    func <- entry_points[[nm]]
    if (!inherits(func, "td_func")) stop("All entry points must be tinydsl functions", call. = FALSE)
    if (identical(func$kind, "kernel")) next
    callable <- c(callable, nm)

    params <- func$params
    pnames <- names(params)
    sexp_args <- paste0("SEXP ", pnames)
    wrapper_name <- paste0(wrapper_prefix, nm)

    wrapper_lines <- c(wrapper_lines, paste0("RcppExport SEXP ", wrapper_name, "(", paste(sexp_args, collapse = ", "), ") {"))
    wrapper_lines <- c(wrapper_lines, "    BEGIN_RCPP;")

    converted_vars <- character()
    for (pn in pnames) {
      conv <- rcpp_sexp_to_cpp(pn, pn, params[[pn]])
      wrapper_lines <- c(wrapper_lines, paste0("    ", conv$decl))
      converted_vars <- c(converted_vars, conv$var)
    }

    call_expr <- paste0(nm, "(", paste(converted_vars, collapse = ", "), ")")
    ret_lines <- rcpp_wrap_return(call_expr, func$ret)
    wrapper_lines <- c(wrapper_lines, paste0("    ", ret_lines))

    wrapper_lines <- c(wrapper_lines, "    END_RCPP;", "}", "")
  }

  wrapper_lines <- c(wrapper_lines, "}", "")
  code <- paste0(code, "\n", paste(wrapper_lines, collapse = "\n"))

  td <- tempfile("tinydsl_cuda_")
  dir.create(td)

  cu_path <- file.path(td, "tinydsl.cu")
  so_path <- file.path(td, paste0("tinydsl", .Platform$dynlib.ext))
  writeLines(code, cu_path, useBytes = TRUE)

  # Get R include paths
  r_include <- system2("R", c("CMD", "config", "--cppflags"), stdout = TRUE, stderr = TRUE)
  r_include <- gsub("^-I", "", r_include)  # Extract path from -I flag
  r_include <- trimws(strsplit(r_include, "\\s+")[[1]])
  r_include <- r_include[grepl("^/", r_include) | grepl("^-I", r_include)]
  
  # Get Rcpp include path
  rcpp_include <- system.file("include", package = "Rcpp")
  
  # Get R ldflags
  r_ldflags <- system2("R", c("CMD", "config", "--ldflags"), stdout = TRUE, stderr = TRUE)
  rcpp_ldflags <- Rcpp:::LdFlags()

  cuda_home <- Sys.getenv("CUDA_HOME", Sys.getenv("CUDA_PATH", dirname(dirname(nvcc))))
  cuda_lib64 <- file.path(cuda_home, "lib64")
  cuda_lib <- if (dir.exists(cuda_lib64)) cuda_lib64 else file.path(cuda_home, "lib")

  # Detect GPU compute capability
  detect_gpu_arch <- function() {
    # Try to get from environment variable first
    arch_env <- Sys.getenv("CUDA_ARCH", "")
    if (nzchar(arch_env)) {
      return(paste0("-arch=sm_", gsub("^sm_?", "", arch_env)))
    }
    
    # Try using nvidia-smi to detect GPU
    tryCatch({
      smi_out <- system2("nvidia-smi", "--query-gpu=compute_cap --format=csv,noheader", 
                         stdout = TRUE, stderr = FALSE)
      if (length(smi_out) > 0 && nzchar(smi_out[1])) {
        cap <- gsub("\\.", "", trimws(smi_out[1]))
        return(paste0("-arch=sm_", cap))
      }
    }, error = function(e) NULL)
    
    return("-arch=sm_90")
  }
  
  arch_flag <- detect_gpu_arch()
  if (isTRUE(verbose)) message("Using CUDA architecture: ", arch_flag)

  # Build include flags
  include_flags <- c(
    paste0("-I", rcpp_include),
    if (length(r_include) > 0) paste0("-I", r_include[!grepl("^-I", r_include)]) else character()
  )
  
  # Process linker flags - wrap compiler flags with -Xcompiler and linker flags with -Xlinker
  process_ldflags <- function(flags_str) {
    flags <- trimws(strsplit(flags_str, "\\s+")[[1]])
    flags <- flags[nzchar(flags)]
    
    result <- character()
    i <- 1
    while (i <= length(flags)) {
      flag <- flags[i]
      if (grepl("^-flto", flag) || grepl("^-ffat-lto", flag)) { # Skip LTO flags - they conflict with CUDA compilation
        i <- i + 1
        next
      }
      
      if (grepl("^-Wl,", flag)) {
        # Linker flag: convert -Wl,opt1,opt2 to -Xlinker opt1 -Xlinker opt2
        opts <- strsplit(sub("^-Wl,", "", flag), ",")[[1]]
        for (opt in opts) {
          result <- c(result, "-Xlinker", opt)
        }
      } else if (flag %in% c("-fopenmp")) {
        # Compiler flags (excluding LTO flags)
        result <- c(result, "-Xcompiler", flag)
      } else if (grepl("^-L", flag)) {
        # Library path
        result <- c(result, flag)
      } else if (grepl("^-l", flag)) {
        # Library
        result <- c(result, flag)
      } else if (flag == "-z" && i < length(flags)) {
        # -z relro should be passed to linker
        result <- c(result, "-Xlinker", flag, "-Xlinker", flags[i + 1])
        i <- i + 1
      } else {
        result <- c(result, flag)
      }
      
      i <- i + 1
    }
    result
  }

  all_ldflags <- process_ldflags(paste(r_ldflags, rcpp_ldflags))

  args <- c(
    "-shared",
    "-Xcompiler", "-fPIC",
    "-std=c++17",
    arch_flag,
    include_flags,
    "-o", so_path,
    cu_path,
    all_ldflags,
    if (dir.exists(cuda_lib)) c(paste0("-L", cuda_lib), "-Xlinker", "-rpath", "-Xlinker", cuda_lib) else character(),
    "-lcudart"
  )

  out <- system2(nvcc, args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status") %||% 0
  if (!identical(status, 0L) && !identical(status, 0)) {
    msg <- paste(out, collapse = "\n")
    stop("nvcc failed:\n", msg, call. = FALSE)
  }
  if (isTRUE(verbose) && length(out) > 0) message(paste(out, collapse = "\n"))

  dyn.load(so_path)

  make_r_fun <- function(symbol) {
    f <- function(...) .Call(symbol, ...)
    attr(f, "dll") <- so_path
    f
  }

  if (length(callable) == 0) stop("compile_cuda() had no callable exported functions (only kernels?)", call. = FALSE)
  funs <- setNames(lapply(callable, function(nm) make_r_fun(paste0(wrapper_prefix, nm))), callable)
  if (length(funs) == 1) return(funs[[1]])
  funs
}

td <- new.env(parent = emptyenv())
td$ty <- ty
td$Params <- Params
td$kernel <- kernel
td$fn <- fn
td$render <- render
td$const <- td_const
td$let <- let
td$if_ <- if_
td$for_ <- for_
td$raw_stmt <- raw_stmt
td$raw_expr <- raw_expr
td$cast <- cast
td$len  <- len
td$ret <- ret
td$compile_cpp <- compile_cpp
td$compile_cuda <- compile_cuda

td$block_idx_x <- function() td_expr("blockIdx.x", ty$u32)
td$block_idx_y <- function() td_expr("blockIdx.y", ty$u32)
td$block_idx_z <- function() td_expr("blockIdx.z", ty$u32)


td$block_dim_x <- function() td_expr("blockDim.x", ty$u32)
td$block_dim_y <- function() td_expr("blockDim.y", ty$u32)
td$block_dim_z <- function() td_expr("blockDim.z", ty$u32)

td$thread_idx_x <- function() td_expr("threadIdx.x", ty$u32)
td$thread_idx_y <- function() td_expr("threadIdx.y", ty$u32)
td$thread_idx_z <- function() td_expr("threadIdx.z", ty$u32)
