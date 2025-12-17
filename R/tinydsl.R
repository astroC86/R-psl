.tinydsl_state <- new.env(parent = emptyenv())
.tinydsl_state$ctx <- NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

psl_type <- function(kind, ...) {
  structure(list(kind = kind, ...), class = "psl_type")
}

cxx_type <- function(type) {
  stopifnot(inherits(type, "psl_type"))
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
ty$i32 <- psl_type("prim", name = "i32", cxx = "int32_t")
ty$u32 <- psl_type("prim", name = "u32", cxx = "uint32_t")
ty$f64 <- psl_type("prim", name = "f64", cxx = "double")
ty$bool <- psl_type("prim", name = "bool", cxx = "bool")

ty$mut_ptr   <- function(elem) psl_type("ptr", mut = "mut", elem = elem)
ty$const_ptr <- function(elem) psl_type("ptr", mut = "const", elem = elem)
ty$mut_rcpp_vector   <- function(elem) psl_type("rcpp_vector", mut = "mut", elem = elem)
ty$const_rcpp_vector <- function(elem) psl_type("rcpp_vector", mut = "const", elem = elem)
ty$mut_rcpp_matrix   <- function(elem) psl_type("rcpp_matrix", mut = "mut", elem = elem)
ty$const_rcpp_matrix <- function(elem) psl_type("rcpp_matrix", mut = "const", elem = elem)

psl_expr <- function(code, type = NULL) {
  structure(list(code = code, type = type), class = "psl_expr")
}

psl_code <- function(x) {
  if (inherits(x, "psl_expr")) return(x$code)
  if (inherits(x, "psl_ptr")) return(x$base)
  if (inherits(x, "psl_rcpp_vector") || inherits(x, "psl_rcpp_matrix")) return(x$name)
  if (is.numeric(x) && length(x) == 1) return(as.character(x))
  if (is.character(x) && length(x) == 1) return(x)
  stop("Unsupported value in codegen: ", paste(class(x), collapse = "/"), call. = FALSE)
}

psl_const <- function(value, type) {
  stopifnot(length(value) == 1)
  if (!inherits(type, "psl_type") || type$kind != "prim") stop("const() requires a primitive type", call. = FALSE)
  if (type$name == "u32")  return(psl_expr(paste0(as.integer(value), "U"), type))
  if (type$name == "i32")  return(psl_expr(as.character(as.integer(value)), type))
  if (type$name == "bool") return(psl_expr(if (isTRUE(value)) "true" else "false", type))
  psl_expr(as.character(value), type)
}

Ops.psl_expr <- function(e1, e2 = NULL) {
  op <- .Generic

  if (missing(e2)) {
    if (op %in% c("+", "-")) {
      return(psl_expr(paste0("(", op, psl_code(e1), ")"), e1$type))
    }
    stop("Unsupported unary op for psl_expr: ", op, call. = FALSE)
  }

  if (op %in% c("+", "-", "*", "/", "%/%", "%%", "<", "<=", ">", ">=", "==", "!=")) {
    left <- psl_code(e1)
    right <- psl_code(e2)
    out_type <- e1$type %||% (if (inherits(e2, "psl_expr")) e2$type else NULL)
    if (op %in% c("<", "<=", ">", ">=", "==", "!=")) out_type <- ty$bool
    cxx_op <- switch(op, "%/%" = "/", "%%" = "%", op)
    return(psl_expr(paste0("(", left, " ", cxx_op, " ", right, ")"), out_type))
  }

  if (op %in% c("&", "|")) {
    left <- psl_code(e1)
    right <- psl_code(e2)
    cxx_op <- if (op == "&") "&&" else "||"
    return(psl_expr(paste0("(", left, " ", cxx_op, " ", right, ")"), ty$bool))
  }

  stop("Unsupported op for psl_expr: ", op, call. = FALSE)
}

psl_ptr <- function(name, type) {
  stopifnot(inherits(type, "psl_type"), type$kind == "ptr")
  structure(list(name = name, base = name, type = type), class = "psl_ptr")
}

psl_ptr_base <- function(base, type) {
  stopifnot(inherits(type, "psl_type"), type$kind == "ptr")
  structure(list(name = NULL, base = base, type = type), class = "psl_ptr")
}

`[.psl_ptr` <- function(x, i) {
  elem <- x$type$elem
  if (is.null(elem)) stop("Cannot index void* pointer", call. = FALSE)
  psl_expr(paste0(x$base, "[", psl_code(i), "]"), elem)
}

`[<-.psl_ptr` <- function(x, i, value) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("Pointer assignment used outside tinydsl render context", call. = FALSE)
  ctx$emit(paste0(x$base, "[", psl_code(i), "] = ", psl_code(value), ";"))
  x
}

psl_rcpp_vector <- function(name, type) {
  stopifnot(inherits(type, "psl_type"), type$kind == "rcpp_vector")
  structure(list(name = name, type = type), class = "psl_rcpp_vector")
}

psl_rcpp_matrix <- function(name, type) {
  stopifnot(inherits(type, "psl_type"), type$kind == "rcpp_matrix")
  structure(list(name = name, type = type), class = "psl_rcpp_matrix")
}

cast <- function(x, target_type) {
  if (!inherits(target_type, "psl_type")) stop("target_type must be a tinydsl type", call. = FALSE)
  if ((inherits(x, "psl_rcpp_vector") || inherits(x, "psl_rcpp_matrix")) && target_type$kind == "ptr") {
    elem <- target_type$elem
    elem_cxx <- if (is.null(elem)) "void" else cxx_type(elem)
    return(psl_ptr_base(paste0("reinterpret_cast<", elem_cxx, "*>(", x$name, ".begin())"), target_type))
  }
  if (inherits(x, "psl_expr") && target_type$kind == "prim") {
    return(psl_expr(paste0("(", cxx_type(target_type), ")(", psl_code(x), ")"), target_type))
  }
  stop("Unsupported cast() combination", call. = FALSE)
}

len <- function(x) {
  if (inherits(x, "psl_rcpp_vector") || inherits(x, "psl_rcpp_matrix")) {
    return(psl_expr(paste0(x$name, ".size()"), ty$u32))
  }
  stop("len() only supports Rcpp vectors/matrices", call. = FALSE)
}

raw_expr <- function(fmt, out_type, ...) {
  args <- list(...)
  out <- fmt
  nms <- names(args)
  if (!is.null(nms) && any(nzchar(nms))) {
    for (nm in nms) {
      out <- gsub(paste0("$", nm), psl_code(args[[nm]]), out, fixed = TRUE)
    }
  }
  for (idx in seq_along(args)) {
    out <- gsub(paste0("$", idx - 1L), psl_code(args[[idx]]), out, fixed = TRUE)
  }
  psl_expr(out, out_type)
}

psl_ctx <- function(dialect = NULL, device = FALSE, func_kind = NULL) {
  ctx <- new.env(parent = emptyenv())
  ctx$lines <- character()
  ctx$indent <- 1L
  ctx$dialect <- dialect
  ctx$device <- isTRUE(device)
  ctx$func_kind <- func_kind
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

psl_require_ctx <- function(feature, dialect = NULL, device = FALSE) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop(feature, " used outside tinydsl render context", call. = FALSE)
  if (!is.null(dialect) && !identical(ctx$dialect, dialect)) stop(feature, " requires dialect='", dialect, "'", call. = FALSE)
  if (isTRUE(device) && !isTRUE(ctx$device)) stop(feature, " requires device code (kernel or device fn)", call. = FALSE)
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
  ctx$emit(paste0(cxx_type(type), " ", name, " = ", psl_code(value), ";"))
  if (type$kind == "prim") return(psl_expr(name, type))
  if (type$kind == "ptr") return(psl_ptr(name, type))
  if (type$kind == "rcpp_vector") return(psl_rcpp_vector(name, type))
  if (type$kind == "rcpp_matrix") return(psl_rcpp_matrix(name, type))
  stop("Unsupported let() type kind: ", type$kind, call. = FALSE)
}

if_ <- function(cond, expr) {
  expr_quoted <- substitute(expr)
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("if_() used outside tinydsl render context", call. = FALSE)
  ctx$emit(paste0("if (", psl_code(cond), ") {"))
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

  if (is.null(step)) step <- psl_const(1, ty$u32)

  ctx$emit(paste0("for (uint32_t ", name, " = ", psl_code(lo), "; ", name, " < ", psl_code(hi), "; ", name, " += ", psl_code(step), ") {"))
  ctx$with_indent({
    env <- new.env(parent = parent.frame())
    env[[name]] <- psl_expr(name, ty$u32)
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
    ctx$emit(paste0("return ", psl_code(value), ";"))
  }
  invisible(NULL)
}

raw_stmt <- function(fmt, ...) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("raw_stmt() used outside tinydsl render context", call. = FALSE)
  args <- list(...)
  out <- fmt
  for (idx in seq_along(args)) {
    out <- gsub(paste0("$", idx - 1L), psl_code(args[[idx]]), out, fixed = TRUE)
  }
  ctx$emit(out)
  invisible(NULL)
}

cuda_launch <- function(kernel, grid, block, ..., kernel_name = NULL, sync = TRUE) {
  ctx <- .tinydsl_state$ctx
  if (is.null(ctx)) stop("cuda_launch() used outside tinydsl render context", call. = FALSE)

  if (!inherits(kernel, "psl_func") || !identical(kernel$kind, "kernel")) {
    stop("cuda_launch() requires a tinydsl kernel() as the first argument", call. = FALSE)
  }

  kernel_sym <- substitute(kernel)
  if (is.null(kernel_name)) {
    if (is.symbol(kernel_sym)) {
      kernel_name <- as.character(kernel_sym)
    } else {
      stop("cuda_launch() requires kernel_name when kernel is not a simple symbol", call. = FALSE)
    }
  }
  if (!is.character(kernel_name) || length(kernel_name) != 1 || !nzchar(kernel_name)) {
    stop("kernel_name must be a non-empty string", call. = FALSE)
  }

  kparams <- kernel$params
  kparam_names <- names(kparams)
  user_args <- list(...)
  user_arg_names <- names(user_args) %||% rep("", length(user_args))

  if (length(user_args) != length(kparam_names)) {
    stop(
      "cuda_launch() requires exactly ", length(kparam_names),
      " kernel argument(s): ", paste(kparam_names, collapse = ", "),
      call. = FALSE
    )
  }

  has_any_names <- any(nzchar(user_arg_names))
  has_all_names <- all(nzchar(user_arg_names))
  if (has_any_names && !has_all_names) {
    stop("cuda_launch() requires either all kernel arguments named or none named", call. = FALSE)
  }

  args_by_param <- vector("list", length(kparam_names))
  names(args_by_param) <- kparam_names
  if (has_all_names) {
    extra <- setdiff(user_arg_names, kparam_names)
    missing <- setdiff(kparam_names, user_arg_names)
    if (length(extra) > 0) stop("cuda_launch() got unknown kernel argument(s): ", paste(extra, collapse = ", "), call. = FALSE)
    if (length(missing) > 0) stop("cuda_launch() missing kernel argument(s): ", paste(missing, collapse = ", "), call. = FALSE)
    for (nm in kparam_names) args_by_param[[nm]] <- user_args[[nm]]
  } else {
    for (i in seq_along(kparam_names)) args_by_param[[kparam_names[[i]]]] <- user_args[[i]]
  }

  allocs <- list()
  call_args <- vector("list", length(kparam_names))
  names(call_args) <- kparam_names

  for (nm in kparam_names) {
    pty <- kparams[[nm]]
    arg <- args_by_param[[nm]]

    if (pty$kind == "prim") {
      if (inherits(arg, "psl_rcpp_vector") || inherits(arg, "psl_rcpp_matrix")) {
        stop("Kernel argument '", nm, "' expects a primitive, got an Rcpp vector/matrix", call. = FALSE)
      }
      call_args[[nm]] <- psl_code(arg)
      next
    }

    if (pty$kind != "ptr") stop("Unsupported kernel param kind in cuda_launch(): ", pty$kind, call. = FALSE)

    if (inherits(arg, "psl_rcpp_vector") || inherits(arg, "psl_rcpp_matrix")) {
      if (identical(pty$mut, "mut") && identical(arg$type$mut, "const")) {
        stop("Kernel argument '", nm, "' expects a mutable pointer but got a const Rcpp container", call. = FALSE)
      }
      if (is.null(pty$elem)) stop("cuda_launch() cannot infer bytes for void* kernel param '", nm, "'", call. = FALSE)
      if (!identical(arg$type$elem$name, pty$elem$name)) {
        stop(
          "Kernel argument '", nm, "' element type mismatch: kernel expects ",
          pty$elem$name, ", but container has ", arg$type$elem$name,
          call. = FALSE
        )
      }

      elem_cxx <- cxx_type(pty$elem)
      needs_copy_back <- identical(pty$mut, "mut") && identical(arg$type$mut, "mut")
      host_ptr_ty <- if (needs_copy_back) paste0(elem_cxx, "*") else paste0("const ", elem_cxx, "*")
      host_ptr_cast <- if (needs_copy_back) elem_cxx else paste0("const ", elem_cxx)

      h_var <- paste0("psl_h_", nm)
      d_var <- paste0("psl_d_", nm)
      bytes_var <- paste0("psl_bytes_", nm)
      n_elems_expr <- paste0(arg$name, ".size()")

      allocs[[nm]] <- list(
        nm = nm,
        container_name = arg$name,
        elem_cxx = elem_cxx,
        host_ptr_ty = host_ptr_ty,
        host_ptr_cast = host_ptr_cast,
        h_var = h_var,
        d_var = d_var,
        bytes_var = bytes_var,
        n_elems_expr = n_elems_expr,
        needs_copy_back = needs_copy_back
      )
      call_args[[nm]] <- d_var
      next
    }

    call_args[[nm]] <- psl_code(arg)
  }

  ctx$emit("{")
  ctx$with_indent({
    ctx$emit("cudaError_t psl_err;")

    for (a in allocs) {
      ctx$emit(paste0(a$host_ptr_ty, " ", a$h_var, " = reinterpret_cast<", a$host_ptr_cast, "*>(", a$container_name, ".begin());"))
      ctx$emit(paste0(a$elem_cxx, "* ", a$d_var, " = nullptr;"))
      ctx$emit(paste0("size_t ", a$bytes_var, " = (size_t)", a$n_elems_expr, " * sizeof(", a$elem_cxx, ");"))
      ctx$emit(paste0("psl_err = cudaMalloc((void**)&", a$d_var, ", ", a$bytes_var, ");"))
      ctx$emit("if (psl_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(psl_err));")
      ctx$emit(paste0("psl_err = cudaMemcpy(", a$d_var, ", ", a$h_var, ", ", a$bytes_var, ", cudaMemcpyHostToDevice);"))
      ctx$emit("if (psl_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(psl_err));")
      ctx$emit("")
    }

    ctx$emit(paste0(kernel_name, "<<<", psl_code(grid), ", ", psl_code(block), ">>>(", paste(unlist(call_args, use.names = FALSE), collapse = ", "), ");"))

    if (isTRUE(sync)) {
      ctx$emit("psl_err = cudaDeviceSynchronize();")
      ctx$emit("if (psl_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(psl_err));")
    }

    for (a in allocs) {
      if (isTRUE(a$needs_copy_back)) {
        ctx$emit(paste0("psl_err = cudaMemcpy(", a$h_var, ", ", a$d_var, ", ", a$bytes_var, ", cudaMemcpyDeviceToHost);"))
        ctx$emit("if (psl_err != cudaSuccess) Rcpp::stop(cudaGetErrorString(psl_err));")
      }
      ctx$emit(paste0("cudaFree(", a$d_var, ");"))
      ctx$emit("")
    }
  })
  ctx$emit("}")
  invisible(NULL)
}

psl_func <- function(kind, params, body, host = FALSE, device = FALSE, force_inline = FALSE, ret = NULL) {
  stopifnot(inherits(params, "psl_params"))
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
    class = "psl_func"
  )
}

Params <- function(...) {
  types <- list(...)
  if (length(types) == 0) stop("Params(...) requires at least one parameter", call. = FALSE)
  if (is.null(names(types)) || any(names(types) == "")) stop("Params(...) requires named arguments", call. = FALSE)
  for (nm in names(types)) if (!inherits(types[[nm]], "psl_type")) stop("Param '", nm, "' must be a tinydsl type", call. = FALSE)
  structure(types, class = "psl_params")
}

kernel <- function(params, body = NULL, raw_attrs = NULL) {
  if (is.null(body)) return(function(body) kernel(params, body = body, raw_attrs = raw_attrs))
  psl_func("kernel", params, body)
}

fn <- function(params, body = NULL, host = FALSE, device = FALSE, force_inline = FALSE, ret = NULL, raw_attrs = NULL) {
  if (is.null(body)) return(function(body) fn(params, body = body, host = host, device = device, force_inline = force_inline, ret = ret, raw_attrs = raw_attrs))
  psl_func("fn", params, body, host = host, device = device, force_inline = force_inline, ret = ret)
}

compile_func_body <- function(func, export_name, dialect = c("cpp", "cuda")) {
  dialect <- match.arg(dialect)
  stopifnot(inherits(func, "psl_func"))
  is_device <- identical(dialect, "cuda") && (identical(func$kind, "kernel") || (identical(func$kind, "fn") && isTRUE(func$device)))
  ctx <- psl_ctx(dialect = dialect, device = is_device, func_kind = func$kind)
  old <- .tinydsl_state$ctx
  .tinydsl_state$ctx <- ctx
  on.exit({ .tinydsl_state$ctx <- old }, add = TRUE)

  params <- func$params
  arg_objs <- list()
  for (nm in names(params)) {
    pty <- params[[nm]]
    if (pty$kind == "prim") {
      arg_objs[[nm]] <- psl_expr(nm, pty)
    } else if (pty$kind == "ptr") {
      arg_objs[[nm]] <- psl_ptr(nm, pty)
    } else if (pty$kind == "rcpp_vector") {
      arg_objs[[nm]] <- psl_rcpp_vector(nm, pty)
    } else if (pty$kind == "rcpp_matrix") {
      arg_objs[[nm]] <- psl_rcpp_matrix(nm, pty)
    } else {
      stop("Unsupported param kind: ", pty$kind, call. = FALSE)
    }
  }

  do.call(func$body, arg_objs)
  ctx$lines
}

func_signature <- function(func, code_name, dialect = c("cpp", "cuda")) {
  dialect <- match.arg(dialect)
  stopifnot(inherits(func, "psl_func"))
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

psl_sync_threads <- function() {
  ctx <- psl_require_ctx("sync_threads()", dialect = "cuda", device = TRUE)
  ctx$emit("__syncthreads();")
  invisible(NULL)
}

psl_full_warp_mask <- function() psl_expr("0xFFFFFFFFu", ty$u32)
psl_warp_size <- function() psl_expr("((uint32_t)warpSize)", ty$u32)
psl_lane_id <- function() psl_expr("((uint32_t)(threadIdx.x % warpSize))", ty$u32)
psl_warp_id <- function() psl_expr("((uint32_t)(threadIdx.x / warpSize))", ty$u32)

psl_shared_array <- function(sym, elem_type, n) {
  ctx <- psl_require_ctx("shared_array()", dialect = "cuda", device = TRUE)
  sym <- substitute(sym)
  if (!is.symbol(sym)) stop("shared_array() first argument must be a symbol", call. = FALSE)
  name <- as.character(sym)
  if (!inherits(elem_type, "psl_type") || elem_type$kind != "prim") stop("shared_array() elem_type must be a primitive tinydsl type", call. = FALSE)
  if (!(is.numeric(n) && length(n) == 1 && is.finite(n) && n > 0 && n == as.integer(n))) {
    stop("shared_array() requires a positive integer 'n' known at compile time", call. = FALSE)
  }
  ctx$emit(paste0("__shared__ ", cxx_type(elem_type), " ", name, "[", as.integer(n), "];"))
  psl_ptr_base(name, ty$mut_ptr(elem_type))
}

psl_sync_warp <- function(mask = NULL) {
  ctx <- psl_require_ctx("sync_warp()", dialect = "cuda", device = TRUE)
  if (is.null(mask)) mask <- psl$full_warp_mask()
  ctx$emit(paste0("__syncwarp(", psl_code(mask), ");"))
  invisible(NULL)
}

psl_ballot_sync <- function(mask, pred) {
  psl_require_ctx("ballot_sync()", dialect = "cuda", device = TRUE)
  psl_expr(paste0("__ballot_sync(", psl_code(mask), ", ", psl_code(pred), ")"), ty$u32)
}

psl_shfl_down_sync <- function(mask, var, delta, width = NULL) {
  psl_require_ctx("shfl_down_sync()", dialect = "cuda", device = TRUE)
  if (!inherits(var, "psl_expr") || is.null(var$type) || !inherits(var$type, "psl_type") || var$type$kind != "prim") {
    stop("shfl_down_sync() requires a typed primitive psl_expr for var", call. = FALSE)
  }
  if (!(var$type$name %in% c("i32", "u32"))) stop("shfl_down_sync() only supports i32/u32 currently", call. = FALSE)
  args <- c(psl_code(mask), psl_code(var), psl_code(delta))
  if (!is.null(width)) args <- c(args, psl_code(width))
  psl_expr(paste0("__shfl_down_sync(", paste(args, collapse = ", "), ")"), var$type)
}

psl_shfl_up_sync <- function(mask, var, delta, width = NULL) {
  psl_require_ctx("shfl_up_sync()", dialect = "cuda", device = TRUE)
  if (!inherits(var, "psl_expr") || is.null(var$type) || !inherits(var$type, "psl_type") || var$type$kind != "prim") {
    stop("shfl_up_sync() requires a typed primitive psl_expr for var", call. = FALSE)
  }
  if (!(var$type$name %in% c("i32", "u32"))) stop("shfl_up_sync() only supports i32/u32 currently", call. = FALSE)
  args <- c(psl_code(mask), psl_code(var), psl_code(delta))
  if (!is.null(width)) args <- c(args, psl_code(width))
  psl_expr(paste0("__shfl_up_sync(", paste(args, collapse = ", "), ")"), var$type)
}

psl_shfl_xor_sync <- function(mask, var, lane_mask, width = NULL) {
  psl_require_ctx("shfl_xor_sync()", dialect = "cuda", device = TRUE)
  if (!inherits(var, "psl_expr") || is.null(var$type) || !inherits(var$type, "psl_type") || var$type$kind != "prim") {
    stop("shfl_xor_sync() requires a typed primitive psl_expr for var", call. = FALSE)
  }
  if (!(var$type$name %in% c("i32", "u32"))) stop("shfl_xor_sync() only supports i32/u32 currently", call. = FALSE)
  args <- c(psl_code(mask), psl_code(var), psl_code(lane_mask))
  if (!is.null(width)) args <- c(args, psl_code(width))
  psl_expr(paste0("__shfl_xor_sync(", paste(args, collapse = ", "), ")"), var$type)
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
  if (inherits(entry_points, "psl_func")) {
    entry_points <- list(entry_points)
  }
  if (!is.list(entry_points) || length(entry_points) == 0) stop("entry_points must be a non-empty list", call. = FALSE)

  export_names <- names(entry_points)
  if (is.null(export_names)) export_names <- rep(NA_character_, length(entry_points))

  entries <- vector("list", length(entry_points))
  for (i in seq_along(entry_points)) {
    func <- entry_points[[i]]
    name <- export_names[[i]]
    if (!inherits(func, "psl_func")) stop("All entry points must be tinydsl functions", call. = FALSE)
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
    body_lines <- compile_func_body(e$func, e$export_name, dialect = dialect)
    out <- c(out, body_lines, "}", "")
  }

  if (length(internal) > 0) {
    out <- c(out, "namespace internal {", "")
    for (e in internal) {
      sig <- func_signature(e$func, e$code_name, dialect = dialect)
      out <- c(out, render_one_decl(sig, with_semicolon = FALSE))
      body_lines <- compile_func_body(e$func, e$export_name, dialect = dialect)
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
  stopifnot(inherits(type, "psl_type"))
  if (type$kind != "prim") stop("cpp_type_for only supports primitive types", call. = FALSE)
  if (type$name == "i32") return("int32_t")
  if (type$name == "u32") return("uint32_t")
  if (type$name == "f64") return("double")
  if (type$name == "bool") return("bool")
  stop("Unsupported primitive type: ", type$name, call. = FALSE)
}

rcpp_sexp_to_cpp <- function(sexp_name, param_name, type) {
  stopifnot(inherits(type, "psl_type"))
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
  stopifnot(inherits(ret_type, "psl_type"))
  return(c(paste0("auto res = ", value_expr, ";"), "return Rcpp::wrap(res);"))
}


compile_cuda <- function(entry_points, headers = c("Rcpp.h", "cuda_runtime.h", "cstdint"), wrapper_prefix = "psl_call_", verbose = FALSE) {
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
    if (!inherits(func, "psl_func")) stop("All entry points must be tinydsl functions", call. = FALSE)
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

  psl <- tempfile("tinydsl_cuda_")
  dir.create(psl)

  cu_path <- file.path(psl, "tinydsl.cu")
  so_path <- file.path(psl, paste0("tinydsl", .Platform$dynlib.ext))
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

psl <- new.env(parent = emptyenv())
psl$ty <- ty
psl$Params <- Params
psl$kernel <- kernel
psl$fn <- fn
psl$ret <- ret
psl$render <- render
psl$let <- let
psl$const <- psl_const
psl$cast <- cast
psl$shared_array <- psl_shared_array
psl$if_ <- if_
psl$for_ <- for_
psl$len  <- len
psl$raw_stmt <- raw_stmt
psl$raw_expr <- raw_expr
psl$cuda_launch <- cuda_launch
psl$compile_cpp <- compile_cpp
psl$compile_cuda <- compile_cuda

psl$sync_threads <- psl_sync_threads
psl$full_warp_mask <- psl_full_warp_mask
psl$warp_size <- psl_warp_size
psl$lane_id <- psl_lane_id
psl$warp_id <- psl_warp_id
psl$sync_warp <- psl_sync_warp
psl$ballot_sync <- psl_ballot_sync
psl$shfl_down_sync  <- psl_shfl_down_sync 
psl$shfl_up_sync <- psl_shfl_up_sync
psl$shfl_xor_sync  <- psl_shfl_xor_sync 

psl$block_idx_x <- function() psl_expr("blockIdx.x", ty$u32)
psl$block_idx_y <- function() psl_expr("blockIdx.y", ty$u32)
psl$block_idx_z <- function() psl_expr("blockIdx.z", ty$u32)

psl$block_dim_x <- function() psl_expr("blockDim.x", ty$u32)
psl$block_dim_y <- function() psl_expr("blockDim.y", ty$u32)
psl$block_dim_z <- function() psl_expr("blockDim.z", ty$u32)

psl$thread_idx_x <- function() psl_expr("threadIdx.x", ty$u32)
psl$thread_idx_y <- function() psl_expr("threadIdx.y", ty$u32)
psl$thread_idx_z <- function() psl_expr("threadIdx.z", ty$u32)

psl$grid_dim_x <- function() psl_expr("gridDim.x", ty$u32)
psl$grid_dim_y <- function() psl_expr("gridDim.y", ty$u32)
psl$grid_dim_z <- function() psl_expr("gridDim.z", ty$u32)

psl$global_idx_x    <- function() psl_expr("((blockIdx.x * blockDim.x) + threadIdx.x)", ty$u32)
psl$global_stride_x <- function() psl_expr("(blockDim.x * gridDim.x)", ty$u32)
