test_that("compile_cpp compiles and runs", {
  testthat::skip_if_not_installed("Rcpp")
  has_compiler <- nzchar(Sys.which("g++")) || nzchar(Sys.which("clang++"))
  testthat::skip_if_not(has_compiler, "No C++ compiler available")

  mul2 <- psl$fn(psl$Params(x = ty$mut_rcpp_vector(ty$i32)), ret = ty$mut_rcpp_vector(ty$i32))(function(x) {
    ptr <- psl$let(ptr, psl$cast(x, ty$mut_ptr(ty$i32)))
    n <- psl$let(n, psl$len(x))
    psl$for_(i, psl$const(0, ty$u32), n, expr = {
      ptr[i] <- ptr[i] * psl$const(2, ty$i32)
    })
    psl$ret(x)
  })

  f <- psl$compile_cpp(list(mul2 = mul2))
  expect_equal(f(1:5), c(2L, 4L, 6L, 8L, 10L))
})

