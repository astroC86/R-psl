test_that("compile_cpp compiles and runs", {
  testthat::skip_if_not_installed("Rcpp")
  has_compiler <- nzchar(Sys.which("g++")) || nzchar(Sys.which("clang++"))
  testthat::skip_if_not(has_compiler, "No C++ compiler available")

  mul2 <- td$fn(td$Params(x = ty$rcpp_vector_mut(ty$i32)), ret = ty$rcpp_vector_mut(ty$i32))(function(x) {
    ptr <- td$let(ptr, td$cast(x, ty$ptr_mut(ty$i32)))
    n <- td$let(n, td$len(x))
    td$for_(i, td$const(0, ty$u32), n, expr = {
      ptr[i] <- ptr[i] * td$const(2, ty$i32)
    })
    td$ret(x)
  })

  f <- td$compile_cpp(list(mul2 = mul2))
  expect_equal(f(1:5), c(2L, 4L, 6L, 8L, 10L))
})

