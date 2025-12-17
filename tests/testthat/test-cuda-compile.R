test_that("compile_cuda builds a loadable shared library", {
  testthat::skip_if_not_installed("Rcpp")
  nvcc <- Sys.which("nvcc")
  testthat::skip_if_not(nzchar(nvcc), "nvcc not available")

  mul2_kernel <- td$kernel(td$Params(n = ty$u32, x = ty$ptr_mut(ty$i32)))(function(n, x) {
    td$for_(i, lo = td$global_idx_x(), hi = n, step = td$global_stride_x(), expr = {
      x[i] <- x[i] * td$const(2, ty$i32)
    })
  })

  mul2_cuda <- td$fn(
    td$Params(x = ty$rcpp_vector_mut(ty$i32)),
    host = TRUE,
    ret = ty$rcpp_vector_mut(ty$i32)
  )(function(x) {
    n <- td$let(n, td$len(x))

    grid <- td$let(grid, (n + td$const(255, ty$u32)) %/% td$const(256, ty$u32))
    block <- td$let(block, td$const(256, ty$u32))
    td$cuda_launch(mul2_kernel, grid, block, n = n, x = x)
    td$ret(x)
  })

  f <- td$compile_cuda(list(mul2_cuda = mul2_cuda, mul2_kernel = mul2_kernel))
  expect_true(is.function(f))
})
