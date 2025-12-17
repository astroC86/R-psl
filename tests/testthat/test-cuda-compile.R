test_that("compile_cuda builds a loadable shared library", {
  testthat::skip_if_not_installed("Rcpp")
  nvcc <- Sys.which("nvcc")
  testthat::skip_if_not(nzchar(nvcc), "nvcc not available")

  mul2_kernel <- psl$kernel(psl$Params(n = ty$u32, x = ty$mut_ptr(ty$i32)))(function(n, x) {
    psl$for_(i, lo = psl$global_idx_x(), hi = n, step = psl$global_stride_x(), expr = {
      x[i] <- x[i] * psl$const(2, ty$i32)
    })
  })

  mul2_cuda <- psl$fn(
    psl$Params(x = ty$mut_rcpp_vector(ty$i32)),
    host = TRUE,
    ret = ty$mut_rcpp_vector(ty$i32)
  )(function(x) {
    n <- psl$let(n, psl$len(x))

    grid <- psl$let(grid, (n + psl$const(255, ty$u32)) %/% psl$const(256, ty$u32))
    block <- psl$let(block, psl$const(256, ty$u32))
    psl$cuda_launch(mul2_kernel, grid, block, n = n, x = x)
    psl$ret(x)
  })

  f <- psl$compile_cuda(list(mul2_cuda = mul2_cuda, mul2_kernel = mul2_kernel))
  expect_true(is.function(f))
})
