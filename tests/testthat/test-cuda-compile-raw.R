test_that("compile_cuda builds a loadable shared library", {
  testthat::skip_if_not_installed("Rcpp")
  nvcc <- Sys.which("nvcc")
  testthat::skip_if_not(nzchar(nvcc), "nvcc not available")

  mul2_kernel <- psl$kernel(psl$Params(n = ty$u32, x = ty$mut_ptr(ty$i32)))(function(n, x) {
    i <- psl$let(i, psl$block_idx_x() * psl$block_dim_x() + psl$thread_idx_x())
    psl$if_(i < n, {
      x[i] <- x[i] * psl$const(2, ty$i32)
    })
  })

  mul2_cuda <- psl$fn(
    psl$Params(x = ty$mut_rcpp_vector(ty$i32)),
    host = TRUE,
    ret = ty$mut_rcpp_vector(ty$i32)
  )(function(x) {
    n <- psl$let(n, psl$len(x))
    psl$raw_stmt("int32_t* h_x = reinterpret_cast<int32_t*>(x.begin());")
    psl$raw_stmt("int32_t* d_x = nullptr;")
    psl$raw_stmt("cudaError_t err;")
    psl$raw_stmt("err = cudaMalloc((void**)&d_x, (size_t)n * sizeof(int32_t));")
    psl$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")
    psl$raw_stmt("err = cudaMemcpy(d_x, h_x, (size_t)n * sizeof(int32_t), cudaMemcpyHostToDevice);")
    psl$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")

    grid <- psl$let(grid, (n + psl$const(255, ty$u32)) %/% psl$const(256, ty$u32))
    block <- psl$let(block, psl$const(256, ty$u32))
    psl$raw_stmt("mul2_kernel<<<$0, $1>>>($2, d_x);", grid, block, n)
    psl$raw_stmt("err = cudaDeviceSynchronize();")
    psl$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")

    psl$raw_stmt("err = cudaMemcpy(h_x, d_x, (size_t)n * sizeof(int32_t), cudaMemcpyDeviceToHost);")
    psl$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")
    psl$raw_stmt("cudaFree(d_x);")
    psl$ret(x)
  })

  f <- psl$compile_cuda(list(mul2_cuda = mul2_cuda, mul2_kernel = mul2_kernel))
  expect_true(is.function(f))
})
