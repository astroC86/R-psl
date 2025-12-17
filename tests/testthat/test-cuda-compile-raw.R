test_that("compile_cuda builds a loadable shared library", {
  testthat::skip_if_not_installed("Rcpp")
  nvcc <- Sys.which("nvcc")
  testthat::skip_if_not(nzchar(nvcc), "nvcc not available")

  mul2_kernel <- td$kernel(td$Params(n = ty$u32, x = ty$ptr_mut(ty$i32)))(function(n, x) {
    i <- td$let(i, td$block_idx_x() * td$block_dim_x() + td$thread_idx_x())
    td$if_(i < n, {
      x[i] <- x[i] * td$const(2, ty$i32)
    })
  })

  mul2_cuda <- td$fn(
    td$Params(x = ty$rcpp_vector_mut(ty$i32)),
    host = TRUE,
    ret = ty$rcpp_vector_mut(ty$i32)
  )(function(x) {
    n <- td$let(n, td$len(x))
    td$raw_stmt("int32_t* h_x = reinterpret_cast<int32_t*>(x.begin());")
    td$raw_stmt("int32_t* d_x = nullptr;")
    td$raw_stmt("cudaError_t err;")
    td$raw_stmt("err = cudaMalloc((void**)&d_x, (size_t)n * sizeof(int32_t));")
    td$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")
    td$raw_stmt("err = cudaMemcpy(d_x, h_x, (size_t)n * sizeof(int32_t), cudaMemcpyHostToDevice);")
    td$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")

    grid <- td$let(grid, (n + td$const(255, ty$u32)) %/% td$const(256, ty$u32))
    block <- td$let(block, td$const(256, ty$u32))
    td$raw_stmt("mul2_kernel<<<$0, $1>>>($2, d_x);", grid, block, n)
    td$raw_stmt("err = cudaDeviceSynchronize();")
    td$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")

    td$raw_stmt("err = cudaMemcpy(h_x, d_x, (size_t)n * sizeof(int32_t), cudaMemcpyDeviceToHost);")
    td$raw_stmt("if (err != cudaSuccess) Rcpp::stop(cudaGetErrorString(err));")
    td$raw_stmt("cudaFree(d_x);")
    td$ret(x)
  })

  f <- td$compile_cuda(list(mul2_cuda = mul2_cuda, mul2_kernel = mul2_kernel))
  expect_true(is.function(f))
})
