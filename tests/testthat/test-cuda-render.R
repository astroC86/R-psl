test_that("render CUDA kernel and launcher looks reasonable", {
  mul2_kernel <- psl$kernel(psl$Params(n = ty$u32, x = ty$mut_ptr(ty$i32)))(function(n, x) {
    i <- psl$let(i, psl$block_idx_x() * psl$block_dim_x() + psl$thread_idx_x())
    psl$if_(i < n, {
      x[i] <- x[i] * psl$const(2, ty$i32)
    })
  })

  mul2 <- psl$fn(psl$Params(n = ty$u32, x = ty$mut_ptr(ty$i32)), host = TRUE)(function(n, x) {
    grid <- psl$let(grid, (n + psl$const(255, ty$u32)) %/% psl$const(256, ty$u32))
    block <- psl$let(block, psl$const(256, ty$u32))
    psl$raw_stmt("mul2_kernel<<<$0, $1>>>($2, $3);", grid, block, n, x)
  })

  code <- psl$render(
    list(mul2 = mul2, mul2_kernel = mul2_kernel),
    headers = c("cuda_runtime.h", "cstdint"),
    dialect = "cuda"
  )

  expect_snapshot(code)
})

test_that("render CUDA kernel auto wrapper looks reasonable", {
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
    grid <- psl$let(grid, (n + psl$const(255, ty$u32)) %/% psl$const(256, ty$u32))
    block <- psl$let(block, psl$const(256, ty$u32))
    psl$cuda_launch(mul2_kernel, grid, block, n = n, x = x)
    psl$ret(x)
  })

  code <- psl$render(
    list(mul2_cuda = mul2_cuda, mul2_kernel = mul2_kernel),
    headers = c("Rcpp.h", "cuda_runtime.h", "cstdint"),
    dialect = "cuda"
  )

  expect_snapshot(code)
})
