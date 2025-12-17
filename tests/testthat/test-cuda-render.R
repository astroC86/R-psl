test_that("render CUDA kernel and launcher looks reasonable", {
  mul2_kernel <- td$kernel(td$Params(n = ty$u32, x = ty$ptr_mut(ty$i32)))(function(n, x) {
    i <- td$let(i, td$block_idx_x() * td$block_dim_x() + td$thread_idx_x())
    td$if_(i < n, {
      x[i] <- x[i] * td$const(2, ty$i32)
    })
  })

  mul2 <- td$fn(td$Params(n = ty$u32, x = ty$ptr_mut(ty$i32)), host = TRUE)(function(n, x) {
    grid <- td$let(grid, (n + td$const(255, ty$u32)) %/% td$const(256, ty$u32))
    block <- td$let(block, td$const(256, ty$u32))
    td$raw_stmt("mul2_kernel<<<$0, $1>>>($2, $3);", grid, block, n, x)
  })

  code <- td$render(
    list(mul2 = mul2, mul2_kernel = mul2_kernel),
    headers = c("cuda_runtime.h", "cstdint"),
    dialect = "cuda"
  )

  expect_snapshot(code)
})

