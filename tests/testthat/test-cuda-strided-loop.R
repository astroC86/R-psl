test_that("CUDA strided loops render", {
  kernel <- td$kernel(td$Params(n = ty$u32, x = ty$ptr_mut(ty$i32)))(function(n, x) {
    td$for_(i, lo = td$global_idx_x(), hi = n, step = td$global_stride_x(), expr = {
      x[i] <- x[i] * td$const(2, ty$i32)
    })
  })

  code <- td$render(list(kernel = kernel), headers = c("cuda_runtime.h", "cstdint"), dialect = "cuda")
  expect_snapshot(code)
})
