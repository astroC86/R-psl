test_that("CUDA strided loops render", {
  kernel <- psl$kernel(psl$Params(n = ty$u32, x = ty$mut_ptr(ty$i32)))(function(n, x) {
    psl$for_(i, lo = psl$global_idx_x(), hi = n, step = psl$global_stride_x(), expr = {
      x[i] <- x[i] * psl$const(2, ty$i32)
    })
  })

  code <- psl$render(list(kernel = kernel), headers = c("cuda_runtime.h", "cstdint"), dialect = "cuda")
  expect_snapshot(code)
})
