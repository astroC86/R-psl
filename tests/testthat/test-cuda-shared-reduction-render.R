test_that("CUDA static shared memory reduction renders", {
  reduce_kernel <- td$kernel(td$Params(n = ty$u32, x = ty$ptr_const(ty$i32), out = ty$ptr_mut(ty$i32)))(function(n, x, out) {
    s    <- td$let(s, td$shared_array(s, ty$i32, 256L))
    gtid <- td$let(gtid, td$global_idx_x())
    tid  <- td$let(tid, td$thread_idx_x())
    bid  <- td$let(bid, td$block_idx_x())

    val <- td$let(val, td$const(0, ty$i32))
    td$if_(gtid < n, {
      val <- x[tid]
    })
    s[tid] <- val
    td$sync_threads()

    offsets <- c(128L, 64L, 32L, 16L, 8L, 4L, 2L, 1L)
    for (off in offsets) {
      off_td <- td$const(off, ty$u32)
      td$if_(tid < off_td, {
        s[tid] <- s[tid] + s[tid + off_td]
      })
      td$sync_threads()
    }

    td$if_(tid == td$const(0, ty$u32), {
      out[bid] <- s[td$const(0, ty$u32)]
    })
  })
  code <- td$render(list(reduce_kernel = reduce_kernel), headers = c("cuda_runtime.h", "cstdint"), dialect = "cuda")
  expect_snapshot(code)
})

