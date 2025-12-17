test_that("CUDA static shared memory reduction renders", {
  reduce_kernel <- psl$kernel(psl$Params(n = ty$u32, x = ty$const_ptr(ty$i32), out = ty$mut_ptr(ty$i32)))(function(n, x, out) {
    s    <- psl$let(s, psl$shared_array(s, ty$i32, 256L))
    gtid <- psl$let(gtid, psl$global_idx_x())
    tid  <- psl$let(tid, psl$thread_idx_x())
    bid  <- psl$let(bid, psl$block_idx_x())

    val <- psl$let(val, psl$const(0, ty$i32))
    psl$if_(gtid < n, {
      val <- x[tid]
    })
    s[tid] <- val
    psl$sync_threads()

    offsets <- c(128L, 64L, 32L, 16L, 8L, 4L, 2L, 1L)
    for (off in offsets) {
      off_td <- psl$const(off, ty$u32)
      psl$if_(tid < off_td, {
        s[tid] <- s[tid] + s[tid + off_td]
      })
      psl$sync_threads()
    }

    psl$if_(tid == psl$const(0, ty$u32), {
      out[bid] <- s[psl$const(0, ty$u32)]
    })
  })
  code <- psl$render(list(reduce_kernel = reduce_kernel), headers = c("cuda_runtime.h", "cstdint"), dialect = "cuda")
  expect_snapshot(code)
})

