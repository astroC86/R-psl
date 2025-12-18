test_that("CUDA static shared memory reduction renders", {
  reduce_kernel <- psl$kernel(psl$Params(n = ty$u32, x = ty$const_ptr(ty$i32), out = ty$mut_ptr(ty$i32)))(function(n, x, out) {
    s <- psl$shared_array(s, ty$i32, 256L)
    bid <- psl$let(bid, psl$block_idx_x())
    tid  <- psl$let(tid, psl$thread_idx_x())
    i  <- psl$let(i, bid * (psl$const(2, ty$u32) * psl$block_dim_x()) + tid)
    i2 <- psl$let(i2, i + psl$block_dim_x())
    x1 <- psl$raw_expr("($0 ? $1 : $2)", ty$i32, i  < n, x[i] , psl$const(0, ty$i32))
    x2 <- psl$raw_expr("($0 ? $1 : $2)", ty$i32, i2 < n, x[i2], psl$const(0, ty$i32))
    s[tid] <- x1 + x2
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

  reduction_cuda <- psl$fn(
    psl$Params(x = ty$mut_rcpp_vector(ty$i32), scratch = ty$mut_rcpp_vector(ty$i32)),
    host = TRUE,
    ret = ty$mut_rcpp_vector(ty$i32)
  )(function(x, scratch) {
    n     <- psl$let(n, psl$len(x))
    grid1 <- psl$let(grid1, (n + psl$const(511, ty$u32)) %/% psl$const(512, ty$u32))
    block <- psl$let(block, psl$const(256, ty$u32))
    grid2 <- psl$let(grid2, psl$const(1, ty$u32))
    psl$cuda_launch(reduce_kernel, grid1, block, n = n, x = x, out = scratch)
    psl$cuda_launch(reduce_kernel, grid2, block, n = grid1, x = scratch, out = scratch)
    psl$ret(scratch)
  })

  code <- psl$render(
    list(reduction_cuda = reduction_cuda, reduce_kernel = reduce_kernel),
    headers = c("Rcpp.h", "cuda_runtime.h", "cstdint"),
    dialect = "cuda"
  )
  f <- psl$compile_cuda(list(reduction_cuda = reduction_cuda, reduce_kernel = reduce_kernel))
  expect_snapshot(code)
})
