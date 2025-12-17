test_that("render minimal kernel looks reasonable", {
  mul2 <- psl$fn(psl$Params(x = ty$mut_rcpp_vector(ty$i32)), ret = ty$mut_rcpp_vector(ty$i32))(function(x) {
    ptr <- psl$let(ptr, psl$cast(x, ty$mut_ptr(ty$i32)))
    n <- psl$let(n, psl$len(x))
    psl$for_(i, psl$const(0, ty$u32), n, expr = {
      ptr[i] <- ptr[i] * psl$const(2, ty$i32)
    })
    psl$ret(x)
  })

  code <- psl$render(
    list(mul2 = mul2),
    headers = c("Rcpp.h", "cstdint"),
    dialect = "cpp"
  )

  expect_snapshot(code)
})
