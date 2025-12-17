test_that("render minimal kernel looks reasonable", {
  mul2 <- td$fn(td$Params(x = ty$rcpp_vector_mut(ty$i32)), ret = ty$rcpp_vector_mut(ty$i32))(function(x) {
    ptr <- td$let(ptr, td$cast(x, ty$ptr_mut(ty$i32)))
    n <- td$let(n, td$len(x))
    td$for_(i, td$const(0, ty$u32), n, expr = {
      ptr[i] <- ptr[i] * td$const(2, ty$i32)
    })
    td$ret(x)
  })

  code <- td$render(
    list(mul2 = mul2),
    headers = c("Rcpp.h", "cstdint"),
    dialect = "cpp"
  )

  expect_snapshot(code)
})
