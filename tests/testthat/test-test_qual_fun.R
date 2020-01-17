test_that("qual_fun works", {
  res <- nd.tidytuesday::qual_fun(iris)
  
  expect_true( prod(dim(res)) == 15 )  
  expect_true( prod(res) == 0 )
})
