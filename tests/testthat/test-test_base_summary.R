test_that("base_summary works", {
        res <- nd.tidytuesday::base_summary(mtcars)
        testthat::expect_true( prod( dim(res) == c(11, 9)) == 1 )
})
# as.logical(1)
