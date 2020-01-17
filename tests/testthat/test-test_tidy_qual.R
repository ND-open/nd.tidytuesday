test_that("multiplication works", {
        # example(tidy_qual, "nd.tidytuesday")
        res <- nd.tidytuesday::tidy_qual(iris)
        expect_true(prod(res[,-1]) == 0)
        expect_true(prod(dim(res)) == 18)
})
