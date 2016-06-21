context("StarChart")

test_that("StarChart stops when it should", {
    expect_error( StarChart(c(2.5,6,1), 5) )
})


test_that("StarChart works when it should", {
    expect_error( StarChart(c(2.5,4,3,1), 5), NA )
})
