context("StarBarChart")

test_that("StarBarChart stops when it should", {
    expect_error( StarBarChart(c(2.5,6,1), 5) )
})


test_that("StarBarChart works when it should", {
    expect_error( StarBarChart(c(2.5,4,3,1), 5), NA )
})
