context("PictographChart")

x1 <- c(A=1, Bbbbbb=3.6, Cc=2.8)
test_that("Simple barchart", {
    expect_error( PictographChart(x1, show.legend=T), NA)
})
test_that("Scaling works", {
    expect_error( PictographChart(x1*100, show.legend=T), NA)
})
test_that("Simple column chart", {
    expect_error( PictographChart(x1, mode="column"), NA)
})
