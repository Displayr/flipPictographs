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

test_that("Wide pictograph", {
    x2 <- structure(c(17, 83, 56, 144, 138, 26, 26, 206, 196, 4, 312), .Dim = 11L, statistic = "Count", .Dimnames = list(
    c("iPad", "iPod", "iPhone", "Nokia mobile phone", "Other mobile phone (not Nokia and not iPhone)",
    "Mac computer - desktop", "Mac computer – laptop", "PC (non-Mac)",
    "Laptop computer (non-Mac)", "None of these", "NET")), name = "Q6", questions = c("Q6",
"SUMMARY"))
    expect_error(suppressWarnings(PictographChart(x2, mode="bar", hide.base.image=T, graphic.width.inch=244/72, graphic.height.inch=583/72,
                                 show.label.data=T, label.data.position="Next to bar")), "Window is too narrow.")
})
