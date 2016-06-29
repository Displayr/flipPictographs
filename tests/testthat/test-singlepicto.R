context("SinglePicto")

test_that("StarChart stops when it should", {
    expect_error( StarChart(c(2.5,6,1), 5) )
})

test_that("SinglePicto runs (simple example)", {
    expect_error( SinglePicto(2.5, 1, 5), NA )
})

test_that("SinglePicto accepts width parameter", {
    expect_error( SinglePicto(2.5, 1, 5, width=10), NA )
})


test_that("SinglePicto accepts direction parameter", {
    expect_error( SinglePicto(4.5, 2, 3, direction="vertical"), NA )
})

test_that("SinglePicto accepts image parameter", {
    expect_error( SinglePicto(2.5, 5, 1, image="people"), NA )
})


test_that("SinglePicto accepts background parameter", {
    expect_error( SinglePicto(2.5, 5, 1, image="people", bg.color="red"), NA )
})

test_that("SinglePicto accepts autosize parameter", {
    expect_error( SinglePicto(2.5, 2, 3, image="people", bg.color="red", auto.size = T), NA )
})
