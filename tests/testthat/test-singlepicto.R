context("SinglePicto")

test_that("SinglePicto runs (simple example)", {
    expect_error( SinglePicto(2.5, 5), NA)
})

test_that("SinglePicto accepts width parameter", {
    expect_error( SinglePicto(2.5, 5, icon.width=10), NA)
})


test_that("SinglePicto accepts direction parameter", {
    expect_error( SinglePicto(4.5, 6, number.rows=2, fill.direction="fromright"), NA)
})

test_that("SinglePicto accepts image parameter", {
    expect_error( SinglePicto(2.5, 5, image="stickman"), NA)
})


test_that("SinglePicto accepts background parameter", {
    expect_error( SinglePicto(2.5, 5, 5, image="stickman", background.color="red") , NA)
})

test_that("SinglePicto accepts autosize parameter", {
    expect_error( SinglePicto(2.5, 5, 3, image="stickman", background.color="red", auto.size = T) , NA)
})
