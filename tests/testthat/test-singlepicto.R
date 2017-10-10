context("SinglePicto")

test_that("SinglePicto runs (simple example)", {
    expect_warning( SinglePicto(2.5, 5))
})

test_that("SinglePicto accepts width parameter", {
    expect_warning( SinglePicto(2.5, 5, icon.width=10))
})


test_that("SinglePicto accepts direction parameter", {
    expect_warning( SinglePicto(4.5, 6, number.rows=2, fill.direction="fromright"))
})

test_that("SinglePicto accepts image parameter", {
    expect_warning( SinglePicto(2.5, 5, image="stickman"))
})


test_that("SinglePicto accepts background parameter", {
    expect_warning( SinglePicto(2.5, 5, 5, image="stickman", background.color="red") )
})

test_that("SinglePicto accepts autosize parameter", {
    expect_warning( SinglePicto(2.5, 5, 3, image="stickman", background.color="red", auto.size = T) )
})
