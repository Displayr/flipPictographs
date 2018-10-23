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

test_that("SinglePicto custom icon", {
    expect_error( SinglePicto(48,
                              total.icons = 1,
                              maximum.value = 100,
                              width.height.ratio = 1.125,
                              image = "http://docs.displayr.com/images/5/51/Female_Blue.svg",
                              base.image = "http://docs.displayr.com/images/7/70/Female_Grey.svg",
                              is.custom.url = TRUE) , NA)
})

#test_that("SinglePicto redirected custom icon", {
#    expect_error( SinglePicto(48,
#                              total.icons = 1,
#                              maximum.value = 100,
#                              width.height.ratio = 1.125,
#                              image = "http://docs.displayr.com/images/5/51/Female_Blue.svg",
#                              base.image = "http://docs.displayr.com/images/7/70/Female_Grey.svg",
#                              is.custom.url = TRUE) , "Image type is text/html. Ensure the image url is #correct and not redirected.")
#})
