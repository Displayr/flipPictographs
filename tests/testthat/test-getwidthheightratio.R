context("getWidthHeightRatio")

test_that("getWidthHeightRatio", {
    expect_equal(getWidthHeightRatio("https://wiki.q-researchsoftware.com/images/a/ab/Dizzy_drunk_color.png"), 0.529)
})

test_that("getImage", {
    expect_error(getImage("https://wiki.q-researchsoftware.com/images/a/ab/Dizzy_drunk_color.png"), NA)
    expect_error(getImage("https://wiki.q-researchsoftware.com/wiki/File:Dizzy_drunk_color.png"),
                 "The url content type is 'text/html'")
    expect_error(getImage("google.com"), "The url content type is 'text/html'")
    expect_error(getImage("www.google.com"), "The url content type is 'text/html'")
})

test_that("checkImageUrl", {
    expect_error(checkImageUrl("https://wiki.q-researchsoftware.com/images/a/ab/Dizzy_drunk_color.png"), NA)
    expect_error(checkImageUrl("www.google.com"), "The url content type is 'text/html'")
})
