context("getWidthHeightRatio")

test_that("Check content header",
{
    expect_error(getWidthHeightRatio("https://wiki.q-researchsoftware.com/images/a/ab/Dizzy_drunk_color.png", NA))
    expect_error(getWidthHeightRatio("https://wiki.q-researchsoftware.com/wiki/File:Dizzy_drunk_color.png"),
                 "The url content type is 'text/html'")
    expect_error(getWidthHeightRatio("google.com"), "The url content type is 'text/html'")
    expect_error(getWidthHeightRatio("www.google.com"))



})
