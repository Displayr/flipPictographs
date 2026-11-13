context("getWidthHeightRatio")

test_that("getWidthHeightRatio", {
    server <- localImageServer()
    expect_equal(getWidthHeightRatio(server$url("/Dizzy_drunk_color.png")), 0.529)
})

test_that("getImage", {
    server <- localImageServer()
    expect_error(getImage(server$url("/Dizzy_drunk_color.png")), NA)
    expect_error(getImage(server$url("/page.html")),
                 "The url content type is 'text/html'")
    # A url without a scheme.
    expect_error(getImage(sub("^http://", "", server$url("/page.html"))),
                 "The url content type is 'text/html'")
})

test_that("checkImageUrl", {
    server <- localImageServer()
    expect_error(checkImageUrl(server$url("/Dizzy_drunk_color.png")), NA)
    expect_error(checkImageUrl(server$url("/page.html")), "The url content type is 'text/html'")
})
