# Serves the images in tests/testthat/images over HTTP locally, so the tests that fetch image URLs don't need external
# network access. The server stops when the calling test finishes.
localImageServer <- function(.local_envir = parent.frame())
{
    app <- webfakes::new_app()
    app$get("/page.html", function(req, res) res$set_type("text/html")$send("<html><body>Not an image</body></html>"))
    app$use(webfakes::mw_static(root = testthat::test_path("images")))
    webfakes::local_app_process(app, .local_envir = .local_envir)
}
