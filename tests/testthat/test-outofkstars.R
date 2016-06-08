context("OutOfKStars")

test_that("OutOfKStars stops when it should", {
    expect_error( OutOfKStars(5, 1, 4) )
})


test_that("OutOfKStars works when it should", {
    expect_error( OutOfKStars(5, 1, 5), NA )
})
