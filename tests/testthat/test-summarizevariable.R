context("SummarizeVariable")

x0 <- 1:10
x.logical <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
x.numeric <- x0/10
x.categoric <- factor(letters[c(1,1,1,2,3,4,5,6)], levels = letters[6:1])
x.date <- as.Date("2010-04-01") + 1:10
ww <- rep(c(1,5), each = 5)

test_that("SummarizeVariable",
{
    expect_equal(SummarizeVariable(x0, "Sum"), 55)
    expect_equal(SummarizeVariable(x0, "Sum", weight = ww), 215)
    expect_equal(round(SummarizeVariable(x.logical, "Percentage"),2), 0.67)
    expect_equal(round(SummarizeVariable(x.logical, "Average"),2), 0.67)
    expect_equal(SummarizeVariable(x.logical, "Sum"), 4)
    expect_warning(SummarizeVariable(x.logical, "Percentage", category="3"),
                   "Showing percentage selected")
    expect_warning(SummarizeVariable(x.categoric, "Average"),
                   "Data has been automatically converted to numeric")
    expect_equal(round(SummarizeVariable(x.date, "Average"),2), 14705.5)

    expect_equal(SummarizeVariable(x.numeric, "Average"), 0.55)
    expect_warning(SummarizeVariable(x.numeric, "Percentage", category = 1),
                   "A numeric variable was supplied")

    expect_equal(SummarizeVariable(x.numeric, "Percentage", category = "0.4 - 0.6"), 0.3)
})
