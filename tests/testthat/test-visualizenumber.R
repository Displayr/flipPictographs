context("VisualizeNumber")
library(flipTransformations)

# This function replaces SinglePicto as the function used in the Standard R pages
test_that("VisualizeNumber",
{
    expect_error(VisualizeNumber(0.4, maximum.value = 1.0), NA)

    expect_error(VisualizeNumber(ParseText("40%"), display = "Pictograph (single icon)",
                    label.data.number.type = "Percentage",
                    maximum.value = ParseText("100%")), NA)

    expect_error(VisualizeNumber(ParseText("40%"), display = "Pictograph (repeated icons)",
                    label.data.number.type = "Percentage",
                    maximum.value = ParseText("100%"), total.icons = 10), NA)

    expect_error(VisualizeNumber(4, display = "Pictograph (repeated icons)", scale = ParseText("100%"),
                    label.data.number.type = "Percentage",
                    total.icons = NA), NA)

    expect_error(VisualizeNumber(4, display = "Pictograph (repeated icons)", scale = 2,
                    label.data.number.type = "Percentage",
                    total.icons = NA), NA)

    expect_error(VisualizeNumber(ParseText("-40%"), display = "Gauge",
                    label.data.number.type = "Percentage",
                    maximum.value = ParseText("100%"),
                    minimum.value = ParseText("-100%")), NA)


})
