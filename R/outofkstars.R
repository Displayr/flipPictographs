#' OutOfKStars
#'
#' @param x A number out of \code{K = number.rows * number.columns}.
#' @param number.rows The number of rows of stars.
#' @param number.columns The number of columns of stars.
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param width Width of the image when \code{auto.size} is FALSE.
#'
#' @importFrom  rhtmlPictographs graphic
#' @export
OutOfKStars <- function (x, number.rows, number.columns, auto.size = FALSE, width = 100)
{
    image.height <- 25*number.rows
    number.images <- number.columns * number.rows
    prop <- x/number.images
    if (prop < 0 | prop > 1)
        stop("x must be between 0 and ", number.images, "\n")

    base.image <- "http://wiki.q-researchsoftware.com/images/f/f2/Star_unfilled.svg"
    variable.image <- "horizontal:http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg"


    json.string <- paste("{\"percentage\":", prop,
          ",\"numImages\":", number.images,
          ",\"numRows\":", number.rows,
          ",\"baseImage\":\"", base.image, "\", ",
          "\"variableImage\":\"", variable.image, "\", ",
          "\"width\":", width,
          ",\"height\":", image.height,
          sep="")

    json.string <- if(auto.size) paste(json.string, ", \"preserveAspectRatio\":\"xMidYMid\"}", sep="")
            else paste(json.string, ",\"resizable\":\"false\"}", sep="")

    graphic(json.string)
}
