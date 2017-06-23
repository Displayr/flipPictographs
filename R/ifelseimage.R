#' IfElseImage
#'
#' Conditionally shows an image as a htmlwidget
#' @param condition Expression evaluating to \code{TRUE} or {FALSE} that determines which image is displayed.
#' @param true.image URL to image (jpeg, png or svg) to be shown when \code{condition} is \code{TRUE}.
#' @param false.image URL to image (jpeg, png or svg) to be shown when \code{condition} is \code{FALSE}.
#' @examples
#' IfElseImage(TRUE)
#' IfElseImage(3 < 2,
#'             "https://dl.dropboxusercontent.com/u/539177224/thumbsup_grey.svg",
#'             "https://dl.dropboxusercontent.com/u/539177224/thumbsdown_grey.svg")
#' @export

IfElseImage <- function(condition,
            true.image  = "https://dl.dropboxusercontent.com/u/539177224/uparrow_grey.svg",
            false.image = "https://dl.dropboxusercontent.com/u/539177224/downarrow_grey.svg")
{
    if (is.na(condition) || !is.logical(condition))
        stop("Parameter 'condition' should be TRUE or FALSE.")

    image <- if (condition) true.image else false.image
    SinglePicto(1, 1, is.custom.url=T, image=image)
}
