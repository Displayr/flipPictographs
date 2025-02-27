#' Rect
#'
#' Draws a rectangle
#' @param color One of 'red', 'green' or 'yellow' or a hex color code.
#' @param opacity A numeric value between 0 and 1.
#' @param print.config If set to \code{TRUE}, the JSON string used to generate pictograph will be printed to standard output. This is useful for debugging.
#' @importFrom rhtmlPictographs graphic
#' @importFrom flipU StopForUserError
#' @examples
#' Rect("red")
#' Rect("#000000", opacity=0.2)
#' @export
Rect <- function(color = "red", opacity = 0.9, print.config = FALSE)
{
    if (opacity < 0.0 || opacity > 1.0)
        StopForUserError("'opacity' should be a numeric between 0 and 1.")
    if (tolower(color) == "red")
        color <- rgb(255, 76, 92, maxColorValue = 255)
    if (tolower(color) == "yellow")
        color <- rgb(252, 174, 50, maxColorValue = 255)
    if (tolower(color) == "green")
        color <- rgb(39, 195, 153, maxColorValue = 255)

    config <- sprintf("{\"variableImage\":\"rect:%s:opacity=%f\", \"resizable\":\"true\"}",
                      color, opacity)
    if (print.config)
        cat(config, "\n")
    graphic(config)
}
