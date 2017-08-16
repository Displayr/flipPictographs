#' Circle
#'
#' Draws a circle
#' @param color One of 'red', 'green' or 'yellow'.
#' @export
Circle <- function(color = "red")
{
    if (!color %in% c("red", "yellow", "green"))
        stop("Parameter 'color' must be one of 'red', 'green', or 'yellow'.")
    SinglePicto(1.0, image=sprintf("https://dl.dropboxusercontent.com/u/539177224/circle_%s_transparent.svg", color),
                is.custom.url = T, auto.size = T)
}
