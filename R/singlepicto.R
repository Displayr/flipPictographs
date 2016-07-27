#' SinglePicto
#'
#' Creates a single pictograph. Allows customization of the number of icons
#' and dimensions.
#' @seealso PictoStdChart to create a chart or table of pictographs
#'
#' @param x Number of filled icons (out of \code{K = number.rows * number.columns})
#' @param number.icons Total number of icons.
#' @param number.rows Number of rows icons should be arranged into.
#' @param image name of icon
#' @param hide.base.image Set to \code{TRUE} to use blank background instead of background image.
#' @param direction Direction in which pictograph is filled (one of \code{"fromleft","fromright","fromtop","frombottom"}).
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param width Width of a single icon in pixels when \code{auto.size} is \code{FALSE}.
#' @param icon.color Color of icon
#' @param bg.color Background color of graphic.
#'
#' @importFrom  rhtmlPictographs graphic
#' @export
SinglePicto <- function (x,
                         number.icons,
                         number.rows=1,
                         image="star",
                         hide.base.image=FALSE,
                         direction="fromleft",
                         auto.size = FALSE,
                         width=50,
                         icon.color="black",
                         bg.color="transparent")
{
    image.height <- width/imageWHRatio[image] * number.rows
    prop <- x/number.icons
    if (prop < 0 | prop > 1)
        stop("x must be between 0 and ", number.icons, "\n")
    if (number.rows == 0 || round(number.rows) != number.rows)
        stop("number.rows must be an integer greater than 0\n")
    if (number.icons == 0 || round(number.icons) != number.icons)
        stop("number.icons must be an integer greater than 0\n")

    base.image.str <- ""
    if (!hide.base.image)
        base.image.str <- paste(",\"baseImage\":\"url:", imageURL[image], "\"", sep="")
    variable.image <- paste("url:", direction, ":", icon.color, ":", imageURL[image], sep="")

    json.string <- paste("{\"proportion\":", prop,
          ",\"numImages\":", number.icons,
          ",\"numRows\":", number.rows,
          ",\"variableImage\":\"", variable.image, "\"", base.image.str,
          ",\"width\":", ceiling(number.icons/number.rows)*width,
          ",\"height\":", image.height,
          ",\"background-color\":\"", bg.color, "\"",
          sep="")

    json.string <- if(auto.size) paste(json.string, ", \"preserveAspectRatio\":\"xMidYMid\"}", sep="")
            else paste(json.string, ",\"resizable\":\"false\"}", sep="")
    #cat(json.string)
    graphic(json.string)
}
