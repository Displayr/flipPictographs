#' SinglePicto
#'
#' Creates a single pictograph. Allows customization of the number of icons
#' and dimensions.
#' @seealso PictoStdChart to create a chart or table of pictographs
#'
#' @param x Number of filled icons (out of \code{K = number.rows * number.columns})
#' @param number.rows The number of rows of icons.
#' @param number.columns The number of columns of stars.
#' @param image name of icon
#' @param hide.base.image Set to \code{TRUE} to use blank background instead of background image.
#' @param direction Direction in which pictograph is filled (horizontal, vertical or radial).
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param width Width of a single icon in pixels when \code{auto.size} is FALSE.
#' @param bg.color Background color of graphic.
#' @param text.type One of \code{"none", "count", "proportion", "percentage"}
#'
#' @importFrom  rhtmlPictographs graphic
#' @export
SinglePicto <- function (x,
                         number.rows,
                         number.columns,
                         image="star",
                         hide.base.image=FALSE,
                         direction="horizontal",
                         auto.size = FALSE,
                         width=50,
                         bg.color="transparent",
                         text.type="none",
                         text.size=8,
                         text.weight=700,
                         text.halign="center")
{
    image.height <- width/imageWHRatio[image] * number.rows + (2*text.size*(text.type!="none"))
    number.images <- number.columns * number.rows
    prop <- x/number.images
    if (prop < 0 | prop > 1)
        stop("x must be between 0 and ", number.images, "\n")
    if (number.rows == 0 || round(number.rows) != number.rows)
        stop("number.rows must be an integer greater than 0\n")
    if (number.columns == 0 || round(number.columns) != number.columns)
        stop("number.columns must be an integer greater than 0\n")

    base.image.str <- ""
    if (!hide.base.image)
        base.image.str <- paste(",\"baseImage\":\"url:", imageURL[image, "bg"], "\"", sep="")
    variable.image <- paste("url:", direction, ":", imageURL[image, "fg"], sep="")

    text.str <- ""
    if (text.type != "none")
    {
        if (text.type == "count")
            text.type <- as.character(x)
        text.str <- sprintf(",\"text-footer\":{\"text\":\"%s\", \"font-size\":\"%fpx\",\"font-weight\":\"%d\", \"horizontal-align\":\"%s\"}",
                            text.type, text.size, text.weight, text.halign)
    }

    json.string <- paste("{\"proportion\":", prop,
          ",\"numImages\":", number.images,
          ",\"numRows\":", number.rows,
          ",\"variableImage\":\"", variable.image, "\"", base.image.str,
          ",\"width\":", number.columns*width,
          ",\"height\":", image.height,
          text.str,
          ",\"background-color\":\"", bg.color, "\"",
          sep="")

    json.string <- if(auto.size) paste(json.string, ", \"preserveAspectRatio\":\"xMidYMid\"}", sep="")
            else paste(json.string, ",\"resizable\":\"false\"}", sep="")
    #cat(json.string)
    graphic(json.string)
}
