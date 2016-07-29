#' SinglePicto
#'
#' Creates a single pictograph. Allows customization of the number of icons
#' and dimensions.
#' @seealso PictoStdChart to create a chart or table of pictographs
#'
#' @param x Number of filled icons (0 <= x < total.icons)
#' @param total.icons Total number of icons.
#' @param number.rows Control layout of icons. Note that number.rows is ignored when number.cols is non-zero.
#' @param number.cols Maximum number of icons in each column.
#' @param image name of icon
#' @param hide.base.image Set to \code{TRUE} to use blank background instead of background image.
#' @param fill.direction Direction in which pictograph is filled (one of \code{"fromleft","fromright","fromtop","frombottom"}).
#' @param fill.icon.color Color of the filled icons
#' @param base.icon.color Color of the unfilled icons when \code{hide.base.image == FALSE}. Default is grey (#CCCCCC).
#' @param background.color Color of the graphic background
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param icon.width Width of a single icon in pixels when \code{auto.size} is \code{FALSE}.

#' @param pad.row Vertical space between icons. This should be a number between 0 (no space) and 1.0 (all space).
#' @param pad.col Horizontal space between icons.
#' @param margin Controls space on margins of the graphic. When \code{margin} is used, space on all 4 sides are adjusted simultaneously, but margins can also be adjusted separately using \code{margin.top, margin.right, margin.bottom, margin.left}.
#'
#' @importFrom  rhtmlPictographs graphic
#' @export
SinglePicto <- function (x,
                         total.icons,
                         number.rows = 1,
                         number.cols = 0,
                         image = "star",
                         hide.base.image = FALSE,
                         fill.direction = "fromleft",
                         fill.icon.color = "black",
                         base.icon.color = "",
                         background.color = "transparent",
                         auto.size = FALSE,
                         icon.width = 50,
                         pad.row = 0,
                         pad.col = 0,
                         margin = 0,
                         margin.top = margin,
                         margin.right = margin,
                         margin.bottom = margin,
                         margin.left = margin)
{

    prop <- x/total.icons
    if (prop < 0 | prop > 1)
        stop("x must be between 0 and ", total.icons, "\n")
    if (total.icons == 0 || round(total.icons) != total.icons)
        stop("total.icons must be an integer greater than 0\n")

    layout.str <- paste(",\"numRows\":", number.rows, sep="")
    if (number.cols > 0)
    {
        layout.str <- paste(",\"numCols\":", number.cols, sep="")
        number.rows <- ceiling(total.icons/number.cols)
    }

    base.image.str <- ""
    if (nchar(base.icon.color) > 0)
        base.icon.color <- paste(base.icon.color, ":", sep="")
    if (!hide.base.image)
        base.image.str <- paste(",\"baseImage\":\"url:", base.icon.color, imageURL[image], "\"", sep="")
    variable.image <- paste("url:", fill.direction, ":", fill.icon.color, ":", imageURL[image], sep="")

    image.height <- icon.width/imageWHRatio[image] * number.rows
    json.string <- paste("{\"proportion\":", prop,
          ",\"numImages\":", total.icons,
          layout.str,
          ",\"variableImage\":\"", variable.image, "\"", base.image.str,
          ",\"width\":", ceiling(total.icons/number.rows)*icon.width,
          ",\"height\":", image.height,
          ",\"background-color\":\"", background.color, "\"",
          ",\"columnGutter\":", pad.col,
          ",\"rowGutter\":", pad.row,
          ",\"padding\":\"", paste(margin.top, margin.right, margin.bottom, margin.left, sep = " "), "\"",
          sep = "")

    json.string <- if(auto.size) paste(json.string, ", \"preserveAspectRatio\":\"xMidYMid\"}", sep="")
            else paste(json.string, ",\"resizable\":\"false\"}", sep="")
    #cat(json.string)
    graphic(json.string)
}
