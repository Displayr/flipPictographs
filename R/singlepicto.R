#' SinglePicto
#'
#' Creates a single pictograph. Allows customization of the number of icons
#' and dimensions.
#' @seealso PictoStdChart to create a chart or table of pictographs
#'
#' @param x Data which determines the number of icons (\code{= x/scale}) filled in the pictograph.
#' @param total.icons Total number of icons. Defaults to \code{total.icons=ceiling(x/scale)}.
#' @param scale Scaling factor for \code{x}. Defaults to 1.
#' @param number.rows Controls layout of icons. If neither \code{number.rows} and \code{number.cols} is supplied, the default behaviour is to place icons in to a square. Note that number.rows is ignored when number.cols is non-zero.
#' @param number.cols Maximum number of icons in each column.
#' @param image name of icon
#' @param hide.base.image Set to \code{TRUE} to use blank background instead of base image.
#' @param fill.direction Direction in which pictograph is filled (one of \code{"fromleft","fromright","fromtop","frombottom"}).
#' @param fill.icon.color Color of the filled icons
#' @param base.icon.color Color of the unfilled icons when \code{hide.base.image == FALSE}. Defaults to grey (#CCCCCC).
#' @param background.color Color of the graphic background
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param icon.width Width of a single icon in pixels when \code{auto.size} is \code{FALSE}.
#' @param pad.row Vertical space between icons. This should be a number between 0 (no space) and 1.0 (all space).
#' @param pad.col Horizontal space between icons.
#' @param margin Controls space on margins of the graphic. When \code{margin} is used, space on all 4 sides are adjusted simultaneously, but margins can also be adjusted separately using \code{margin.top, margin.right, margin.bottom, margin.left}.
#' @param print.config If set to \code{TRUE}, the JSON string used to generate pictograph will be printed to standard output. This is useful for debugging.
#'
#' @importFrom  rhtmlPictographs graphic
#' @export
SinglePicto <- function (x,
                         total.icons = NA,
                         image = "star",
                         scale = 1,
                         number.rows = NA,
                         number.cols = NA,
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
                         margin.left = margin,
                         print.config = FALSE)
{
    if (!(length(x) == 1 && x >= 0))
        stop("x must be a single numeric value\n")
    if (scale <= 0)
        stop("scale must be greater than zero\n")

    sc10 <- log10(x/scale)
    if (sc10 > 2)
    {
        scale <- scale * 10^{round(sc10)}
        warning("x is too large to plot. scale has been set to", scale, "\n")
    }

    x <- x/scale
    if (is.na(total.icons))
        total.icons <- ceiling(x)


    if (length(total.icons) != 1 && total.icons > 0)
        stop("total.icons must be a single numeric value and greater than zero\n")
    if (!is.na(number.rows) && (number.rows <= 0 || number.rows != ceiling(number.rows)))
        stop("number.rows must be a positive integer\n")
    if (!is.na(number.cols) && (number.cols <= 0 || number.cols != ceiling(number.cols)))
        stop("number.cols must be a positive integer\n")

    prop <- x/total.icons
    if (prop < 0 | prop > 1)
        stop("x/scale must be between 0 and total.icons\n")
    if (round(total.icons) != total.icons)
        stop("total.icons must be an integer\n")

    layout.str <- ""
    if (!is.na(number.rows)  && is.na(number.cols))
    {
        layout.str <- paste(",\"numRows\":", number.rows, sep="")
    }
    if (!is.na(number.cols))
    {
        layout.str <- paste(",\"numCols\":", number.cols, sep="")
        number.rows <- ceiling(total.icons/number.cols)
    }
    if (is.na(number.rows) && is.na(number.cols))
        number.rows <- floor(sqrt(total.icons))

    base.image.str <- ""
    if (nchar(base.icon.color) > 0)
        base.icon.color <- paste(base.icon.color, ":", sep="")
    if (!hide.base.image)
        base.image.str <- paste(",\"baseImage\":\"url:", base.icon.color, imageURL[image], "\"", sep="")
    variable.image <- paste("url:", fill.direction, ":", fill.icon.color, ":", imageURL[image], sep="")

    image.height <- (icon.width/imageWHRatio[image] * number.rows) + margin.top + margin.bottom
    image.width <- (icon.width * ceiling(total.icons/number.rows)) + margin.left + margin.right
    json.string <- paste("{\"proportion\":", prop,
          ",\"numImages\":", total.icons,
          layout.str,
          ",\"variableImage\":\"", variable.image, "\"", base.image.str,
          ",\"width\":", image.width,
          ",\"height\":", image.height,
          ",\"background-color\":\"", background.color, "\"",
          ",\"columnGutter\":", pad.col,
          ",\"rowGutter\":", pad.row,
          ",\"padding\":\"", paste(margin.top, margin.right, margin.bottom, margin.left, sep = " "), "\"",
          sep = "")

    json.string <- if(auto.size) paste(json.string, ", \"preserveAspectRatio\":\"xMidYMid\"}", sep="")
            else paste(json.string, ",\"resizable\":\"false\"}", sep="")
    if (print.config)
        cat(json.string)
    graphic(json.string)
}
