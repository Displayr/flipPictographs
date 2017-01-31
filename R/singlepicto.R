#' SinglePicto
#'
#' Creates a single pictograph. Allows customization of the number of icons
#' and dimensions.
#' @seealso PictographChart to create a chart or table of pictographs
#' @param x Input data which determines the number of icons (\code{x/scale}) filled in the pictograph.
#' @param total.icons Total number of icons. Defaults to \code{total.icons=ceiling(x/scale)}.
#' @param image The name of the icon to use (e.g. \code{"star", "stickman"}) or the URL of an image when \code{is.custom.url} is true.
#' @param base.image The URL of the base image. Only used if \code{is.custom.url = TRUE} and \code{hide.base.image = FALSE}.
#' @param is.custom.url When set to true, image is expected to be a URL to an jpeg or png image file available from online.
#' @param scale Scaling factor for \code{x}. Defaults to 1.
#' @param layout Optional parameter to determine how the layout is specified. Can be one of \code{"Width-to-height ratio", "Number of rows", "Number of columns", "Fill graphic"}. If not supplied, a decision will be made based on which parameters are supplied
#' @param number.rows If neither \code{number.rows} and \code{number.cols} is supplied, the default behaviour is to place icons according to \code{width.height.ratio}. Note that number.rows is ignored when number.cols is non-zero.
#' @param number.cols Maximum number of icons in each column. Overrides \code{number.rows} and \code{width.height.ratio}.
#' @param width.height.ratio Width to height ratio of pictograph if \code{layout == "Width-to-height ratio"}.
#' @param hide.base.image Set to \code{TRUE} to use blank background instead of base image.
#' @param fill.direction Direction in which pictograph is filled (one of \code{"fromleft","fromright","fromtop","frombottom"}).
#' @param fill.icon.color Color of the filled icons. Only applicable for built-in icons.
#' @param base.icon.color Color of the unfilled icons when \code{hide.base.image == FALSE}. Defaults to grey (#CCCCCC). Only applicable for built-in icons.
#' @param background.color Color of the graphic background
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param icon.width Width of a single icon in pixels when \code{auto.size} is \code{FALSE}.
#' @param pad.row Vertical space between icons. This should be a number between 0 (no space) and 1.0 (all space).
#' @param pad.col Horizontal space between icons.
#' @param margin Controls space on margins of the graphic. When \code{margin} is used, space on all 4 sides are adjusted simultaneously, but margins can also be adjusted separately using \code{margin.top, margin.right, margin.bottom, margin.left}.
#' @param print.config If set to \code{TRUE}, the JSON string used to generate pictograph will be printed to standard output. This is useful for debugging.
#' @param label.data.position One of \code{"None"}, \code{"Above"} or \code{"Below"}.
#' @param label.data.digits Number of digits of the data label to show.
#' @param label.data.bigmark Option to prettify large numbers. By default a comma is placed after a thousand.
#' @param label.data.100prc Option to show data labels multiplied by 100. This is useful when reporting percentages.
#' @param label.data.prefix String to prepend data label.
#' @param label.data.suffix String to append to data label.
#' @param x.limit Upper limit of x above which \code{scale} is automatically calculated. This can be set to \code{NA}, but may cause slowness or freezing when the user inputs a large \code{x}.
#' @importFrom  rhtmlPictographs graphic
#' @examples
#' xx <- 4
#' SinglePicto(xx)
#' SinglePicto(xx, total.icons=10, image="stickman", number.cols=5,
#'    fill.icon.color="red", base.icon.color="deepskyblue")
#' SinglePicto(xx, 9, number.rows=3, is.custom.url=TRUE,
#'    image="http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg",
#'    base.image="http://wiki.q-researchsoftware.com/images/2/21/Star_unfilled.png")
#' @export
SinglePicto <- function (x,
                         total.icons = NA,
                         image = "star",
                         base.image = "",
                         is.custom.url = FALSE,
                         number.rows = NA,
                         number.cols = NA,
                         width.height.ratio = 1,
                         graphic.width = NA,
                         graphic.height = NA,
                         layout = NA,
                         scale = 1,
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
                         label.data.position = c("None", "Above", "Below")[1],
                         label.data.digits = 0,
                         label.data.bigmark = ",",  # to prettify large numbers
                         label.data.prefix = "",
                         label.data.suffix = "",
                         label.data.100prc = FALSE,
                         label.data.font.weight = "normal",
                         label.data.font.size = 12,
                         label.data.font.family = "arial",
                         label.data.font.color = "#2C2C2C",
                         label.data.align.horizontal = "center",
                         graphic.resolution = 96,
                         font.whratio = 0.6,
                         print.config = FALSE,
                         x.limit = 1000)
{
    if (!(length(x) == 1 && x >= 0))
        stop("Input data must be a single numeric value\n")
    if (scale <= 0)
        stop("Scale must be greater than zero\n")

    if (!is.na(total.icons) && total.icons == 1 && x > 1 && scale == 1)
        stop("Input data should be a proportion between 0 and 1. To input count data select option for 'multiple icons'\n")

    # Some parameter substitutions for R GUI Controls
    if (!is.custom.url)
        image <- gsub(" ", "", tolower(image))
    fill.direction <- gsub(" ", "", tolower(fill.direction))
    if (auto.size)
        icon.width <- 50
    if (!is.na(layout))
    {
        if (layout != "Width-to-height ratio")
            width.height.ratio = 1
        if (layout != "Number of rows")
            number.rows = NA
        if (layout != "Number of columns")
            number.cols = NA
    }
    label.data.str <- ""
    label.data.values <- x

    # Determine plot values
    sc10 <- log10(x/scale)
    if (!is.na(x.limit) && x/scale > x.limit)
    {
        scale <- scale * 10^{floor(log10(x/scale)) - 1}
        warning("The input value is too large to plot, and the Scale has been set to ", scale, ". Consider entering a larger Scale value in the inputs.\n")
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
    if (width.height.ratio <= 0)
        stop("width.height.ratio must be greater than zero\n")
    if (icon.width <= 0)
        stop("icon.width must be greater than zero\n")

    prop <- x/total.icons
    if (prop < 0 | prop > 1)
        stop("Input data/scale must be between 0 and total.icons\n")
    if (round(total.icons) != total.icons)
        stop("total.icons must be an integer\n")

    # Determine layout based on which parameters are supplied
    layout.str <- ""
    icon.WHratio <- if (is.custom.url) getWidthHeightRatio(image) * (1 + pad.col) / (1 + pad.row)
                    else imageWHRatio[image] * (1 + pad.col) / (1 + pad.row)
    if (!is.na(number.rows)  && is.na(number.cols))
    {
        layout.str <- paste(",\"numRows\":", number.rows, sep="")
        number.cols <- ceiling(total.icons/number.rows)

    } else if (!is.na(number.cols))
    {
        layout.str <- paste(",\"numCols\":", number.cols, sep="")
        number.rows <- ceiling(total.icons/number.cols)
    } else
    {
        number.rows <- round(sqrt(icon.WHratio/width.height.ratio * total.icons))
        number.cols <- ceiling(total.icons/number.rows)
        layout.str <- paste(",\"numRows\":", number.rows, sep="")
    }

    image.type <- "url"
    if (image %in% c("circle", "square"))
        image.type <- image

    base.image.str <- ""
    if (!hide.base.image)
    {
        if (nchar(base.icon.color) > 0)
            base.icon.color <- paste(base.icon.color, ":", sep="")
        base.image.url <- if (is.custom.url) base.image else imageURL[image]
        base.image.str <- if (nchar(base.image.url) == 0 && is.custom.url) ""
                          else paste(",\"baseImage\":\"", image.type, ":", base.icon.color, base.image.url, "\"", sep="")
    }

    image.url <- if (is.custom.url) image else imageURL[image]
    variable.image <- if (is.custom.url)
        paste(image.type, ":", fill.direction, ":", image.url, sep="")
    else
        paste(image.type, ":", fill.direction, ":", fill.icon.color, ":", image.url, sep="")

    image.height <- (icon.width/icon.WHratio * number.rows) + margin.top + margin.bottom +
                    (label.data.position %in% c("Below","Above")) * label.data.font.size
    image.width <- (icon.width * ceiling(total.icons/number.rows)) + margin.left + margin.right

    # Data labels
    if (label.data.position != "None")
    {
        tmp.str <- ""
        if (label.data.position == "Next to icons")
        {
            x.pos <- ceiling(label.data.values/scale)
            label.float.position <- sprintf("%d:%d", floor(x.pos/number.cols),
                                                     x.pos %% number.cols)
            tmp.str <- "]"
        }

        label.data.text <- sprintf("%s%s%s", label.data.prefix,
                                formatC(label.data.values * (1+(99*label.data.100prc)),                                  digits=label.data.digits, format="f", big.mark=label.data.bigmark),
                                label.data.suffix)
        label.pos.str <- switch(label.data.position,
                                'Above' = "\"text-header\":{",
                                'Below' = "\"text-footer\":{",
                                'Next to icons' = sprintf("\"floatingLabels\":[{\"position\":\"%s\", ",
                                                           label.float.position))
        label.data.str <- sprintf(", %s\"text\":\"%s\", \"font-size\":\"%fpx\",
                                 \"font-weight\":\"%s\", \"font-family\":\"%s\",
                                 \"font-color\":\"%s\", \"horizontal-align\":\"%s\"}%s",
                            label.pos.str, label.data.text, label.data.font.size,
                            label.data.font.weight, label.data.font.family,
                            label.data.font.color, label.data.align.horizontal, tmp.str)
    }

    json.string <- paste("{\"proportion\":", prop,
          ",\"numImages\":", total.icons,
          layout.str,
          label.data.str,
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
