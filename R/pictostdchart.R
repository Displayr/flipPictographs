#' Wrapper function to create Pictographs
#'
#' \code{PictoStdChart} provides a simpler interface to create pictographs. In particular it can automatically aggregate data to different time scales.
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param by Optional vector for aggregating \code{x}.
#' @param image Name of icon, e.g. \code{"star", "stickman",...}.
#' @param hide.base.image Turns off background image (on by default). In most cases it is appropriate to turn off the base image if num.max.icon varies between entries.
#' @param num.max.icon Maximum number of icons in each table cell. By default, it will be taken to be \code{ceiling(x)} (if icon.autosize is on) or \code{ceiling(max(x))}. This variable is ignored if \code{read.KfromX} is \code{true}.
#' @param scale Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param legend.text Text shown with legend. If this string is empty, it will be automatically filled in using \code{scale}. (To hide text completely, use \code{legend.text  =  " "})
#' @param aggregate.period Time period in by (e.g. "month", "year").
#' @param icon.autosize Rescale icons to fill up table cell. If num.max.icon varies between cells, this can result in icons of varying sizes.
#' @param fill.direction Direction in which icons are filled (\code{horizontal}(default) or \code{vertical}). When vertical is used, the icons are placed in a single column per entry.
#' @param transpose Swap rows and columns in data matrix \code{x}.
#' @param hide.label.left Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, it is taken from the rownames of \code{x}.
#' @param hide.label.top Suppress labels above graphics.
#' @param mode Can be set to one of \code{"table", "bar", "column"}. For options \code{bar} and \code{column}, the chart is constrained to look like a bar or column chart. e.g For \code{mode  =  "column"}, 1-dimensional vectors/matrices are re-shaped to have multiple columns, labels are put below the graphis and icons are arranged vertically. Option \code{mode  =  "table"} is the most general and does not impose constraints.
#' @param ... Arguments passed to \code{PictoChart()}.
#' @importFrom flipChartBasics AsChartMatrix
#' @export
#'
PictoStdChart <- function(x,
                          by = NULL,
                          image = "star",
                          hide.base.image = FALSE,
                          total.icons = 0,
                          scale = 0,
                          legend.text = "",
                          legend.icon.color = "",
                          aggregate.period = "month",
                          mode = "table",
                          stack = FALSE,
                          gradient.col1 = "deepskyblue",
                          gradient.col2 = "orange",
                          gradient.dir = "column",
                          fill.direction = "fromleft",
                          icon.nrow = 1,
                          icon.ncol = 0,
                          icon.autosize = FALSE,
                          icon.align.horizontal = "left",
                          icon.align.vertical = "center",
                          transpose = FALSE,
                          hide.label.left = FALSE,
                          hide.label.right = FALSE,
                          hide.label.bottom = FALSE,
                          hide.label.top = FALSE,
                          label.left = c(),
                          label.top = c(),
                          label.bottom = c(),
                          label.bottom.align.horizontal = "center",
                          width.height.ratio = 0,
                          label.data.type = "none",
                          label.data.text = NULL,
                          label.data.position = "footer",
                          ...)
{
    # Get maximum before any aggregating
    total.icons.tmp <- 0
    if (total.icons == 0 && is.numeric(x))
        total.icons.tmp <- ceiling(max(x))
    if (total.icons == 0 && is.factor(x))
        total.icons.tmp <- sum(!is.na(x))

    x <- AsChartMatrix(y = x, x = by, transpose = (transpose), aggregate.period = aggregate.period)
    if (length(dim(x)) == 1)
        x <- matrix(x, ncol = 1, dimnames = list(names(x)))
    if (total.icons == 0 && total.icons.tmp == 0)
        total.icons.tmp <- ceiling(max(x))

    # Need to get counts before scaling
    if (label.data.type == "count")
    {
        label.data.type <- "raw"
        label.data.text <- as.character(unlist(x))
    }

    # Prefer scale to be a multiple of 5 - avoids rounding errors in text
    if (scale == 0 && max(x) > 1)
        scale <- max(1, round(floor(total.icons.tmp/10)/5)*5)
    if (scale == 0 && max(x) <=  1)
        scale <- 10^{round(log10(median(x)))}

    if (total.icons == 0)
        total.icons <- ceiling(total.icons.tmp/scale)

    if (nchar(legend.text) == 0 && scale > 0)
        legend.text  =  sprintf(paste(" =  %.", 0-min(0,floor(log10(scale))), "f", sep = ""), scale)
    x <- x/scale

    if (mode == "column")
    {
        if (is.null(dim(x)))
        {
            tmpnames <- names(x)
            x <- matrix(x, nrow = 1)
            colnames(x) <- tmpnames
        }
        if (ncol(x) == 1)
        {
            tmpnames <- rownames(x)
            x <- matrix(unlist(x), nrow = 1)
            colnames(x) <- tmpnames
        }
        label.bottom <- colnames(x)
        fill.direction <- "frombottom"
        hide.label.top <- TRUE
        icon.align.vertical <- "bottom"

    }
    if (mode == "bar")
    {
        if (!is.null(dim(x)) && nrow(x) == 1)
        {
            tmpnames <- colnames(x)
            x <- matrix(unlist(x), ncol = 1)
            rownames(x) <- tmpnames
        }
        fill.direction <- "fromleft"
        icon.autosize <- FALSE
    }


    # Fix dimensions using icon.ncol - icon.nrow will be adjusted in pictochart()
    if (icon.ncol == 0)
        icon.ncol <- unlist(total.icons)/icon.nrow
    if (fill.direction %in% c("vertical", "fromtop", "frombottom"))
        icon.ncol <- 1

    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x)) || is.na(ncol(x))) 1
         else ncol(x)
    if (hide.label.left)
        label.left <- rep("", n)
    if (hide.label.top)
        label.top <- rep("", m)
    if (hide.label.bottom)
        label.bottom <- rep("", m)
    if (hide.label.right)
        label.right <- rep("", n)

    if (stack)
    {
        return(pictoStack(x, image = image, mode = mode, col1 = gradient.col1, col2 = gradient.col2,
                          label.left = label.left, label.top = label.top,
                          label.bottom = label.bottom, label.bottom.align.horizontal = label.bottom.align.horizontal,
                          fill.direction = fill.direction, legend.text = legend.text, ...))
    }

    c.hex <- ""
    if (nchar(gradient.col1) > 0)
    {
        c.length <- m
        if (m == 1 || gradient.dir == "row")
            c.length <- n

        c.rgb <- colorRamp(c(gradient.col1, gradient.col2))(seq(0,1,length = c.length))
        c.hex <- rgb(c.rgb[,1], c.rgb[,2], c.rgb[,3], maxColorValue = 255)
        c.hex <- matrix(c.hex, n, m, byrow = (gradient.dir != "row"))
    }


    base.image <- ""
    if (!hide.base.image)
        base.image <- imageURL[image]
    return(PictoChart(x, fill.image = imageURL[image], fill.icon.color = c.hex,
                      base.image = base.image, width.height.ratio = imageWHRatio[image],
                      total.icons = total.icons,
                      icon.nrow = icon.nrow, icon.ncol = icon.ncol, icon.fixedsize = 1-icon.autosize,
                      icon.align.horizontal = icon.align.horizontal, icon.align.vertical = icon.align.vertical,
                      label.left = label.left, label.top = label.top,
                      label.bottom = label.bottom, label.bottom.align.horizontal = label.bottom.align.horizontal,
                      fill.direction = fill.direction, legend.text = legend.text, legend.icon.color = legend.icon.color,
                      label.data.position = label.data.position, label.data.type = label.data.type,
                      label.data.text = label.data.text, ...))
}


