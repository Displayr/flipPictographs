#' Wrapper function to create Pictographs
#'
#' \code{PictoStdChart} provides a simpler interface to create pictographs. In particular it can automatically aggregate data to different time scales.
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param groupBy Optional vector for aggregating \code{x}.
#' @param image Name of icon, e.g. \code{"star", "people",...}.
#' @param hide.base.image Turns off background image (on by default). In most cases it is appropriate to turn off the base image if K varies between entries.
#' @param K Maximum number of icons in each table cell. By default, it will be taken to be \code{ceiling(x)} (if icon.autosize is on) or \code{ceiling(max(x))}. This variable is ignored if \code{read.KfromX} is \code{true}.
#' @param read.KfromX If set to true, maximum number of icons in each row is taken from the last column of X. This option cannot be used with \code{groupBy}.
#' @param scale Value of one icon. If \code{scale = 0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param legend.text Text shown with legend. If this string is empty, it will be automatically filled in using \code{scale}. (To hide text completely, use \code{legend.text = " "})
#' @param aggregate.period Time period in groupBy (e.g. "month", "year").
#' @param icon.autosize Rescale icons to fill up table cell. If K varies between cells, this can result in icons of varying sizes.
#' @param direction Direction in which icons are filled (\code{horizontal}(default) or \code{vertical}). When vertical is used, the icons are placed in a single column per entry.
#' @param transpose Swap rows and columns in data matrix \code{x}.
#' @param hide.label.left Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, it is taken from the rownames of \code{x}.
#' @param hide.label.top Suppress labels above graphics.
#' @param mode Can be set to one of \code{"table", "bar", "column"}. For options \code{bar} and \code{column}, the chart is constrained to look like a bar or column chart. e.g For \code{mode = "column"}, 1-dimensional vectors/matrices are re-shaped to have multiple columns, labels are put below the graphis and icons are arranged vertically. Option \code{mode = "table"} is the most general and does not impose constraints.
#' @param ... Arguments passed to \code{PictoChart()}.
#' @importFrom flipChartBasics AsChartMatrix
#' @export
#'
PictoStdChart <- function(x,
                          groupBy=NULL,
                          image="star",
                          hide.base.image=FALSE,
                          K=0,
                          K.varying=hide.base.image,
                          read.KfromX=FALSE,
                          scale=0,
                          legend.text="",
                          aggregate.period="month",
                          mode="table",
                          direction="fromleft",
                          icon.nrow=1,
                          icon.ncol=0,
                          icon.autosize=FALSE,
                          icon.halign="left",
                          icon.valign="center",
                          transpose=FALSE,
                          hide.label.left=FALSE,
                          hide.label.right=FALSE,
                          hide.label.bottom=FALSE,
                          hide.label.top=FALSE,
                          label.left=c(),
                          label.top=c(),
                          label.bottom=c(),
                          label.bottom.halign="center",
                          wh.ratio=0,
                          text.type="none",
                          text.text=NULL,
                          text.position="footer",
                          ...)
{
    # Get maximum before any aggregating
    K.default <- 0
    if (K == 0 && is.numeric(x))
        K.default <- ceiling(max(x))

    # Get data into the right format
    if (read.KfromX && (is.null(groupBy) || is.na(groupBy)))
    {
        x2 <- x
        K <- ceiling(x2[,ncol(x2)])
        x <- x2[,-ncol(x2)]
    }
    x <- AsChartMatrix(y=x, x=groupBy, transpose=(transpose), aggregate.period=aggregate.period)
    if (K == 0 && K.default == 0)
        K.default <- ceiling(max(x))

    # Need to get counts before scaling
    if (text.type == "count")
    {
        text.type <- "raw"
        text.text <- as.character(unlist(x))
    }

    if (scale==0 && max(x) > 1)
        scale <- max(1, floor(max(x)/10))
    if (scale==0 && max(x) <= 1)
        scale <- 10^{log10(median(x))}

    if (nchar(legend.text)==0 && scale > 0)
        legend.text = sprintf(paste("= %.", 0-min(0,floor(log10(scale))), "f", sep=""), scale)
    x <- x/scale
    if (read.KfromX)
        K <- K/scale

    if (mode=="column")
    {
        if (is.null(dim(x)))
        {
            tmpnames <- names(x)
            x <- matrix(x, nrow=1)
            colnames(x) <- tmpnames
        }
        if (ncol(x) == 1)
        {
            tmpnames <- rownames(x)
            x <- matrix(unlist(x), nrow=1)
            colnames(x) <- tmpnames
        }
        label.bottom <- colnames(x)
        direction <- "frombottom"
        hide.label.top <- TRUE
        icon.valign <- "bottom"
        icon.autosize <- FALSE
        K.varying <- TRUE
        text.position <- "header"

    }
    if (mode=="bar")
    {
        if (!is.null(dim(x)) && nrow(x) == 1)
        {
            tmpnames <- colnames(x)
            x <- matrix(unlist(x), ncol=1)
            rownames(x) <- tmpnames
        }
        direction <- "fromleft"
        icon.autosize <- FALSE
        K.varying <- TRUE
    }

    # If read.KfromX fails, K tries default values
    if (all(K == 0))
        K <- if (K.varying) ceiling(x)
             else ceiling(K.default/scale)

    # Fix dimensions using icon.ncol - icon.nrow will be adjusted in pictochart()
    if (icon.ncol == 0)
        icon.ncol <- unlist(K)/icon.nrow
    if (direction %in% c("vertical", "fromtop", "frombottom"))
        icon.ncol <- 1

    if (hide.label.left)
    {
        n <- if (is.null(nrow(x))) length(x)
             else nrow(x)
        label.left <- rep("", n)
    }
    if (hide.label.top)
    {
        m <- if (is.null(ncol(x))) 1
             else ncol(x)
        label.top <- rep("", m)
    }
    if (hide.label.bottom)
    {
        m <- if (is.null(ncol(x))) 1
             else ncol(x)
        label.bottom <- rep("", m)
    }
    if (hide.label.right)
    {
        n <- if (is.null(nrow(x))) length(x)
             else nrow(x)
        label.right <- rep("", n)
    }

    base.image <- ""
    if (!hide.base.image)
        base.image <- imageURL[image,"bg"]
    return(PictoChart(x, variable.image=imageURL[image,"fg"],
                      base.image=base.image, wh.ratio=imageWHRatio[image],
                      K=K, icon.nrow=icon.nrow, icon.ncol=icon.ncol, icon.fixedsize=1-icon.autosize,
                      icon.halign=icon.halign, icon.valign=icon.valign,
                      label.left=label.left, label.top=label.top,
                      label.bottom=label.bottom, label.bottom.halign=label.bottom.halign,
                      direction=direction, legend.text=legend.text,
                      text.position=text.position, text.type=text.type, text.text=text.text, ...))
}


