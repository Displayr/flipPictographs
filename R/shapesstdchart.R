#' Wrapper function to create Pictographs
#'
#' \code{ShapesStdChart}
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param groupBy Optional vector for aggregating \code{x}.
#' @param image Name of icon, e.g. \code{"star", "people",...}.
#' @param hide.base.image Turns off background image (on by default). In most cases it is appropriate to turn off the base image if K varies between entries.
#' @param aggregate.period Time period in groupBy (e.g. "month", "year").
#' @param direction Direction in which icons are filled (\code{horizontal}(default) or \code{vertical}). When vertical is used, the icons are placed in a single column per entry.
#' @param transpose Swap rows and columns in data matrix \code{x}.
#' @param hide.label.left Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, it is taken from the rownames of \code{x}.
#' @param hide.label.top Suppress labels above graphics.
#' @param ... Arguments passed to \code{PictoChart()}.
#' @importFrom flipChartBasics AsChartMatrix
#' @export
#'
ShapesStdChart <- function(x,
                          groupBy=NULL,
                          variable.image="red",
                          gradient.type="color",
                          gradient.col1="red",
                          gradient.col2="blue",
                          gradient.dir="column",
                          gradient.dsize=5,
                          base.image="grey",
                          image.type="circle",
                          direction="radial",
                          text.type="percentage",
                          text.position="overlay",
                          text.text=NULL,
                          aggregate.period="month",
                          transpose=FALSE,
                          hide.label.left=FALSE,
                          hide.label.right=FALSE,
                          hide.label.bottom=FALSE,
                          hide.label.top=FALSE,
                          label.left=c(),
                          label.top=c(),
                          label.bottom=c(),
                          label.bottom.halign="center",
                          column.width=20,
                          row.height=20,
                          ...)
{

    x <- AsChartMatrix(y=x, x=groupBy, transpose=(transpose), aggregate.period=aggregate.period)
    if (any(x < 0) || any(x > 1))
        stop("x must be between 0 and 1\n")

    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x))) 1
         else ncol(x)

    if (hide.label.left)
        label.left <- rep("", n)
    if (hide.label.top)
        label.top <- rep("", m)
    if (hide.label.bottom)
        label.bottom <- rep("", m)
    if (hide.label.right)
        label.right <- rep("", n)

    if (gradient.type == "color")
    {
        n.c <- n
        if (gradient.dir == "column")
            n.c <- m
        c.rgb <- colorRamp(c(gradient.col1, gradient.col2))(seq(0,1,length=n.c))
        c.hex <- rgb(c.rgb[,1], c.rgb[,2], c.rgb[,3], maxColorValue=255)
        variable.image <- matrix(c.hex, n, m, byrow=(gradient.dir=="column"))
    }

    if (gradient.type == "size")
    {
        if (gradient.dir == "row")
        {
            row.height <- 20 + (gradient.dsize*(1:n))
            column.width <- max(row.height)
        }
        else
        {
            column.width <- 20 + (gradient.dsize*(1:m))
            row.height <- max(column.width)
        }
    }

    return(PictoChart(x, image.type=image.type, direction=direction,
                      variable.image=variable.image, base.image=base.image,
                      K=1, icon.nrow=1, icon.ncol=1, wh.ratio=0, show.legend=F,
                      column.width=column.width, row.height=row.height,
                      label.left=label.left, label.top=label.top,
                      label.bottom=label.bottom, label.bottom.halign=label.bottom.halign,
                      text.position=text.position, text.type=text.type, text.text=text.text, ...))
}


