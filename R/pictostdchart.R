#' Wrapper function to create Pictographs
#'
#' \code{PictoStdChart} provides a simpler interface to create pictographs. In particular it can automatically aggregate data to different time scales.
#' @seealso \code{PictoChart()} provides finer control over plot parameters
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param variable.image URL of icon image.
#' @param base.image URL of base image (optional).
#' @param K Maximum number of icons in each table cell. This variable is ignored if \code{read.KfromX} is \code{true}.
#' @param read.KfromX. If set to true, maximum number of icons in each row is taken from the last column of X. This option cannot be used with \code{groupBy}.
#' @param groupBy Optional vector for aggregating \code{x}.
#' @param aggregate.period
#' @param icon.autosize Rescale icons to fill up table cell
#' @param icon.halign Alignment of icons when \code{icon.autosize} is \code{false}. Should be one of left, center or right.
#' @param direction Direction in which icons are filled (\code{horizontal} or \code{vertical}).
#' @param transpose Transpose \code{x}.
#' @param show.labels Use default labels based on row/column names.
#' @param show.table Put table lines around row/column labels.
#' @importFrom flipChartBasics AsChartMatrix
#' @export
#'
PictoStdChart <- function(x,
                          variable.image,
                          base.image=NULL,
                          K=0,
                          read.KfromX=FALSE,
                          groupBy=NULL,
                          aggregate.period="month",
                          icon.autosize=FALSE,
                          icon.halign="left",
                          direction="horizontal",
                          transpose=TRUE,
                          show.labels=TRUE,
                          show.table=FALSE)
{
    if (read.KfromX && is.null(groupBy))
    {
        K <- x[,ncol(x)]
        x <- x[,-ncol(x)]
    }
    x <- AsChartMatrix(x, groupBy, transpose, aggregate.period)

    if (K == 0 && !read.KfromX)
        K <- ifelse(icon.autosize, ceiling(x), ceiling(max(x)))
    icon.ncol <- unlist(K)

    if (direction=="vertical")
        icon.ncol <- 1

    label.left <- c()
    label.top <- c()
    if (!show.labels)
    {
        n <- if (is.null(nrow(x))) length(x)
             else nrow(x)
        m <- if (is.null(ncol(x))) 1
             else ncol(x)

        label.left <- rep("", n)
        label.top <- rep("", m)
    }

    return(PictoChart(x, variable.image, base.image, K,
                      icon.ncol=icon.ncol,
                      icon.fixedsize=1-icon.autosize, icon.halign = icon.halign,
                      label.left=label.left, label.top=label.top,
                      direction=direction, show.table=show.table))
}


