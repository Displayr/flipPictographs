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
                          groupBy=NULL,
                          image="star.filled",
                          base.image="star.empty",
                          K=0,
                          read.KfromX=FALSE,
                          units=1,
                          legend.text=sprintf("= %d", units),
                          aggregate.period="month",
                          direction="horizontal",
                          icon.autosize=FALSE,
                          icon.halign="left",
                          transpose=FALSE,
                          hide.labels=FALSE, ...)
{
    x <- x/units
    if (read.KfromX && is.null(groupBy))
    {
        K <- ceiling(x[,ncol(x)])
        x <- x[,-ncol(x)]
    }
    x <- AsChartMatrix(y=x, x=groupBy, transpose=(!transpose), aggregate.period=aggregate.period)

    if (K == 0 && !read.KfromX)
        K <- ifelse(icon.autosize, ceiling(x), ceiling(max(x)))
    icon.ncol <- unlist(K)

    if (direction=="vertical")
        icon.ncol <- 1

    label.left <- c()
    label.top <- c()
    if (hide.labels)
    {
        n <- if (is.null(nrow(x))) length(x)
             else nrow(x)
        m <- if (is.null(ncol(x))) 1
             else ncol(x)

        label.left <- rep("", n)
        label.top <- rep("", m)
    }

    URL <- c(none = "",
                star.filled = "http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg",
                star.empty = "http://wiki.q-researchsoftware.com/images/f/f2/Star_unfilled.svg",
                ppl.filled = "http://wiki.q-researchsoftware.com/images/9/98/Stick_man_black.svg",
                ppl.red = "http://wiki.q-researchsoftware.com/images/0/00/Stick_man_dark_red.svg",
                ppl.grey = "http://wiki.q-researchsoftware.com/images/8/89/Stick_man_light_grey.svg",
                wine.filled = "http://www.iconsdb.com/icons/preview/black/bar-2-xxl.png",
                drink.filled = "http://wiki.q-researchsoftware.com/images/3/3a/Cocktail.svg",
                drink.grey = "http://wiki.q-researchsoftware.com/images/a/a5/Cocktail_light_grey.svg",
                drop.pic = "http://wiki.q-researchsoftware.com/images/7/70/Water-drop.jpg",
                sheep = "http://wiki.q-researchsoftware.com/images/6/6a/Sheep-black.jpeg",
                pig = "http://wiki.q-researchsoftware.com/images/d/d4/Pig-black.png",
                cow = "http://wiki.q-researchsoftware.com/images/3/32/Cow-black.png",
                chicken = "http://wiki.q-researchsoftware.com/images/7/7d/Chicken-black.png",
                fish = "http://wiki.q-researchsoftware.com/images/d/d7/Fish-blue.png")

    return(PictoChart(x, variable.image=URL[image], base.image=URL[base.image],
                      K, icon.ncol=icon.ncol, icon.fixedsize=1-icon.autosize, icon.halign = icon.halign,
                      label.left=label.left, label.top=label.top,
                      direction=direction, legend.text=legend.text, ...))
}


