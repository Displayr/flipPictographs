#' Wrapper function to create Pictographs
#'
#' \code{PictoStdChart} provides a simpler interface to create pictographs. In particular it can automatically aggregate data to different time scales.
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param image Name of icon, e.g. \code{"star", "stickman",...}.
#' @param hide.base.image Turns off background image (on by default). In most cases it is appropriate to turn off the base image if num.max.icon varies between entries.
#' @param num.max.icon Maximum number of icons in each table cell. By default, it will be taken to be \code{ceiling(x)} (if icon.autosize is on) or \code{ceiling(max(x))}. This variable is ignored if \code{read.KfromX} is \code{true}.
#' @param scale Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param legend.text Text shown with legend. If this string is empty, it will be automatically filled in using \code{scale}. (To hide text completely, use \code{legend.text  =  " "})
#' @param icon.autosize Rescale icons to fill up table cell. If num.max.icon varies between cells, this can result in icons of varying sizes.
#' @param fill.direction Direction in which icons are filled (\code{horizontal}(default) or \code{vertical}). When vertical is used, the icons are placed in a single column per entry.
#' @param bar.order Specify order of bars when \code{mode=="bar"}. Should be one of \code{NA, "Ascending", "Descending"}.
#' @param column.order Specify order of bars when \code{mode=="column"}. Should be one of \code{NA, "Ascending", "Descending"}.
#' @param transpose Swap rows and columns in data matrix \code{x}.
#' @param hide.label.left Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, it is taken from the rownames of \code{x}.
#' @param hide.label.top Suppress labels above graphics.
#' @param mode Can be set to one of \code{"table", "bar", "column"}. For options \code{bar} and \code{column}, the chart is constrained to look like a bar or column chart. e.g For \code{mode  =  "column"}, 1-dimensional vectors/matrices are re-shaped to have multiple columns, labels are put below the graphis and icons are arranged vertically. Option \code{mode  =  "table"} is the most general and does not impose constraints.
#' @param data.label.position When \code{label.data.type != "none"}, the position of the data labels can be one of \code{"Above icons", "Below icons} (all modes) or \code{"On left", "On right"} (bar mode only). Note that \code{"On left"} will overrride \code{label.left2} and \code{"On right"} will overrride \code{label.right2}.
#' @param ... Arguments passed to \code{PictoChart()}.
#' @importFrom flipChartBasics AsChartMatrix
#' @export
#'
PictoStdChart <- function(x,
                          by = NULL,
                          image = "star",
                          image.type = "url",
                          hide.base.image = FALSE,
                          total.icons = NA,
                          scale = NA,
                          aggregate.period = "month",
                          mode = "table",
                          #stack = FALSE,
                          #gradient.col1 = "deepskyblue",
                          #gradient.col2 = "orange",
                          #gradient.dir = "column",
                          icon.palette = "Set3",
                          icon.colors = "black",
                          fill.direction = "fromleft",
                          show.lines = FALSE,
                          layout = NA,
                          icon.nrow = 1,
                          icon.ncol = NA,
                          table.by.row = FALSE,
                          bar.order = NA,
                          column.order = NA,
                          #icon.autosize = FALSE,
                          #icon.align.horizontal = "left",
                          #icon.align.vertical = "center",
                          transpose = FALSE,
                          show.legend = FALSE,
                          legend.text = "",
                          legend.icon.color = NA,
                          pad.legend = 40,
                          hide.label.right = TRUE,
                          hide.label.left = !hide.label.right,
                          hide.label.bottom = (mode!="column"),
                          hide.label.top = (mode=="column"),
                          label.color.asIcon = FALSE,
                          label.left = NA,
                          label.top = NA,
                          label.bottom = NA,
                          label.right = NA,
                          label.left2 = NA,
                          label.right2 = NA,
                          label.bottom.align.horizontal = "center",
                          label.left.align.horizontal = "center",
                          label.right.align.horizontal = "center",
                          label.top.align.horizontal = "center",
                          label.left.align.vertical = "center",
                          label.top.align.vertical = "center",
                          label.left2.align.horizontal = "center",
                          label.right2.align.horizontal = "center",
                          width.height.ratio = NA,
                          label.width = NA,
                          label.top.height = NA,
                          label.left.width = label.width,
                          label.right.width = label.width,
                          label.font.family = "arial",
                          label.font.size = 12,
                          label.font.color = "#2C2C2C",
                          label.left.font.color = label.font.color,
                          label.right.font.color = label.font.color,
                          label.top.font.color = label.font.color,
                          label.bottom.font.color = label.font.color,
                          label.left.font.size = label.font.size,
                          label.top.font.size = label.font.size,
                          label.right.font.size = label.font.size,
                          label.bottom.font.size = label.font.size,
                          label.left2.font.size = label.left.font.size,
                          label.right2.font.size = label.right.font.size,
                          label.left.font.weight = "normal",
                          label.top.font.weight = "normal",
                          label.bottom.font.weight = "normal",
                          label.right.font.weight = "normal",
                          label.left2.font.weight = label.left.font.weight,
                          label.right2.font.weight = label.right.font.weight,
                          label.data.type = "none",
                          label.data.text = NULL,
                          label.data.position = "footer",
                          label.data.font.weight = "normal",
                          label.data.font.size = 0.8*label.font.size,
                          label.data.align.horizontal = "default",
                          label.data.onTop = FALSE,
                          ...)
{
    # Parameter substitutions for R Gui Controls
    fill.direction <- gsub(" ", "", tolower(fill.direction))
    label.data.type <- tolower(label.data.type)
    image <- gsub(" ", "", tolower(image))
    #gradient.dir <- tolower(gradient.dir)
    if (label.data.type == "none")
    {
        label.data.align.horizontal <- "center"
        if (label.data.position == "Above icons")
            label.data.position <- "header"
        if (label.data.position == "Below icons")
            label.data.position <- "footer"
    }
    if (!is.na(layout))
    {
        if (layout != "Number of rows")
            icon.nrow <- NA
        if (layout != "Number of columns")
            icon.ncol <- NA
    }
    if (hide.label.left)
    {
        label.width <- NA
        label.left.font.size <- label.font.size
        label.left.font.weight <- "normal"
        label.left.align.horizontal <- "center"
        label.left.align.vertical <- "center"
    }
    if (hide.label.right)
    {
        label.width <- NA
        label.right.font.size <- label.font.size
        label.right.font.weight <- "normal"
        label.right.align.horizontal <- "center"
        label.right.align.vertical <- "center"
    }
    if (hide.label.top)
    {
        label.top.height <- NA
        label.top.font.size <- label.font.size
        label.top.font.weight <- "normal"
        label.top.align.horizontal <- "center"
        label.top.align.vertical <- "center"
    }
    label.top.align.horizontal <- tolower(label.top.align.horizontal)
    label.top.align.vertical <- tolower(label.top.align.vertical)
    label.left.align.horizontal <- tolower(label.left.align.horizontal)
    label.left.align.vertical <- tolower(label.left.align.vertical)
    label.right.align.horizontal <- tolower(label.right.align.horizontal)
    label.right.align.vertical <- tolower(label.right.align.vertical)
    label.data.align.horizontal <- tolower(label.data.align.horizontal)

    if (!show.legend)
    {
        legend.text <- ""
        pad.legend <- 0
        legend.icon.color <- NA
    }

    # lump label alignments together
    # options are: top-left, top-right, center-center

    # Get maximum before any aggregating
    total.icons.tmp <- ceiling(max(x))
    if (is.na(total.icons) && is.numeric(x))
        total.icons.tmp <- ceiling(max(x))
    if (is.na(total.icons) && is.factor(x))
        total.icons.tmp <- sum(!is.na(x))

    x <- AsChartMatrix(y = x, x = by, transpose = (transpose), aggregate.period = aggregate.period)
    if (length(dim(x)) == 1)
        x <- matrix(x, ncol = 1, dimnames = list(names(x)))
    if (is.na(total.icons) && is.na(total.icons.tmp))
        total.icons.tmp <- ceiling(max(x))

    # Need to get counts before scaling (for data labels)
    count.data <- unlist(x)
    prop.data <- NA
    if (max(x) < 1)
        prop.data <- unlist(x)

    # Prefer scale to be a multiple of 5 - avoids rounding errors in text
    if (is.na(scale) && max(x) > 1)
        scale <- max(1, round(floor(total.icons.tmp/10)/5)*5)
    if (is.na(scale) && max(x) <=  1)
        scale <- 10^{round(log10(median(x)))}
    if (scale <= 0)
        stop("Scale must be greater than zero\n")

    if (is.na(total.icons))
        total.icons <- ceiling(total.icons.tmp/scale)

    if (nchar(legend.text) == 0 && scale > 0)
        legend.text  =  sprintf(paste(" =  %.", 0-min(0,floor(log10(scale))), "f", sep = ""), scale)

    x <- x/scale
    # if mode==table, and dim(x)==2 and show.legend is false, check range of data
    # if range differs by more than a range of 100 and the greatest and 2nd greatest
    # are in the same row/column then compute scale for each row/column

    # Handling data labels
    if (any(is.na(prop.data)))
        prop.data <- unlist(x)/total.icons
    if (label.data.type == "count")
       label.data.text <- gsub(" ", "", format(count.data, big.mark=",", scientific=F, digits=0+2*(max(x)<=1)))
    if (label.data.type == "percentage")
        label.data.text <- sprintf("%.0f%%", round(prop.data*100))
    if (label.data.type == "proportion")
        label.data.text <- sprintf("%.2f", prop.data)
    #if (label.data.text == "fraction")
    #    label.data.text <- sprintf("%.0f/%d", round(prop.data*total.icons), total.icons)
    if (label.data.type != "none")
        label.data.type <- "raw"

    # Restructure 2D matrices if not stacked and mode is column or bar

    if (mode == "column")
    {
        if (is.na(icon.ncol))
            icon.ncol <- 1
        icon.nrow <- NA

        if (!is.null(dim(x)) && min(dim(x)) > 1)
            stop("Column chart expects input of 1-dimensional array")

        # Also converted to a 2D matrix
        if (is.null(ncol(x)) || is.na(ncol(x)))
        {
            tmpnames <- names(x)
            x <- matrix(x, nrow = 1)
            colnames(x) <- tmpnames
        }
        if (ncol(x) == 1)
            x <- t(x)
        if (length(grep("cending", column.order)))
            x <- x[,order(x, decreasing=(column.order=="Descending")),drop=F]

        # Defaults will put labels on the top - add functionality for bottom
        if (!hide.label.bottom)
            label.bottom <- colnames(x)

    }
    if (mode == "bar")
    {
        if (is.na(icon.ncol))
            icon.nrow <- 1
        if (!is.null(dim(x)) && min(dim(x)) > 1)
            stop("Column chart expects input of 1-dimensional array")
        if (is.null(nrow(x)) || is.na(nrow(x)))
        {
            tmpnames <- names(x)
            x <- matrix(unlist(x), ncol = 1)
            rownames(x) <- tmpnames
        }
        if (nrow(x) == 1)
            x <- t(x)
        if (length(grep("cending", bar.order)))
            x <- x[order(x, decreasing=(bar.order=="Descending")),,drop=F]


        # Defaults will put labels on the left - add functionality for right
        if (!hide.label.right)
            label.right <- rownames(x)

        # Allow data labels to be positioned near row labels
        if (label.data.position == "On left")
        {
            if (label.data.align.horizontal == "default")
                label.data.align.horizontal <- label.left.align.horizontal
            if (!hide.label.left && label.data.onTop)
            {
                label.left2 <- rownames(x)
                label.left2.font.size <- label.left.font.size
                label.left2.font.weight <- label.left.font.weight
                label.left2.align.horizontal <- label.left.align.horizontal

                label.left <- label.data.text
                label.left.font.size <- label.data.font.size
                label.left.font.weight <- label.data.font.weight
                label.left.align.horizontal <- label.data.align.horizontal
            } else
            {
                label.left2 <- label.data.text
                label.left2.font.size <- label.data.font.size
                label.left2.font.weight <- label.data.font.weight
                label.left2.align.horizontal <- label.data.align.horizontal
            }
            label.data.type <- "none"
        }
        if (label.data.position == "On right")
        {
            if (label.data.align.horizontal == "default")
                label.data.align.horizontal <- label.right.align.horizontal
            if (!hide.label.right && label.data.onTop)
            {
                label.right2 <- rownames(x)
                label.right2.font.size <- label.right.font.size
                label.right2.font.weight <- label.right.font.weight
                label.right2.align.horizontal <- label.right.align.horizontal

                label.right <- label.data.text
                label.right.font.size <- label.data.font.size
                label.right.font.weight <- label.data.font.weight
                label.right.align.horizontal <- label.data.align.horizontal

            } else
            {
                label.right2 <- label.data.text
                label.right2.font.size <- label.data.font.size
                label.right2.font.weight <- label.data.font.weight
            }
            label.data.type <- "none"
        }
    }

    # Fix dimensions using icon.ncol - icon.nrow will be adjusted in pictochart()
    if (is.na(icon.ncol))
        icon.ncol <- unlist(total.icons)/icon.nrow

    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x)) || is.na(ncol(x))) 1
         else ncol(x)
    if (hide.label.left)
        label.left <- NULL
    if (hide.label.top)
        label.top <- NULL
    if (hide.label.bottom)
        label.bottom <- NULL
    if (hide.label.right)
        label.right <- NULL

    #if (image %in% c("circle", "square"))
    #    image.type <- image
#    if (stack)
#    {
#        return(pictoStack(x, image = image, image.type = image.type, mode = mode,
#                          #col1 = gradient.col1, col2 = gradient.col2,
#                          show.legend = show.legend, legend.icon.color = legend.icon.color, legend.text = legend.text,
#                         label.left = label.left, label.top = label.top, label.right = label.right, label.bottom = label.bottom,
#                          label.bottom.align.horizontal = label.bottom.align.horizontal,
#                          fill.direction = fill.direction,  ...))
#    }

    # Icon colors
    if (icon.palette == "User-specified")
    {
        c.hex <- unlist(strsplit(split=",", icon.colors))
    } else
    {
        c.length <- m
        if (m == 1 || table.by.row)
            c.length <- n

        c.hex <- flipChartBasics::ChartColors(c.length, given.colors = icon.palette)
        if (any(is.na(c.hex)))
            stop("Unknown color palette specified")
    }
    c.hex <- matrix(c.hex, n, m, byrow = table.by.row)
    if (is.na(legend.icon.color))
        legend.icon.color <- c.hex[1]


    # Font colors!
    if (label.color.asIcon && mode == "bar")
    {
        label.left.font.color <- c.hex[,1]
        label.right.font.color <- c.hex[,1]
    }

    if (label.data.align.horizontal == "default")
        label.data.align.horizontal <- "right"

    base.image <- NA
    if (!hide.base.image)
        base.image <- imageURL[image]

    return(PictoChart(x, fill.image = imageURL[image], fill.icon.color = c.hex, image.type = image.type,
                      base.image = base.image, width.height.ratio = imageWHRatio[image],
                      total.icons = total.icons, show.lines = show.lines,
                      icon.nrow = icon.nrow, icon.ncol = icon.ncol, #icon.fixedsize = 1-icon.autosize,
                      #icon.align.horizontal = icon.align.horizontal, icon.align.vertical = icon.align.vertical,
                      label.left = label.left, label.top = label.top, label.right = label.right,
                      label.left2 = label.left2, label.right2 = label.right2,
                      label.font.family = label.font.family,
                      label.left.width = label.left.width, label.right.width = label.right.width,
                      label.font.color = label.font.color,
                      label.left.font.color = label.left.font.color, label.right.font.color = label.right.font.color,
                      label.top.font.color = label.top.font.color, label.bottom.font.color = label.bottom.font.color,
                      label.font.size = label.font.size, label.left.font.size = label.left.font.size,
                      label.right.font.size = label.right.font.size, label.bottom.font.size = label.bottom.font.size,
                      label.left2.font.size = label.left2.font.size, label.right2.font.size = label.right2.font.size,
                      label.data.font.size = label.data.font.size, label.top.font.size = label.top.font.size,
                      label.left.font.weight = label.left.font.weight, label.right.font.weight = label.right.font.weight,
                      label.top.font.weight = label.top.font.weight, label.bottom.font.weight = label.bottom.font.weight,
                      label.left2.font.weight = label.left2.font.weight, label.right2.font.weight = label.right2.font.weight,
                      label.left.align.horizontal = label.left.align.horizontal, label.top.align.horizontal = label.top.align.horizontal,
                      label.right.align.horizontal = label.right.align.horizontal, label.bottom.align.horizontal = label.bottom.align.horizontal,
                      label.left2.align.horizontal = label.left2.align.horizontal, label.right2.align.horizontal = label.right2.align.horizontal,
                      label.left.align.vertical = label.left.align.vertical,
                      label.top.height = label.top.height,
                      label.top.align.vertical = label.top.align.vertical,
                      label.bottom = label.bottom,
                      fill.direction = fill.direction, pad.legend = pad.legend,
                      show.legend = show.legend, legend.text = legend.text, legend.icon.color = legend.icon.color,
                      label.data.position = label.data.position, label.data.type = label.data.type,
                      label.data.text = label.data.text, label.data.font.weight = label.data.font.weight,
                      label.data.align.horizontal = label.data.align.horizontal,...))
}


