#' Create chart of pictographs
#'
#' \code{PictographChart} Create a chart to visually represent the data in a table or vector using the number of icons displayed
#' @aliases PictoStdChart
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param image Name of icon, e.g. \code{"star", "stickman",...}.
#' @param is.custom.url Whether the image parameter is a url supplied by the user.
#' @param hide.base.image Turns off background image (on by default). In general, the base image should only be shown if the input data is a proportion.
#' @param total.icons Maximum number of icons in each table cell. By default, it will be taken to be \code{ceiling(x)} (if icon.autosize is on) or \code{ceiling(max(x))}. This variable is ignored if \code{read.KfromX} is \code{true}.
#' @param scale Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param legend.text Text shown with legend. If this string is empty, it will be automatically filled in using \code{scale}. (To hide text completely, use \code{legend.text  =  " "})
#' @param fill.direction Direction in which icons are filled (\code{horizontal}(default) or \code{vertical}). When vertical is used, the icons are placed in a single column per entry.
#' @param row.names.to.remove List of rownames to exclude from the chart. This can be in the form of a vector or a comma-separated string. This variable is ignored if the input data has no rownames.
#' @param column.names.to.remove List of colnames to exclude from the chart.
#' @param hide.label.left Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, it is taken from the rownames of \code{x}.
#' @param hide.label.top Suppress labels above graphics.
#' @param mode Can be set to one of \code{"table", "bar", "column"}. For options \code{bar} and \code{column}, the chart is constrained to look like a bar or column chart. e.g For \code{mode  =  "column"}, 1-dimensional vectors/matrices are re-shaped to have multiple columns, labels are put below the graphis and icons are arranged vertically. Option \code{mode  =  "table"} is the most general and does not impose constraints.
#' @param data.label.position When \code{label.data.type != "none"}, the position of the data labels can be one of \code{"Above icons", "Below icons"} (all modes) or \code{"On left", "On right"} (bar mode only). Note that \code{"On left"} will overrride \code{sublabel.left} and \code{"On right"} will overrride \code{sublabel.right}.
#' @importFrom flipChartBasics AsChartMatrix
#' @importFrom flipTransformations RemoveRowsAndOrColumns
#' @seealso PictoChart
#' @export
#' @inheritParams PictoChart
#'
PictographChart <- function(x,
                          image = "star",
                          is.custom.url = FALSE,
                          hide.base.image = FALSE,
                          total.icons = NA,
                          scale = NA,
                          mode = "table",
                          icon.palette = "Strong colors",
                          icon.colors = "black",
                          fill.direction = "fromleft",
                          show.lines = FALSE,
                          layout = NA,
                          icon.nrow = 1,
                          icon.ncol = NA,
                          table.by.row = FALSE,
                          row.names.to.remove = "",
                          column.names.to.remove = "",
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
                          sublabel.left = NA,
                          sublabel.right = NA,
                          label.pad = 0,
                          label.left.pad = 0,
                          label.right.pad = 0,
                          label.bottom.align.horizontal = "center",
                          label.left.align.horizontal = "left",
                          label.right.align.horizontal = "left",
                          label.top.align.horizontal = "center",
                          label.left.align.vertical = "center",
                          label.top.align.vertical = "center",
                          label.right.align.vertical = "center",
                          label.bottom.align.vertical = "center",
                          sublabel.left.align.horizontal = "left",
                          sublabel.right.align.horizontal = "left",
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
                          sublabel.left.font.size = label.left.font.size,
                          sublabel.right.font.size = label.right.font.size,
                          label.left.font.weight = "normal",
                          label.top.font.weight = "normal",
                          label.bottom.font.weight = "normal",
                          label.right.font.weight = "normal",
                          sublabel.left.font.weight = label.left.font.weight,
                          sublabel.right.font.weight = label.right.font.weight,
                          label.data.type = "none",
                          label.data.text = NULL,
                          label.data.position = ifelse(mode=="bar", "right", "footer"),
                          label.data.font.weight = "normal",
                          label.data.font.size = 0.8*label.font.size,
                          label.data.font.color = label.font.color,
                          label.data.align.horizontal = "default",
                          data.above.label = FALSE,
                          ...)
{
    if (!is.numeric(unlist(x)))
        stop("Input data must be numeric")

    # Parameter substitutions for R Gui Controls
    fill.direction <- gsub(" ", "", tolower(fill.direction))
    label.data.type <- tolower(label.data.type)
    if (!is.custom.url)
        image <- gsub(" ", "", tolower(image))
    if (label.data.type == "none")
    {
        label.data.align.horizontal <- "center"
        label.data.position <- ""
        data.above.label <- FALSE
    }
    if (label.data.type != "none")
    {
        if (label.data.position == "Above icons")
            label.data.position <- "header"
        if (label.data.position == "Below icons")
            label.data.position <- "footer"
        if (label.data.position == "On left")
            label.data.position <- "left"
        if (label.data.position == "On right")
            label.data.position <- "right"

        if (!(label.data.position %in% c("header", "footer", "left", "right")))
            stop("label.data.position should be one of \'header\', \'footer\', \'left\' or \'right\'")
        if (mode != "bar" && label.data.position %in% c("left", "right"))
            stop("label.data.position can only be \'left\' or \'right\' if mode = \'bar\'")
    }
    if (label.data.type == "percentage" && is.na(total.icons) && max(x) > 1)
        warning("Percentage is calculated as the proportion of icons filled out of the total icons. The value for total icons has not been supplied and is taken as the maximum of the supplied data.")

    if (!is.na(layout))
    {
        if (layout != "Number of rows")
            icon.nrow <- NA
        if (layout != "Number of columns")
            icon.ncol <- NA
    }
    if (mode=="bar" && hide.label.left && hide.label.right)
        label.width <- NA

    if (hide.label.left)
    {
        label.left.width <- NA
        label.left.font.size <- label.font.size
        label.left.font.weight <- "normal"
        label.left.align.horizontal <- "center"
        label.left.align.vertical <- "center"
    }
    if (hide.label.right)
    {
        label.right.width <- NA
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

    # Get maximum before any aggregating
    total.icons.tmp <- ceiling(max(x))
    if (is.na(total.icons) && is.numeric(x))
        total.icons.tmp <- ceiling(max(x))
    if (is.na(total.icons) && is.factor(x))
        total.icons.tmp <- sum(!is.na(x))

    # Some basic checks on input data
    #x <- AsChartMatrix(y = x, x = by, transpose = (transpose), aggregate.period = aggregate.period)
    if (length(dim(x)) > 2)
    {
        err.msg <- ifelse(is.null(attr(x,"questions")), "x has too many dimensions\n",
                          "Input table should only contain one statistic per cell\n")
        stop(err.msg)
    }
    x <- as.matrix(x)
    #if (!is.atomic(x) && !is.table(x) && !is.matrix(x) && !is.data.frame(x) && !is.array(x))
    #    stop(paste("x must be a vector, matrix, data.frame or array"))

    # special case for 1-d arrays
    #if (length(dim(x)) == 1)
    #    x <- matrix(x, ncol = 1, dimnames = list(names(x)))
    if (is.na(total.icons) && is.na(total.icons.tmp))
        total.icons.tmp <- ceiling(max(x))

    # Reshaping arrays/matrices and removing unwanted rows/columns
    if (mode == "column")
    {
        if (is.na(icon.ncol))
            icon.ncol <- 1
        icon.nrow <- NA
        if (!is.null(dim(x)) && min(dim(x)) > 1)
            stop("Input data should be in a single row or column")
        if (ncol(x) == 1)
            x <- t(x)
    }
    if (mode == "bar")
    {
        if (is.na(icon.ncol))
            icon.nrow <- 1
        else
            icon.nrow <- NA
        if (!is.null(dim(x)) && min(dim(x)) > 1)
            stop("Input data should be in a single row or column")
        if (nrow(x) == 1)
            x <- t(x)
    }
    x <- RemoveRowsAndOrColumns(x, row.names.to.remove, column.names.to.remove)

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
    if (label.data.type != "none")
        label.data.type <- "raw"

    # Adjust labels based on chart type
    if (mode == "column")
    {

        # Defaults will put labels on the top - add functionality for bottom
        if (!hide.label.bottom)
            label.bottom <- colnames(x)

    }
    if (mode == "bar")
    {
        # Defaults will put labels on the left
        if (!hide.label.right)
        {
            label.right.pad <- label.pad
            label.right <- rownames(x)
        }
        if (!hide.label.left)
            label.left.pad <- label.pad

        # Allow data labels to be positioned near row labels
        if (label.data.position == "left")
        {
            label.left.pad <- label.pad
            if (label.data.align.horizontal == "default")
                label.data.align.horizontal <- label.left.align.horizontal
            if (!hide.label.left && data.above.label)
            {
                sublabel.left <- rownames(x)
                sublabel.left.font.size <- label.left.font.size
                sublabel.left.font.weight <- label.left.font.weight
                sublabel.left.align.horizontal <- label.left.align.horizontal

                label.left <- label.data.text
                label.left.font.size <- label.data.font.size
                label.left.font.weight <- label.data.font.weight
                label.left.align.horizontal <- label.data.align.horizontal
            } else
            {
                sublabel.left <- label.data.text
                sublabel.left.font.size <- label.data.font.size
                sublabel.left.font.weight <- label.data.font.weight
                sublabel.left.align.horizontal <- label.data.align.horizontal
            }
            label.data.type <- "none"
        }
        if (label.data.position == "right")
        {
            label.right.pad <- label.pad
            if (label.data.align.horizontal == "default")
                label.data.align.horizontal <- label.right.align.horizontal
            if (!hide.label.right && data.above.label)
            {
                sublabel.right <- rownames(x)
                sublabel.right.font.size <- label.right.font.size
                sublabel.right.font.weight <- label.right.font.weight
                sublabel.right.align.horizontal <- label.right.align.horizontal

                label.right <- label.data.text
                label.right.font.size <- label.data.font.size
                label.right.font.weight <- label.data.font.weight
                label.right.align.horizontal <- label.data.align.horizontal

            } else
            {
                sublabel.right <- label.data.text
                sublabel.right.font.size <- label.data.font.size
                sublabel.right.font.weight <- label.data.font.weight
                sublabel.right.align.horizontal <- label.right.align.horizontal
            }
            label.data.type <- "none"
        }
    }



    # Fix dimensions using icon.ncol - icon.nrow will be adjusted in pictochart()
    if (is.na(icon.ncol))
        icon.ncol <- unlist(total.icons)/icon.nrow
    icon.ncol <- min(icon.ncol, total.icons)

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

    if (image %in% c("circle", "square"))
        image.type <- image
    else
        image.type <- "url"

    # Icon colors
    if (icon.palette == "User-specified")
    {
        c.hex <- unlist(strsplit(split=",", icon.colors))
        tryCatch(tmp.col <- col2rgb(c.hex),
                 error = function(cond){cat("Invalid color specified\n"); c.hex <- "black"})
    } else
    {
        c.length <- m
        if (m == 1 || table.by.row)
            c.length <- n

        c.hex <- flipChartBasics::ChartColors(c.length, given.colors = icon.palette)
        c.hex <- c.hex[1:c.length]
        if (any(is.na(c.hex)))
            stop("Unknown color palette specified")
    }
    c.hex <- matrix(c.hex, n, m, byrow = !table.by.row)
    if (is.na(legend.icon.color))
        legend.icon.color <- c.hex[1]


    # Font colors
    if (label.color.asIcon && mode == "bar")
    {
        label.left.font.color <- c.hex[,1]
        label.right.font.color <- c.hex[,1]
        label.data.font.color <- c.hex[,1]
    }

    if (label.color.asIcon && mode == "column")
    {
        label.top.font.color <- c.hex[1,]
        label.bottom.font.color <- c.hex[1,]
        label.data.font.color <- c.hex[1,]
    }

    if (label.data.align.horizontal == "default")
        label.data.align.horizontal <- "right"

    image.url <- if (is.custom.url) image else imageURL[image]
    base.image <- if (hide.base.image || is.custom.url) NA else imageURL[image]
    fill.icon.color <- if (is.custom.url) "" else c.hex
    width.height.ratio <- if (is.custom.url) NA else imageWHRatio[image]

    return(PictoChart(x, fill.image = image.url, fill.icon.color = fill.icon.color, image.type = image.type,
                      base.image = base.image, width.height.ratio = width.height.ratio,
                      total.icons = total.icons, show.lines = show.lines,
                      icon.nrow = icon.nrow, icon.ncol = icon.ncol, #icon.fixedsize = 1-icon.autosize,
                      #icon.align.horizontal = icon.align.horizontal, icon.align.vertical = icon.align.vertical,
                      label.left = label.left, label.top = label.top, label.right = label.right,
                      sublabel.left = sublabel.left, sublabel.right = sublabel.right,
                      label.font.family = label.font.family,
                      label.left.width = label.left.width, label.right.width = label.right.width,
                      label.font.color = label.font.color,
                      label.left.font.color = label.left.font.color, label.right.font.color = label.right.font.color,
                      label.top.font.color = label.top.font.color, label.bottom.font.color = label.bottom.font.color,
                      label.font.size = label.font.size, label.left.font.size = label.left.font.size,
                      label.right.font.size = label.right.font.size, label.bottom.font.size = label.bottom.font.size,
                      sublabel.left.font.size = sublabel.left.font.size, sublabel.right.font.size = sublabel.right.font.size,
                      label.data.font.size = label.data.font.size, label.top.font.size = label.top.font.size,
                      label.left.font.weight = label.left.font.weight, label.right.font.weight = label.right.font.weight,
                      label.top.font.weight = label.top.font.weight, label.bottom.font.weight = label.bottom.font.weight,
                      sublabel.left.font.weight = sublabel.left.font.weight, sublabel.right.font.weight = sublabel.right.font.weight,
                      label.left.align.horizontal = label.left.align.horizontal, label.top.align.horizontal = label.top.align.horizontal,
                      label.right.align.horizontal = label.right.align.horizontal, label.bottom.align.horizontal = label.bottom.align.horizontal,
                      sublabel.left.align.horizontal = sublabel.left.align.horizontal, sublabel.right.align.horizontal = sublabel.right.align.horizontal,
                      label.left.align.vertical = label.left.align.vertical,
                      label.top.height = label.top.height,
                      label.top.align.vertical = label.top.align.vertical,
                      label.bottom.align.vertical = label.bottom.align.vertical,
                      label.right.align.vertical = label.bottom.align.vertical,
                      label.bottom = label.bottom,
                      fill.direction = fill.direction, pad.legend = pad.legend,
                      show.legend = show.legend, legend.text = legend.text, legend.icon.color = legend.icon.color,
                      label.data.position = label.data.position, label.data.type = label.data.type,
                      label.data.text = label.data.text, label.data.font.weight = label.data.font.weight,
                      label.data.font.color = label.data.font.color,
                      label.data.align.horizontal = label.data.align.horizontal,
                      label.left.pad = label.left.pad, label.right.pad = label.right.pad, ...))
}


