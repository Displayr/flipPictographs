#' Create chart of pictographs
#'
#' \code{PictographChart} Create a chart to visually represent the data in a table or vector using the number of icons displayed
#' @aliases PictoStdChart
#'
#' @param x Data to plot. Can be vector, matrix or data.frame.
#' @param image Name of icon (e.g. \code{"star", "stickman",...}) or URL to icon image if \code{is.custom.url}.
#' @param base.image URL of image to use as base image. Only used if \code{is.custom.url = TRUE} and \code{hide.base.image = FALSE}.
#' @param is.custom.url Whether the image parameter is a url supplied by the user.
#' @param hide.base.image Turns off background image (on by default). In general, the base image should only be shown if the input data is a proportion.
#' @param total.icons Maximum number of icons in each table cell. By default, it will be determine based on \code{ceiling(x)}.
#' @param icon.palette Name of palette used to color icons. Only applicable for in-built icons
#' @param icon.colors Vector of colors for icons. Only applicable when \code{icon.palette = "User-specified"} and in-built icons used.
#' @param layout May be one of \code{"Number of rows"} or \code{"Number of columns"}. This parameter controls how the configuration of icons is specified. If no string is supplied, it will be automatically determined depending on whether \code{icon.nrow} or \code{icon.ncol} is supplied.
#' @param scale Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param legend.text Text shown with legend. If this string is empty, it will be automatically filled in using \code{scale}. (To hide text completely, use \code{legend.text  =  " "})
#' @param fill.direction Direction in which icons are filled. One of \code{"From left", "From right", "From top", "From bottom"}.
#' @param row.names.to.remove List of rownames to exclude from the chart. This can be in the form of a vector or a comma-separated string. This variable is ignored if the input data has no rownames.
#' @param column.names.to.remove List of colnames to exclude from the chart.
#' @param hide.label.left Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, labels are taken from the rownames of \code{x}.
#' @param hide.label.top Suppress labels above graphics.
#' @param hide.label.bottom Suppress labels below graphics (shown by default when \code{mode = "column"}).
#' @param hide.label.right Suppress labels on right of graphics.
#' @param mode Can be set to one of \code{"table", "bar", "column"}. For options \code{bar} and \code{column}, the chart is constrained to look like a bar or column chart. e.g For \code{mode  =  "column"}, 1-dimensional vectors/matrices are re-shaped to have multiple columns, labels are put below the graphis and icons are arranged vertically. Option \code{mode  =  "table"} is the most general and does not impose constraints.
#' @param fix.icon.nrow When \code{mode="bar" and hide.base.image=T}, set to \code{FALSE} to allow the bars to contain varying number of rows.
#' @param table.by.row By default, when a 2-dimensional table is supplied, values in each column will be shown in the same color. Set to \code{TRUE} to color values by row.
#' @param label.color.asIcon When set to \code{TRUE}, row and data labels are shown in the same color as the icons.
#' @param label.data.position When \code{show.label.data}, the position of the data labels can be one of \code{"Above icons", "Below icons"} (all modes) or \code{"Next to bar", "Above row label", "Below row label"} (bar mode only). Note that the last two options will overrride \code{sublabel.left} and \code{sublabel.right}
#' @param show.label.data Boolean indicating whether or not to show data labels.
#' @param customize.label.data Boolean indicating whether of not users want to customize data labels. By default this is on, but when set to \code{FALSE}, parameters \code{label.data.digits, label.data.100prc, label.data.prefix, label.data.suffix} is ignored.
#' @param data.above.label Set to \code{TRUE}, to place data labels above row labels.
#' @param label.data.digits Number of digits to show after decimal place.
#' @param label.data.bigmark Option to prettify large numbers. By default a comma is placed after a thousand.
#' @param label.data.100prc Option to show data labels multiplied by 100. This is useful when reporting percentages.
#' @param label.data.prefix String to prepend data label.
#' @param label.data.suffix String to append to data label.
#' @param label.data.type Does nothing. Retained for backwards compatibility.
#' @param label.pad Numeric specifying padding around the labels. Alternatively, the user can individually specify \code{label.left.pad} (horizontal space between left row label and icons), \code{label.right.pad} (horizontal space between right row label and icons) and \code{label.vpad} (vertical space above and below the row labels.
#' @param ... Arguments to pass to pictoChart
#' @importFrom flipChartBasics AsChartMatrix
#' @importFrom flipTransformations RemoveRowsAndOrColumns
#' @importFrom grDevices col2rgb
#' @importFrom stats median
#' @importFrom rhtmlPictographs graphic
#' @examples
#' xx <- c(First = 3, Second = 6, Third=2)
#' PictographChart(xx, image="circle", mode="bar")
#' PictographChart(xx, image="elephant", hide.base.image=TRUE, show.label.data=TRUE, mode="bar")
#' PictographChart(xx, total.icons=10, mode="bar", fill.direction="fromright", is.custom.url=TRUE,
#'    image="http://wiki.q-researchsoftware.com/images/a/a9/Stick_woman_dark_red.png",
#'    base.image="http://wiki.q-researchsoftware.com/images/7/78/Stick_man_light_grey.png")
#' @export
#' @inheritParams pictoChart
#'
PictographChart <- function(x,
                          image = "stickman",
                          base.image = "",
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
                          fix.icon.nrow = TRUE,
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
                          label.pad = 5,    # just for convenience
                          label.vpad = label.pad, #spacing above and below row labels
                          label.left.pad = label.pad,
                          label.right.pad = label.pad,
                          label.bottom.align.horizontal = "center",
                          label.left.align.horizontal = "default",
                          label.right.align.horizontal = "default",
                          label.top.align.horizontal = "center",
                          label.left.align.vertical = ifelse(is.na(icon.ncol[1]), "center", "top"),
                          label.top.align.vertical = "center",
                          label.right.align.vertical = ifelse(is.na(icon.ncol[1]), "center", "top"),
                          label.bottom.align.vertical = "center",
                          width.height.ratio = NA,
                          label.top.height = NA,
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
                          label.left.font.weight = "normal",
                          label.top.font.weight = "normal",
                          label.bottom.font.weight = "normal",
                          label.right.font.weight = "normal",
                          show.label.data = FALSE,
                          customize.label.data = TRUE,
                          label.data.digits = 0,
                          label.data.bigmark = ",",  # to prettify large numbers
                          label.data.prefix = "",
                          label.data.suffix = "",
                          label.data.100prc = FALSE,
                          label.data.position = ifelse(mode=="bar", "Next to bar", "footer"),
                          label.data.font.weight = "normal",
                          label.data.font.size = 0.8*label.font.size,
                          label.data.font.color = label.font.color,
                          label.data.align.horizontal = "default",
                          label.data.type = "None",   # does nothing, retained for backwards compatability
                          data.above.label = FALSE,
                          ...)
{
    # Parameters not controlled by the user by passed to pictoChart
    label.width = NA
    label.left.width = label.width
    label.right.width = label.width
    label.data.text = NULL

    if (!is.numeric(unlist(x)))
        stop("Input data must be numeric")

    if (label.data.type != "None")
        show.label.data <- TRUE

    if (is.custom.url && nchar(base.image) == 0)
        hide.base.image <- TRUE

    # Parameter substitutions for R Gui Controls
    fill.direction <- gsub(" ", "", tolower(fill.direction))
    if (!is.custom.url)
        image <- gsub(" ", "", tolower(image))

    # For !show.label.data, values will be ignored
    # But for !customize.label.data, values will be overridden later
    if (!show.label.data || !customize.label.data)
    {
        if (!show.label.data)
        {
            label.data.align.horizontal <- "center"
            label.data.position <- ""
        }
        data.above.label <- FALSE
        label.data.prefix <- ""
        label.data.suffix <- ""
        label.data.100prc <- FALSE
        label.data.digits <- 0
        label.data.font.size <- 12
    }
    if (show.label.data)
    {
        if (label.data.position == "Above icons")
            label.data.position <- "header"
        if (label.data.position == "Below icons")
            label.data.position <- "footer"
        if (label.data.position == "On left")
            label.data.position <- "left"
        if (label.data.position == "On right")
            label.data.position <- "right"
    }

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

    if (label.left.align.horizontal == "default")
        label.left.align.horizontal <- "right"
    if (label.right.align.horizontal == "default")
        label.right.align.horizontal <- "left"

    if (!show.legend)
    {
        legend.text <- ""
        pad.legend <- 0
        legend.icon.color <- NA
    }

    if (show.label.data && !customize.label.data)
    {
        stat <- attr(x, "statistic")
        if (!is.null(stat) && grepl("%", stat))
        {
            label.data.suffix  <- "%"
            label.data.100prc <- all(x <= 1)
        }
    }

    # More parameters not controlled by the user but passed to pictoChart
    sublabel.left = NA
    sublabel.right = NA
    sublabel.left.align.horizontal = "left"
    sublabel.right.align.horizontal = "left"
    sublabel.left.font.size = label.left.font.size
    sublabel.right.font.size = label.right.font.size
    sublabel.left.font.weight = label.left.font.weight
    sublabel.right.font.weight = label.right.font.weight
    show.label.float = FALSE
    label.float.text = NULL
    label.float.font.weight = label.data.font.weight
    label.float.font.size = label.data.font.size
    label.float.font.color = label.data.font.color
    label.float.align.horizontal = "left"
    label.float.align.vertical = "center"

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

    # Reshaping arrays/matrices and removing unwanted rows/columns
    if (mode == "column")
    {
        if (all(is.na(icon.ncol)))
            icon.ncol <- 1
        icon.nrow <- NA
        if (!is.null(dim(x)) && min(dim(x)) > 1)
            stop("Input data should be a one-dimensional table or a numeric vector")
        if (is.null(rownames(x)) && is.null(colnames(x)))
            stop("Input data should have row or column names")
        if (is.null(colnames(x)))
            x <- t(x)
    }
    if (mode == "bar")
    {
        if (all(is.na(icon.ncol)))
            icon.nrow <- 1
        else
            icon.nrow <- NA
        if (!is.null(dim(x)) && min(dim(x)) > 1)
            stop("Input data should be a one-dimensional table or a numeric vector")
        if (is.null(rownames(x)) && is.null(colnames(x)))
            stop("Input data should have row or column names")
        if (is.null(rownames(x)))
            x <- t(x)
    }
    x <- RemoveRowsAndOrColumns(x, row.names.to.remove, column.names.to.remove)

    # Data labels
    label.data.values <- unlist(x) * (1+(99*label.data.100prc))
    if (!customize.label.data && max(label.data.values) <= 1)
        label.data.digits <- 2
    label.data.text <- sprintf("%s%s%s", label.data.prefix,
        formatC(label.data.values, digits=label.data.digits, format="f", big.mark=label.data.bigmark),
        label.data.suffix)

    # Automatically set scale to be nearest power of 10
    if (is.na(scale))
        scale <- 10^{round(log10(max(x)) - 1)}
    #if (is.na(scale) && max(x) <=  1)
    #    scale <- 10^{round(log10(median(x)))}
    if (scale <= 0)
        stop("Scale must be greater than zero\n")

    if (all(is.na(total.icons)))
        total.icons <- ceiling(max(x)/scale)

    if (nchar(legend.text) == 0 && scale > 0)
        legend.text  =  sprintf(paste(" =  %.", 0-min(0,floor(log10(scale))), "f", sep = ""), scale)

    x <- x/scale
    # if mode==table, and dim(x)==2 and show.legend is false, check range of data
    # if range differs by more than a range of 100 and the greatest and 2nd greatest
    # are in the same row/column then compute scale for each row/column

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
            label.right <- rownames(x)
        }



        if (!fix.icon.nrow && hide.base.image && !is.na(icon.ncol))
        {
            icon.nrow <- ceiling(x/icon.ncol)
            total.icons <- icon.nrow * icon.ncol
            icon.nrow <- NA  # needed so that icon.ncol is used
        }


        # Position data labels relative to fill direction
        if (label.data.position == "Next to bar" && hide.base.image)
        {
            show.label.float <- TRUE
            label.float.text <- label.data.text
            label.float.align.horizontal <- switch(fill.direction,
                                                   fromleft="left",
                                                   fromright="right")
            # Ignore icon.ncol if its too large
            if (all(!is.na(icon.ncol)) && all(icon.ncol >= total.icons))
            {
                icon.ncol <- NA
                icon.nrow <- 1
            }
            if (any(ceiling(x) >= total.icons) && !is.na(icon.nrow) && all(icon.nrow == 1))
                total.icons <- total.icons + 1
            show.label.data <- FALSE

        } else if (label.data.position == "Next to bar")
        {
            label.data.position <- switch(fill.direction,
                                          fromleft="right",
                                          fromright="left")

        } else if (label.data.position == "Below row label" || label.data.position == "Above row label")
        {
            if (label.data.position == "Above row label")
                data.above.label <- TRUE

            label.data.position <- switch(fill.direction,
                                          fromleft="left",
                                          fromright="right")
        }


        # Position of data labels in absolute terms
        if (label.data.position == "left")
        {
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
            show.label.data  <- FALSE
        }
        if (label.data.position == "right")
        {
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
            show.label.data <- FALSE
        }
    }


    # Fix dimensions using icon.ncol - icon.nrow will be adjusted in pictochart()
    if (all(is.na(icon.ncol)))
        icon.ncol <- unlist(total.icons)/icon.nrow + show.label.float
    if (length(icon.ncol) == 1)
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

        c.hex <- flipChartBasics::ChartColors(c.length, given.colors = icon.palette,
                                              palette.end = 1 - 0.2 * (icon.palette %in% c("Reds","Blues","Greens","Greys")),
                                              reverse = icon.palette %in% c("Reds","Blues","Greens","Greys"))
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
        label.float.font.color <- c.hex[,1]
    }

    if (label.color.asIcon && mode == "column")
    {
        label.top.font.color <- c.hex[1,]
        label.bottom.font.color <- c.hex[1,]
        label.data.font.color <- c.hex[1,]
        label.float.font.color <- c.hex[1,]
    }

    if (label.data.align.horizontal == "default")
        label.data.align.horizontal <- "right"

    image.url <- if (is.custom.url) image else imageURL[image]
    base.image <- if (hide.base.image) NA else if (is.custom.url) base.image else imageURL[image]
    fill.icon.color <- if (is.custom.url) "" else c.hex
    width.height.ratio <- if (is.custom.url) getWidthHeightRatio(image) else imageWHRatio[image]

    json <- NA
    f.mspace <- 0 # space in margin for floating labels
    while (is.na(json) || is.numeric(json))
    {
        json <- pictoChart(x, fill.image = image.url, fill.icon.color = fill.icon.color, image.type = image.type,
                      f.mspace = f.mspace,
                      base.image = base.image, width.height.ratio = width.height.ratio, show.lines = show.lines,
                      total.icons = total.icons, icon.nrow = icon.nrow, icon.ncol = icon.ncol,
                      label.left = label.left, label.top = label.top, label.right = label.right,
                      sublabel.left = sublabel.left, sublabel.right = sublabel.right,
                      label.font.family = label.font.family, label.font.color = label.font.color,
                      label.left.width = label.left.width, label.right.width = label.right.width,
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
                      label.left.align.vertical = label.left.align.vertical, label.top.align.vertical = label.top.align.vertical,
                      label.bottom.align.vertical = label.bottom.align.vertical, label.right.align.vertical = label.right.align.vertical,
                      label.bottom = label.bottom, label.top.height = label.top.height,
                      fill.direction = fill.direction, pad.legend = pad.legend,
                      show.legend = show.legend, legend.text = legend.text, legend.icon.color = legend.icon.color,
                      show.label.data = show.label.data, label.data.position = label.data.position,
                      label.data.text = label.data.text, label.data.font.weight = label.data.font.weight,
                      label.data.font.color = label.data.font.color, label.float.font.weight = label.float.font.weight,
                      label.data.align.horizontal = label.data.align.horizontal,
                      label.vpad = label.vpad, label.left.pad = label.left.pad, label.right.pad = label.right.pad,
                      show.label.float = show.label.float, label.float.text = label.float.text,
                      label.float.font.size = label.float.font.size, label.float.font.color = label.float.font.color,
                      label.float.align.horizontal = label.float.align.horizontal, label.float.align.vertical = label.float.align.vertical,
                      ...)
        if (is.na(json) && hide.base.image)
            total.icons <- total.icons + 1
        if(is.numeric(json))
            f.mspace <- f.mspace + json
    }
    return(graphic(json))
}


