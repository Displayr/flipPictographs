#' PictoChart
#'
#' Function to create a chart of Pictographs. This function passes output directly to the rhtmlPictograph package and is more flexible (but less user-friendly) than the PictographChart function. In particular, it accepts any URL image as an icon, and most of the parameters will accept vectors as well scalar values.
#'
#' @param x Data for charting
#' @param fill.image URL of icon
#' @param base.image Optional URL of background image
#' @param total.icons Maximum icons in each table cell. Ignored if both \code{icon.nrow} and \code{icon.ncol} supplied.
#' @param fill.direction Accepts \code{horizontal}, \code{vertical}, \code{radial}, \code{scale}. (But \code{scale} may not be appropriate, especially if \code{total.icons} is important).
#' @param show.lines Add horizontal lines between each row
#' @param line.width Width of lines
#' @param line.color Line colour
#' @param show.legend Show legend (true or false).
#' @param legend.text Text to show beside legend icon.
#' @param legend.font.family.family Font of legend.
#' @param legend.font.size Text size of legend text.
#' @param background.color Background colour of pictograph
#' @param icon.nrow Configuration of icons in each table cell.
#' @param icon.ncol Configuration of icons in each table cell.
#' @param icon.fixedsize When \code{true}, icons will not automatically resize to fill table cell.
#' @param icon.align.horizontal Horizontal alignment of icons in cell when \code{icon.fixedsize} is \code{true}. Accepts values of left, right or center both as scalar and vectors.
#' @param label.left Length must be equal to length (if \code{x} is a vector) or number of rows (if \code{x} is matrix or data.frame) as x. If no value is supplied, labels will be read from names/rowname of \code{x}. To suppress labels, use \code{label.left  =  NULL}.
#' @param label.top By default, labels are read from column names of \code{x}.
#' @param label.bottom Optional labels below graphic cells. The length of the labels must be the same as the number of columns in \code{x}.
#' @param label.right Optional labels to the right of graphic cells. The length of the labels must be the same as the number of rows in \code{x}.
#' @param label.font.family Controls font-family of all labels. To modify only the font of one label use \code{label.left.font.family, label.top.font.family}, etc.
#' @param label.font.size Size of label text.
#' @param label.font.color Colour of labels.
#' @param label.top.height Height of top label row in pixels.
#' @param label.left.width Width of left label column in pixels.
#' @param label.left.align.horizontal Horizontal alignment.
#' @param label.left.align.vertical Vertical alignment.
#' @param row.height Height of graphic cells. Can be a single value or a numeric vector the same length as the number of rows in \code{x}.
#' @param column.width Width of graphic cells.
#' @param width.height.ratio Width-to-height ratio used to adjust row heights and column widths so they match the aspect ratio of the icon. Mis-specfication does not distort icon, but graphic will have extra spacing. When set to zero, row.height and column.width are unchanged, otherwise initial values are decreased to match \code{width.height.ratio}.
#' @param pad.row Single numeric specifying vertical spacing between graphic cells in the table.
#' @param pad.col Vertical spacing between cells in table.
#' @param pad.icon.row Numeric specifying vertical spacing between icons inside each table cell. May be a single value or a numeric matrix of the same dimensions as \code{x}.
#' @param pad.icon.ncol Horizontal spacing between icons inside each table cell.
#' @param font.whratio Numeric specifying the average aspect ratio of a single character, which usually varies around 0.4 - 0.6. It is used to calculate the minimum width of the labels.
#' @param graphic.width.inch Horizontal dimension of the chart output in inches. If these dimensions are not specified, the width-to-height ratio of the chart output may not match the desired dimensions.
#' @param graphic.height.inch Verical dimension of the chart output in inches.
#' @seealso PictographChart
#' @importFrom  rhtmlPictographs graphic
#' @export

PictoChart <- function(x,
                       fill.image,
                       base.image = NA,
                       image.type = "url",
                       total.icons = max(ceiling(x)),
                       fill.icon.color = "",
                       base.icon.color = "",
                       fill.direction = "fromleft",
                       show.lines = FALSE,
                       show.legend = FALSE,
                       legend.text = "",
                       icon.nrow = 1,
                       icon.ncol = unlist(total.icons)/icon.nrow,
                       label.left = NA,
                       label.right = NA,
                       label.top = NA,
                       label.bottom = NA,
                       sublabel.left = NA,
                       sublabel.right = NA,
                       label.left.pad = 5,       # padding between label and icon
                       label.right.pad = 5,
                       label.font.family = "arial",
                       label.font.size = 12,
                       label.font.color = "#2C2C2C",
                       label.top.font.family = label.font.family,
                       label.top.font.size = label.font.size,
                       label.top.font.weight = "normal",
                       label.top.font.color = label.font.color,
                       label.top.height = NA,
                       label.right.font.family = label.font.family,
                       label.right.font.size = label.font.size,
                       label.right.font.weight = "normal",
                       label.right.font.color = label.font.color,
                       label.right.width = NA,
                       label.bottom.font.family = label.font.family,
                       label.bottom.font.size = label.font.size,
                       label.bottom.font.weight = "normal",
                       label.bottom.font.color = label.font.color,
                       label.bottom.height = NA,
                       label.left.font.family = label.font.family,
                       label.left.font.size = label.font.size,
                       label.left.font.weight = "normal",
                       label.left.font.color = label.font.color,
                       label.left.width = NA,
                       label.left.align.horizontal = "right",
                       label.right.align.horizontal = "left",
                       label.top.align.horizontal = "center",
                       label.bottom.align.horizontal = "left",
                       label.left.align.vertical = "center",
                       label.right.align.vertical = "center",
                       label.top.align.vertical = "center",
                       label.bottom.align.vertical = "center",
                       sublabel.left.font.size = label.left.font.size,
                       sublabel.left.font.weight = label.left.font.weight,
                       sublabel.left.align.horizontal = label.left.align.horizontal,
                       sublabel.right.font.size = label.right.font.size,
                       sublabel.right.font.weight = label.right.font.weight,
                       sublabel.right.align.horizontal = label.right.align.horizontal,
                       legend.font.family = label.font.family,
                       legend.font.size = 0.8*label.font.size,
                       legend.font.weight = "normal",
                       legend.font.color = label.font.color,
                       legend.icon.color = fill.icon.color[1],
                       show.label.data = FALSE,
                       label.data.text = NULL,
                       label.data.position = "footer",
                       label.data.font.family = label.font.family,
                       label.data.font.size = 0.8*label.font.size,
                       label.data.font.weight = "normal",
                       label.data.font.color = label.font.color,
                       label.data.align.horizontal = "right",
                       show.label.float = FALSE,
                       label.float.text = NULL,
                       label.float.font.family = label.font.family,
                       label.float.font.size = 0.8*label.font.size,
                       label.float.font.weight = "normal",
                       label.float.font.color = label.font.color,
                       label.float.align.horizontal = "center",
                       label.float.align.vertical = "center",
                       row.height = NA,
                       column.width = NA,
                       width.height.ratio = NA,
                       background.color = "transparent",
                       line.color = "#A8A8A8",
                       line.width = 0.5,
                       pad.legend = 0.5*column.width[1],
                       pad.row = 2,
                       pad.col = 2,
                       pad.icon.row = 0.0,
                       pad.icon.col = 0.0,
                       #margin.top = 0,
                       #margin.right = 0,
                       #margin.bottom = 0,
                       #margin.left = 0,
                       graphic.width.inch = NA,
                       graphic.height.inch = NA,
                       font.whratio = 0.6,
                       print.config = FALSE)
{
    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x)) || is.na(ncol(x))) 1
         else ncol(x)
    if (is.na(width.height.ratio))
        width.height.ratio <- 1

    if (any(total.icons != ceiling(total.icons)))
        stop("Parameter total.icons must be a whole number\n")
    if (any(total.icons <= 0))
        stop("Parameter total.icons must be greater than zero\n")

    layout.str <- ifelse(is.na(icon.nrow), paste("\"numCols\":", icon.ncol), paste("\"numRows\":", icon.nrow))
    if (any(is.na(icon.nrow)) ||
        any(icon.nrow * icon.ncol != total.icons))
    {
        if (all(!is.na(icon.nrow)) && any(icon.nrow != 1))
            total.icons  =  ceiling(icon.nrow * icon.ncol)
        else
            icon.nrow  =  ceiling(total.icons/icon.ncol)
    }

    if (length(total.icons) != 1 && length(unlist(total.icons)) != length(unlist(x)) &&
        length(total.icons) != n && length(total.icons) != m)
        stop("total.icons does not match dimensions of x\n")
    if (length(icon.nrow) != 1 && length(unlist(icon.nrow)) !=  length(unlist(x)) &&
        length(icon.nrow) != n && length(icon.nrow) !=  m)
        stop("icon.nrow does not match dimensions of x\n")
    if (length(icon.ncol) !=  1 && length(unlist(icon.ncol)) !=  length(unlist(x)) &&
        length(icon.ncol) != n && length(icon.ncol) !=  m)
        stop("icon.ncol does not match dimensions of x\n")
    if (pad.icon.row < 0 || pad.icon.row >= 1)
        stop("pad.icon.row must be smaller than 1 and greater or equal to 0\n")
    if (pad.icon.col < 0 || pad.icon.col >= 1)
        stop("pad.icon.col must be smaller than 1 and greater or equal to 0\n")

    # Try column-first order first (i.e. each entry of total.icons to one row)
    byrow  =  (length(total.icons)!= n && length(unlist(total.icons)) !=  length(unlist(x)) && !is.data.frame(total.icons))
    total.icons <- matrix(unlist(total.icons), nrow = n, ncol = m, byrow = byrow)
    icon.nrow <- matrix(icon.nrow, nrow = n, ncol = m,
                        byrow = (length(icon.nrow) != n && length(unlist(icon.nrow)) != length(unlist(x)) &&
                                 !is.data.frame(icon.nrow)))
    icon.ncol <- matrix(icon.ncol, nrow = n, ncol = m,
                        byrow = (length(icon.ncol) != n && length(unlist(icon.ncol)) != length(unlist(x)) &&
                                 !is.data.frame(icon.ncol)))
    prop <- as.vector(unlist(x))/unlist(total.icons)

    if (all(total.icons == 0))
        stop("No non-zero entries for total.icons\n")
    prop[total.icons == 0] <- 0
    if (any(is.na(prop)) || any(prop > 1) || any(prop < 0))
        stop("x must be a number between 0 and total.icons\n")

    # Fill row/column labels with defaults
    if (!is.null(label.left) && is.na(label.left) && m == 1 && is.null(row.names(x)) && !is.null(names(x)))
        label.left <- names(x)
    if (!is.null(label.left) && all(is.na(label.left)) && !is.null(rownames(x)))
        label.left <- rownames(x)
    if (!is.null(label.top) && all(is.na(label.top)) && !is.null(colnames(x)))
        label.top <- colnames(x)
    if (!is.null(label.right) && all(is.na(label.right)))
        label.right <- NULL
    if (!is.null(label.bottom) && all(is.na(label.bottom)))
        label.bottom <- NULL
    if (!is.null(label.right) && all(is.na(label.right)))
        label.right <- NULL

    if (!is.null(label.left) && all(is.na(label.left)))
        label.left <- NULL
    if (!is.null(label.top) && all(is.na(label.top)))
        label.top <- NULL

    if (!is.null(sublabel.left) && all(is.na(sublabel.left)))
        sublabel.left <- NULL
    if (!is.null(sublabel.right) && all(is.na(sublabel.right)))
        sublabel.right <- NULL

    if (!is.null(label.left) && length(label.left) != n)
        stop("label.left must be of length ", n, "\n")
    if (!is.null(label.right) && length(label.right) != n)
        stop("label.right must be of length ", n, "\n")
    if (!is.null(label.top) && length(label.top) != m)
        stop("label.top must be of length ", m, "\n")
    if (!is.null(label.bottom) && length(label.bottom) != m)
        stop("label.bottom must be of length ", m, "\n")

    if (length(row.height) !=  1 && length(row.height) !=  n)
        stop("row.height must be of length 1 or ", n, "\n")
    if (length(column.width) !=  1 && length(column.width) != m)
        stop ("column.width must be of length 1 or ", m, "\n")


    # To check: fill.direction, images, alignments,
    # label.data.position, image.type

    fill.icon.color.str <- ifelse(nchar(fill.icon.color) > 0, paste(fill.icon.color, ":", sep = ""), "")
    base.image.str <- ""
    if (any(!is.na(base.image)))
    {
        base.icon.color.str <- ifelse(nchar(base.icon.color) > 0, paste(base.icon.color, ":", sep = ""), "")
        base.image.str <- ifelse(!is.na(base.image), paste("\"baseImage\":\"", image.type, ":", base.icon.color.str, base.image, "\",", sep = ""), "")
    }

    # Calculate size of table cells
    # The units should roughly be equal to 1 px = 1/72 inch
    # Note that icons will resize automatically to fill table cell
    # But label font sizes are in fixed px
    if (is.null(label.left))
        label.left.font.size <- 0
    if (is.null(sublabel.left))
        sublabel.left.font.size <- 0
    if (is.null(label.right))
        label.right.font.size <- 0
    if (is.null(sublabel.right))
        sublabel.right.font.size <- 0
    if (is.null(label.top))
        label.top.font.size <- 0
    if (is.null(label.bottom))
        label.bottom.font.size <- 0
    if (!show.label.data || !(label.data.position %in% c("header","footer")))
        label.data.font.size <- 0

    max.font.size <- max(label.left.font.size + sublabel.left.font.size,
                         label.right.font.size + sublabel.right.font.size, 1)
    size.warning <- 0   # tracker so we only give one size warning


    # Fix minimum size of label cells
    if (is.na(label.left.width))
        label.left.width <- font.whratio * max(0, c(label.left.font.size * nchar(label.left),
                                                 sublabel.left.font.size * nchar(sublabel.left)), na.rm=T)
    if (is.na(label.right.width))
        label.right.width <- font.whratio * max(0, c(label.right.font.size * nchar(label.right),
                                                 sublabel.right.font.size * nchar(sublabel.right)), na.rm=T)
    legend.width <- 0
    if (show.legend)
        legend.width <- pad.legend + font.whratio*legend.font.size*nchar(legend.text)
    tot.side.widths <- label.left.width + label.right.width + legend.width
    if (!is.na(graphic.width.inch) && tot.side.widths > 72 * graphic.width.inch)
    {
        size.warning <- 1
        warning("Pictograph is wider than the window. Consider increasing the image width, reducing label lengths, or decreasing font size\n")
    }

    if(is.na(label.top.height))
        label.top.height <- label.top.font.size*1.0
    if(is.na(label.bottom.height))
        label.bottom.height <- label.bottom.font.size*1.0

    # Calculate size of chart - icons are adjusted to fill the space
    # described by row.height x column.width per data.cell
    # Excludes row padding and header/footer text
    if (!is.na(graphic.width.inch) && !is.na(graphic.height.inch) && (is.na(column.width) || is.na(row.height)))
    {
        chart.width <- max(10, graphic.width.inch * 72 - tot.side.widths, na.rm=T)
        chart.height <- max(10, graphic.height.inch * 72
                                - (label.top.height + label.bottom.height)
                                - ((n-1)/n)*pad.row
                                - n * label.data.font.size, na.rm=T)

        column.width.max <- chart.width/m   # maximum column width
        icon.width <- column.width.max/max(icon.ncol) * (1 - pad.icon.col)

        h1 <- chart.height/n
        h2 <- icon.width/width.height.ratio * max(icon.nrow)/(1 - pad.icon.row)

        if (h1 < h2)
        {
            # Height constrained
            row.height <- h1
            icon.height <- row.height/max(icon.nrow) * (1 - pad.icon.row)
            icon.width <- width.height.ratio * icon.height
            column.width <- icon.width * max(icon.ncol)/(1 - pad.icon.col)
            extra.width <- chart.width - (column.width * m)

            ifelse(!is.null(label.left), label.left.width <- label.left.width + extra.width,
                                         label.right.width <- label.right.width + extra.width)
            #cat("Height constrained", column.width, row.height, "\n")
        } else
        {
            # Width constrained
            row.height <- h2
            column.width <- column.width.max
            #cat("Width constrained", column.width, row.height, "\n")
        }

        # Very constrained case where icons are smaller than text (width-constrained)
        if (row.height < max.font.size)
        {
            row.height <- max.font.size
            column.width <- column.width.max

            #cat("Very constrained case", column.width, row.height, "\n")
        }
    } else
    {
         # Default chart dimensions without graphic sizes
         if (is.na(row.height))
            row.height <- max(15*max(icon.nrow), 1.2 * max.font.size)
         if (is.na(column.width))
            column.width <- row.height[1]*max(icon.ncol)/max(icon.nrow)*max(1, width.height.ratio,na.rm=T)
                                #font.whratio*label.top.font.size*nchar(label.top),
                                #font.whratio*label.bottom.font.size*nchar(label.bottom))
    }
    if (length(row.height) == 1)
        row.height <- rep(row.height, n)
    if (length(column.width) == 1)
        column.width <- rep(column.width, m)
    icon.width <- min(column.width)/max(icon.ncol) * (1 - pad.icon.col)
    icon.height <- min(row.height)/max(icon.nrow) * (1 - pad.icon.row)
    if (!is.na(width.height.ratio))
        icon.height <- icon.width/width.height.ratio

    # Calculating padding/alignment
    pad.left <- matrix(0, n, m)
    pad.right <- matrix(0, n, m)
    pad.top <- matrix(0, n, m)
    pad.bottom <- matrix(0, n, m)

    # Compensating for rowGutters/pad.row
    # This additional padding is required to ensure that all rowheights are the same and
    # rowlabels on the top and bottom of the table remain vertically centered
    # It also ensures that lines are visible at the top and bottom of the table
    lab.tpad <- rep(0, n)
    lab.bpad <- rep(0, n)
    if (length(label.top) == 0 || all(nchar(label.top) == 0))
    {
        pad.top[1,] <- pad.top[1,] + pad.row/2
        row.height[1] <- row.height[1] + pad.row/2
        lab.tpad[1] <- pad.row/2
    }
    if (length(label.bottom)  ==  0 || all(nchar(label.bottom) == 0))
    {
        pad.bottom[n,] <- pad.bottom[n,] + pad.row/2
        row.height[n] <- row.height[n] + pad.row/2
        lab.bpad[n] <- pad.row/2
    }

    # Preparing labels
    # Dimensions of all table components are expected to be fixed by now
    if (length(label.top) > 0)
        label.top.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\", \"horizontal-align\":\"%s\", \"vertical-align\":\"%s\"}}",
                         label.top, label.top.font.family, label.top.font.size, label.top.font.weight,
                         label.top.font.color, label.top.align.horizontal, label.top.align.vertical)
    if (length(label.bottom) > 0)
        label.bottom.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\",\"horizontal-align\":\"%s\", \"vertical-align\":\"%s\"}}",
                         label.bottom, label.bottom.font.family, label.bottom.font.size, label.bottom.font.weight,
                         label.bottom.font.color, label.bottom.align.horizontal, label.bottom.align.vertical)
    if (length(label.left) > 0 || length(sublabel.left) > 0)
    {
        text.str <- ""
        config1.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               label.left.font.size, label.left.font.weight, label.left.align.horizontal)
        config2.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               sublabel.left.font.size, sublabel.left.font.weight, sublabel.left.align.horizontal)
        config12.str <- sprintf("\"font-family\":\"%s\",\"font-color\":\"%s\"",
                              label.left.font.family, label.left.font.color)

        if (length(label.left) > 0 && length(sublabel.left) == 0)
            text.str <- paste("\"text\":\"", label.left, "\",", config1.str, ",", config12.str, sep="")
        if (length(label.left) == 0 && length(sublabel.left) > 0)
            text.str <- paste("\"text\":\"", sublabel.left, "\",", config2.str,  ",", config12.str, sep="")
        if (length(label.left) > 0 && length(sublabel.left) > 0)
            text.str <- sprintf("\"labels\": [{\"text\":\"%s\", %s, %s},{\"text\": \"%s\", %s, %s}]",
                                label.left, config1.str, config12.str, sublabel.left, config2.str, config12.str)

        label.left.str <- sprintf("{\"type\":\"label\", \"value\":{\"padding-right\":%f,
                                    \"padding-top\":%f, \"padding-bottom\":%f, \"vertical-align\":\"%s\", %s}}",
                         label.left.pad, lab.tpad, lab.bpad, label.left.align.vertical, text.str)
    }
    if (length(label.right) > 0 || length(sublabel.right) > 0)
    {
        text.str <- ""
        config1.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               label.right.font.size, label.right.font.weight, label.right.align.horizontal)
        config2.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               sublabel.right.font.size, sublabel.right.font.weight, sublabel.right.align.horizontal)
        config12.str <- sprintf("\"font-family\":\"%s\",\"font-color\":\"%s\"",
                              label.right.font.family, label.right.font.color)

        if (length(label.right) > 0 && length(sublabel.right) == 0)
            text.str <- paste("\"text\":\"", label.right, "\",", config1.str, ",", config12.str, sep="")
        if (length(label.right) == 0 && length(sublabel.right) > 0)
            text.str <- paste("\"text\":\"", sublabel.right, "\",", config2.str,  ",", config12.str, sep="")
        if (length(label.right) > 0 && length(sublabel.right) > 0)
            text.str <- sprintf("\"labels\": [{\"text\":\"%s\", %s, %s},{\"text\": \"%s\", %s, %s}]",
                                label.right, config1.str, config12.str, sublabel.right, config2.str, config12.str)

        label.right.str <- sprintf("{\"type\":\"label\", \"value\":{\"padding-left\":%f,
                                     \"padding-top\":%f, \"padding-bottom\":%f, \"vertical-align\":\"%s\", %s}}",
                         label.right.pad, lab.tpad, lab.bpad, label.right.align.vertical, text.str)
    }

    # Preparing data labels
    label.data.str <- ""
    if (show.label.data)
    {
        label.data.str <- sprintf("\"text-%s\":{\"text\":\"%s\", \"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                             \"font-family\":\"%s\", \"font-color\":\"%s\", \"horizontal-align\":\"%s\"},",
                            label.data.position, label.data.text,
                            label.data.font.size, label.data.font.weight, label.data.font.family,
                            label.data.font.color, label.data.align.horizontal)
        row.height <- row.height + label.data.font.size
    }

    label.float.str <- ""
    if (show.label.float)
    {
        #cat("pictochart line 458\n")
        if (any(x >= total.icons))
            warning("Floating labels placed at invalid positions. Please increase total.icons\n")

        x.pos <- ceiling(x)
        label.float.position <- sprintf("%d:%d", floor(x.pos/icon.ncol), x.pos %% icon.ncol)
        label.float.str <- sprintf("\"floatingLabels\":[{\"position\":\"%s\", \"text\":\"%s\",
                            \"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                            \"font-family\":\"%s\", \"font-color\":\"%s\",
                            \"horizontal-align\":\"%s\", \"vertical-align\":\"%s\"}],",
                            label.float.position, label.float.text,
                            label.float.font.size, label.float.font.weight, label.float.font.family,
                            label.float.font.color, label.float.align.horizontal, label.float.align.vertical)
        #cat("label.float.str:", label.float.str, "\n")
    }

    row.str <- sprintf("{\"type\":\"graphic\", \"value\":{\"proportion\":%f,\"numImages\":%d,
                         \"variableImage\":\"%s:%s%s:%s\", %s %s, %s %s
                        \"columnGutter\":%f, \"rowGutter\":%f, \"padding\":\"%f %f %f %f\"}}",
                        prop, total.icons, image.type, fill.icon.color.str, fill.direction,
                        fill.image, base.image.str, layout.str, label.data.str, label.float.str,
                        pad.icon.col, pad.icon.row, pad.top, pad.right, pad.bottom, pad.left)
    row.str <- matrix(row.str, n, m)

    # Adding left/right labels
    empty.str <- "{\"type\":\"label\", \"value\":{\"text\":\"\"}}"
    corner.tl <- NULL
    corner.tr <- NULL
    corner.bl <- NULL
    corner.br <- NULL

    if (length(label.left) > 0 || length(sublabel.left) > 0)
    {
        row.str <- cbind(label.left.str, row.str)
        corner.tl <- empty.str
        corner.bl <- empty.str
        column.width <- c(label.left.width, column.width)
    }
    if (length(label.right) > 0 || length(sublabel.right) > 0)
    {
        row.str <- cbind(row.str, label.right.str)
        corner.tr <- empty.str
        corner.br <- empty.str
        column.width <- c(column.width, label.right.width)
    }


    # Adding legend
    leg.rpad <- 0
    if (nchar(legend.text) == 0)
        legend.text <- " "
    if (show.legend)
    {
        row.str <- cbind(row.str, matrix(empty.str, nrow(row.str), 3))
        leg.row <- ceiling(nrow(row.str)/2)
        leg.col <- ncol(row.str)
        legend.col.str <- ""
        if (nchar(legend.icon.color) > 0)
            legend.col.str <- paste(legend.icon.color[1], ":", sep="")

        leg.vpad <- (row.height[leg.row]-icon.height)/2
        row.str[leg.row, leg.col] <-  sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",\"font-family\":\"%s\",
                                                \"font-size\":\"%fpx\",\"font-weight\":\"%s\",\"font-color\":\"%s\",
                                                \"horizontal-align\":\"left\", \"vertical-align\":\"center\"}}",
                                                legend.text, legend.font.family, legend.font.size, legend.font.weight, legend.font.color)
        row.str[leg.row, leg.col-1] <- sprintf("{\"type\":\"graphic\", \"value\":{\"proportion\":1,\"numImages\":1,
                         \"variableImage\":\"%s:%s:%s%s\", \"padding\":\"%f %f %f %f\"}}",
                                               image.type, legend.col.str, fill.direction[1], fill.image[1],
                                               leg.vpad, 0, leg.vpad, 0)
        column.width <- c(column.width, pad.legend, icon.width, font.whratio*legend.font.size*nchar(legend.text))
        leg.rpad <- sum(tail(column.width, 3))
    }

    # Adding lines to make table
    lines.str <- ""
    legendgap.str <- ""
    if (show.lines)
    {
        if (show.legend)
            legendgap.str <- paste("\"padding-right\":", 3*pad.col+leg.rpad, ",")
        lines.str <- paste("\"lines\":{\"horizontal\":[", paste((0:n)+any(nchar(label.top)>0), collapse = ","), "],",
                           legendgap.str,
                           "\"style\": \"stroke:", line.color, ";stroke-width:", line.width, "\"}, ", sep = "")
    }


    # Adding top/bottom labels
    if (any(nchar(label.top) > 0))
        row.height <- c(label.top.height, row.height)
    if (any(nchar(label.bottom) > 0))
        row.height <- c(row.height, label.bottom.height)
    row.height <- pmax(1, row.height)
    column.width <- pmax(1, column.width)
    row.str <- apply(row.str, 1, paste, collapse = ",")
    graphic.width <- sum(column.width+pad.col) - pad.col
    graphic.height <- sum(row.height+pad.row) - pad.row

    # Check if pictograph output fits within graphic dimensions
    #cat("final:", graphic.width, graphic.height, "\n")
    if (!is.na(graphic.width.inch) && !is.na(graphic.width.inch))
    {
        if (!size.warning && graphic.width > graphic.width.inch * 72 * 1.1)
        {
            size.warning <- 1
            warning("Pictograph is wider than size of window. Consider increasing image width or reducing the number of columns\n")
        }

        if (!size.warning && graphic.height > graphic.height.inch * 72 * 1.1)
            warning("Pictograph is larger than the size of the window. Consider increasing the image height, reducing the number of rows or decreasing the font size\n")
    }

    json.str <- paste("{\"width\":", graphic.width, ", \"height\":", graphic.height, ",",
             "\"background-color\":\"", background.color, "\",",
             "\"table\":{\"rowHeights\":[", paste(row.height, collapse = ","), "],",
             #"\"padding-top\":", margin.top, ",\"padding-right\":", margin.right, ",",
             #"\"padding-bottom\":", margin.bottom, ",\"padding-left\":", margin.left, ",",
             #"\"padding\":\"", paste(margin.top, margin.right, margin.bottom, margin.left, sep = " "), "\",",
             "\"rowGutterLength\":", pad.row, ",\"columnGutterLength\":", pad.col, ",",
             "\"colWidths\":[", paste(column.width, collapse = ","), "],",
             sep = "")
    json.str <- paste(json.str, lines.str, "\"rows\":[[", sep = "")
    if (any(nchar(label.top) > 0))
        json.str <- paste(json.str, paste(c(corner.tl, label.top.str, corner.tr), collapse = ","), "],[", sep = "")
    json.str <- paste(json.str, paste(row.str, collapse = "],["), sep = "")
    if (any(nchar(label.bottom) > 0))
        json.str <- paste(json.str, "],[", paste(c(corner.bl, label.bottom.str, corner.br), collapse = ","), sep = "")
    json.str <- paste(json.str, "]]}}", sep = "")

    if (print.config)
        cat(json.str, "\n")
    graphic(json.str)
}
