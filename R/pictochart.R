#' PictoChart
#'
#' Function to create a chart of Pictographs
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
#' @param label.left Length must be equal to length (if \code{x} is a vector) or number of rows (if \code{x} is matrix or data.frame) as x. If no value is supplied, labels will be read from names/rowname of \code{x}. To suppress labels, use \code{label.left  =  rep("", length(x))}.
#' @param label.top By default, labels are read from column names of \code{x}.
#' @param label.bottom Optional labels below graphic cells. The length of the labels must be the same as the number of columns in \code{x}.
#' @param label.right Optional labels to the right of graphic cells. The length of the labels must be the same as the number of rows in \code{x}.
#' @param label.font.family Controls font-family of all labels. To modify only the font of one label use \code{label.left.font, label.top.font}, etc.
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
#' @param pad.icon.ncol Spacing between horizontal spacing between icons inside each table cell.
#'
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
                       #icon.fixedsize = FALSE,
                       #icon.align.horizontal = "left",
                       #icon.align.vertical = "center",
                       label.left = NA,
                       label.right = NA,
                       label.top = NA,
                       label.bottom = NA,
                       label.left2 = NA,
                       label.right2 = NA,
                       label.left.pad = 0,       # padding between label and icon
                       label.right.pad = 0,
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
                       label.left2.font.size = label.left.font.size,
                       label.left2.font.weight = label.left.font.weight,
                       label.left2.align.horizontal = label.left.align.horizontal,
                       label.right2.font.size = label.right.font.size,
                       label.right2.font.weight = label.right.font.weight,
                       label.right2.align.horizontal = label.right.align.horizontal,
                       legend.font.family = label.font.family,
                       legend.font.size = 0.8*label.font.size,
                       legend.font.weight = "normal",
                       legend.font.color = label.font.color,
                       legend.icon.color = fill.icon.color[1],
                       label.data.type = "none",
                       label.data.text = NULL,
                       label.data.position = "footer",
                       label.data.font.family = label.font.family,
                       label.data.font.size = 0.8*label.font.size,
                       label.data.font.weight = "normal",
                       label.data.font.color = label.font.color,
                       label.data.align.horizontal = "right",
                       row.height = 1.5*label.font.size*max(icon.nrow),
                       column.width = max(15*max(icon.ncol), 0.5*label.top.font.size*nchar(label.top), 0.5*label.bottom.font.size*nchar(label.bottom)),
                       width.height.ratio = NA,
                       background.color = "transparent",
                       line.color = "#A8A8A8",
                       line.width = 0.5,
                       pad.legend = 0.5*column.width[1],
                       pad.row = 5,
                       pad.col = 5,
                       pad.icon.row = 0.0,
                       pad.icon.col = 0.0,
                       #margin.top = 0,
                       #margin.right = 0,
                       #margin.bottom = 0,
                       #margin.left = 0,
                       print.config = FALSE)
{
    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x)) || is.na(ncol(x))) 1
         else ncol(x)

    if (any(total.icons != ceiling(total.icons)))
        stop("Parameter total.icons must be a whole number\n")
    if (any(total.icons <= 0))
        stop("Parameter total.icons must be greater than zero\n")

    if (any(is.na(icon.nrow)) || any(icon.nrow * icon.ncol != total.icons))
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

    if (!is.null(label.left2) && all(is.na(label.left2)))
        label.left2 <- NULL
    if (!is.null(label.right2) && all(is.na(label.right2)))
        label.right2 <- NULL

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

    if (length(row.height) == 1)
        row.height <- rep(row.height, n)
    if (length(column.width) == 1)
        column.width <- rep(column.width, m)

    if(is.na(label.top.height))
        label.top.height = label.top.font.size*2
    if(is.na(label.bottom.height))
        label.bottom.height = label.bottom.font.size*2

    # To check: fill.direction, images, alignments,
    # label.data.type, label.data.position, image.type

    fill.icon.color.str <- ifelse(nchar(fill.icon.color) > 0, paste(fill.icon.color, ":", sep = ""), "")
    base.image.str <- ""
    if (any(!is.na(base.image)))
    {
        base.icon.color.str <- ifelse(nchar(base.icon.color) > 0, paste(base.icon.color, ":", sep = ""), "")
        base.image.str <- ifelse(!is.na(base.image), paste("\"baseImage\":\"", image.type, ":", base.icon.color.str, base.image, "\",", sep = ""), "")
    }

    # Calculating padding/alignment
    pad.left <- matrix(0, n, m)
    pad.right <- matrix(0, n, m)
    pad.top <- matrix(0, n, m)
    pad.bottom <- matrix(0, n, m)
    icon.width <- min(column.width/icon.ncol)
    icon.height <- min(row.height/icon.nrow)

    # Padding should not affect size of the icons
    if (!is.na(width.height.ratio) && width.height.ratio > 0)
    {
        icon.width <- max(icon.width, width.height.ratio * icon.height)
        icon.height <- icon.width/width.height.ratio
        column.width <- rep((max(icon.ncol) * icon.width * 1/(1-pad.icon.col)), m)
        row.height <-   rep((max(icon.nrow) * icon.height * 1/(1-pad.icon.row)), n)
    }

    # Compensating for rowGutters/pad.row
    # This additional padding is required to ensure that all rowheights are the same and
    # rowlabels on the top and bottom of the table remain vertically centered
    # It also ensures that table lines show top and bottom lines
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
    if (length(label.left) > 0 || length(label.left2) > 0)
    {
        text.str <- ""
        config1.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               label.left.font.size, label.left.font.weight, label.left.align.horizontal)
        config2.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               label.left2.font.size, label.left2.font.weight, label.left2.align.horizontal)
        config12.str <- sprintf("\"font-family\":\"%s\",\"font-color\":\"%s\"",
                              label.left.font.family, label.left.font.color)

        if (length(label.left) > 0 && length(label.left2) == 0)
            text.str <- paste("\"text\":\"", label.left, "\",", config1.str, ",", config12.str, sep="")
        if (length(label.left) == 0 && length(label.left2) > 0)
            text.str <- paste("\"text\":\"", label.left2, "\",", config2.str,  ",", config12.str, sep="")
        if (length(label.left) > 0 && length(label.left2) > 0)
            text.str <- sprintf("\"labels\": [{\"text\":\"%s\", %s, %s},{\"text\": \"%s\", %s, %s}]",
                                label.left, config1.str, config12.str, label.left2, config2.str, config12.str)

        label.left.str <- sprintf("{\"type\":\"label\", \"value\":{\"padding-right\":%f,
                                    \"padding-top\":%f, \"padding-bottom\":%f, \"vertical-align\":\"%s\", %s}}",
                         label.left.pad, lab.tpad, lab.bpad, label.left.align.vertical, text.str)
    }
    if (length(label.right) > 0 || length(label.right2) > 0)
    {
        text.str <- ""
        config1.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               label.right.font.size, label.right.font.weight, label.right.align.horizontal)
        config2.str  <- sprintf("\"font-size\": \"%fpx\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"",
                               label.right2.font.size, label.right2.font.weight, label.right2.align.horizontal)
        config12.str <- sprintf("\"font-family\":\"%s\",\"font-color\":\"%s\"",
                              label.right.font.family, label.right.font.color)

        if (length(label.right) > 0 && length(label.right2) == 0)
            text.str <- paste("\"text\":\"", label.right, "\",", config1.str, ",", config12.str, sep="")
        if (length(label.right) == 0 && length(label.right2) > 0)
            text.str <- paste("\"text\":\"", label.right2, "\",", config2.str,  ",", config12.str, sep="")
        if (length(label.right) > 0 && length(label.right2) > 0)
            text.str <- sprintf("\"labels\": [{\"text\":\"%s\", %s, %s},{\"text\": \"%s\", %s, %s}]",
                                label.right, config1.str, config12.str, label.right2, config2.str, config12.str)

        label.right.str <- sprintf("{\"type\":\"label\", \"value\":{\"padding-left\":%f,
                                     \"padding-top\":%f, \"padding-bottom\":%f, \"vertical-align\":\"%s\", %s}}",
                         label.right.pad, lab.tpad, lab.bpad, label.right.align.vertical, text.str)
    }

    # Preparing data labels
    label.data.str <- ""
    if (label.data.type != "none")
    {
        # PictoStdChart always passes in a raw text vector
        #if (label.data.type == "count")
        #    label.data.text <- as.character(unlist(x))
        #
        #if (label.data.type %in% c("proportion", "percentage"))
        #    label.data.text <- label.data.type

        label.data.str <- sprintf("\"text-%s\":{\"text\":\"%s\", \"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                             \"font-family\":\"%s\", \"font-color\":\"%s\", \"horizontal-align\":\"%s\"},",
                            label.data.position, label.data.text,
                            label.data.font.size, label.data.font.weight, label.data.font.family,
                            label.data.font.color, label.data.align.horizontal)
        row.height <- row.height + label.data.font.size
    }

    row.str <- sprintf("{\"type\":\"graphic\", \"value\":{\"proportion\":%f,\"numImages\":%d,
                         \"variableImage\":\"%s:%s%s:%s\", %s \"numRows\":%d, %s
                        \"columnGutter\":%f, \"rowGutter\":%f, \"padding\":\"%f %f %f %f\"}}",
                        prop, total.icons, image.type, fill.icon.color.str, fill.direction,
                        fill.image, base.image.str, icon.nrow, label.data.str,
                        pad.icon.col, pad.icon.row, pad.top, pad.right, pad.bottom, pad.left)
    row.str <- matrix(row.str, n, m)

    # Adding left/right labels
    empty.str <- "{\"type\":\"label\", \"value\":{\"text\":\"\"}}"
    corner.tl <- NULL
    corner.tr <- NULL
    corner.bl <- NULL
    corner.br <- NULL

    if (length(label.left) > 0 || length(label.left2) > 0)
    {
        row.str <- cbind(label.left.str, row.str)
        corner.tl <- empty.str
        corner.bl <- empty.str
        if (is.na(label.left.width))
            label.left.width <- 0.6 * max(c(label.left.font.size * nchar(label.left), label.left2.font.size * nchar(label.left2)))
        column.width <- c(label.left.width, column.width)
    }
    if (length(label.right) > 0 || length(label.right2) > 0)
    {
        row.str <- cbind(row.str, label.right.str)
        corner.tr <- empty.str
        corner.br <- empty.str
        if (is.na(label.right.width))
            label.right.width <- 0.6 * max(c(label.right.font.size * nchar(label.right), label.right2.font.size * nchar(label.right2)))
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
        column.width <- c(column.width, pad.legend, icon.width, legend.font.size*nchar(legend.text))
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
    json.str <- paste("{\"width\":", sum(column.width+pad.col)-pad.col, ", \"height\":", sum(row.height+pad.row)-pad.row, ",",
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
