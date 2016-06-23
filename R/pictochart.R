#' PictoChart
#'
#' Function to create a chart of Pictographs
#'
#' @param x Data for charting
#' @param variable.image
#' @param base.image Option URL of background image
#' @param K Maximum icons in each table cell. Ignored if both \code{icon.nrow} and \code{icon.ncol} supplied.
#' @param direction Accepts \code{horizontal}, \code{vertical}, \code{radial}, \code{scale}. (But \code{scale} may not be appropriate, especially if \code{K} is important).
#' @param show.table
#' @param icon.nrow
#' @param icon.ncol
#' @param icon.fixedsize When \code{true}, icons will not automatically resize to fill table cell.
#' @param icon.halign Horizontal alignment of icons in cell when \code{icon.fixedsize} is \code{true}. Accepts values of left, right or center both as scalar and vectors.
#' @param label.left Length must be equal to length (if \code{x} is a vector) or number of rows (if \code{x} is matrix or data.frame) as x. If no value is supplied, labels will be read from names/rowname of \code{x}. To suppress labels, use \code{label.left = rep("", length(x))}.
#' @param label.top By default, labels are read from column names of \code{x}.
#' @param label.bottom
#' @param label.right
#' @param label.font Controls font-family of all labels. To modify only the font of one label use \code{label.left.font}.
#' @param label.size
#' @param label.weight
#' @param label.color
#' @param label.top.height
#' @param label.left.width
#' @param label.left.halign
#' @param label.left.valign
#' @param row.height Height of graphic cells. Can be a single value or a numeric vector the same length as the number of rows in \code{x}.
#' @param column.width Width of graphic cells.
#' @param pad.row Single numeric specifying spacing between graphic cells in the table.
#' @param pad.col
#' @param pad.icon.row Numeric specifying spacing between icons inside each table cell. May be a single value or a numeric matrix of the same dimensions as \code{x}.
#' @param pad.icon.ncol
#'
#' @importFrom  rhtmlPictographs graphic
#' @export

PictoChart <- function( x,
                        variable.image,
                        base.image="",
                        K=max(ceiling(x)),
                        direction="horizontal",
                        show.lines=FALSE,
                        show.legend=FALSE,
                        legend.text="",
                        icon.nrow=1,
                        icon.ncol=unlist(K)/icon.nrow,
                        icon.fixedsize=FALSE,
                        icon.halign="left",
                        label.left=c(),
                        label.right=c(),
                        label.top=c(),
                        label.bottom=c(),
                        #label.overlay=c(),    # not implemented
                        #label.footer=c(),
                        #label.header=c(),
                        label.font="arial",
                        label.size=10,
                        label.weight="normal",
                        label.color="#2C2C2C",
                        label.top.font=label.font,
                        label.top.size=label.size,
                        label.top.weight=label.weight,
                        label.top.color=label.color,
                        label.top.height=1.2*label.top.size,
                        label.right.font=label.font,
                        label.right.size=label.size,
                        label.right.weight=label.weight,
                        label.right.color=label.color,
                        label.right.width=0,
                        label.bottom.font=label.font,
                        label.bottom.size=label.size,
                        label.bottom.weight=label.weight,
                        label.bottom.color=label.color,
                        label.bottom.height=1.2*label.bottom.size,
                        label.left.font=label.font,
                        label.left.size=label.size,
                        label.left.weight=label.weight,
                        label.left.color=label.color,
                        label.left.width=0,
                        label.left.halign="right",
                        label.right.halign="left",
                        label.top.halign="left",
                        label.bottom.halign="left",
                        label.left.valign="center",
                        label.right.valign="center",
                        label.top.valign="left",
                        label.bottom.valign="center",
                        legend.font=label.font,
                        legend.size=0.8*label.size,
                        legend.weight=label.weight,
                        legend.color=label.color,
                        row.height=25*max(icon.nrow),
                        column.width=max(20*max(icon.ncol), 0.5*label.top.size*nchar(label.top)),
                        line.color="#A8A8A8",
                        line.width=0.5,
                        pad.row = 10,
                        pad.col = 10,
                        pad.icon.row=0.0,
                        pad.icon.col=0.0)
{
    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x))) 1
         else ncol(x)

    if (any(icon.nrow * icon.ncol != K))
    {
        if (any(icon.nrow != 1))
            K = ceiling(icon.nrow * icon.ncol)
        else
            icon.nrow = ceiling(K/icon.ncol)
    }

    if (length(K) != 1 && length(unlist(K)) != length(unlist(x)) && length(K) != n && length(K) != m)
        stop("K does not match dimensions of x\n")
    if (length(icon.nrow) != 1 && length(unlist(icon.nrow)) != length(unlist(x)) &&
        length(icon.nrow) != n && length(icon.nrow) != m)
        stop("icon.nrow does not match dimensions of x\n")
    if (length(icon.ncol) != 1 && length(unlist(icon.ncol)) != length(unlist(x)) &&
        length(icon.ncol) != n && length(icon.ncol) != m)
        stop("icon.ncol does not match dimensions of x\n")

    K <- matrix(unlist(K), nrow=n, ncol=m, byrow=(length(K)==m && !is.data.frame(K)))
    icon.nrow <- matrix(icon.nrow, nrow=n, ncol=m, byrow=(length(icon.nrow)==m && !is.data.frame(icon.nrow)))
    icon.ncol <- matrix(icon.ncol, nrow=n, ncol=m, byrow=(length(icon.ncol)==m && !is.data.frame(icon.ncol)))
    prop <- unlist(x)/unlist(K)
    if (any(is.na(prop)) || any(prop > 1) || any(prop < 0))
        stop("x must be a number between 0 and K\n")

    if (length(label.left) > 0 && length(label.left) != n)
        stop("label.left must be of length ", n, "\n")
    if (length(label.right) > 0 && length(label.right) != n)
        stop("label.right must be of length ", n, "\n")
    if (length(label.top) > 0 && length(label.top) != m)
        stop("label.top must be of length ", m, "\n")
    if (length(label.bottom) > 0 && length(label.bottom) != m)
        stop("label.bottom must be of length ", m, "\n")

    if (m==1 && !is.null(names(x)) && length(label.left)==0)
        label.left <- names(x)
    if (length(label.left)==0 & !is.null(rownames(x)))
        label.left <- rownames(x)
    if (length(label.top)==0 & !is.null(colnames(x)))
        label.top <- colnames(x)

    if (length(row.height) != 1 && length(row.height) != n)
        stop("row.height must be of length 1 or ", n, "\n")
    if (length(column.width) != 1 && length(column.width) != m)
        stop ("column.width must be of length 1 or ", m, "\n")
    if (length(row.height) == 1)
        row.height <- rep(row.height, n)
    if (length(column.width) == 1)
        column.width <- rep(column.width, m)

    # checking for lengths of fonts,spacing etc...
    dir.opt <- c("horizontal", "vertical", "radial", "scale")
    if (any(!direction %in% dir.opt))
        stop("direction must be one of ", paste(dir.opt, collapse=", "), "\n")

    base.image.str <- ""
    if (any(nchar(base.image) > 0))
        base.image.str <- paste("\"baseImage\":\"url:", base.image, "\",", sep="")
    #base.image <- "url:blue:https://s3-ap-southeast-2.amazonaws.com/kyle-public-numbers-assets/htmlwidgets/CroppedImage/stickman.svg"
    #variable.image <- "circle:red:horizontal"
    #variable.image <- "http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg"


    # Adding labels to the table
    lab.left.tpad <- (row.height-label.left.size)/2
    if (label.left.valign=="top")
        lab.left.tpad <- 0
    if (label.left.valign=="bottom")
        lab.left.tpad <- row.height - label.left.size

    lab.right.tpad <- (row.height-label.right.size)/2
    if (label.right.valign=="top")
        lab.right.tpad <- 0
    if (label.right.valign=="bottom")
        lab.right.tpad <- row.height - label.right.size

    lab.top.tpad <- (label.top.height-label.top.size)/2
    if (label.top.valign=="top")
        lab.top.tpad <- 0
    if (label.top.valign=="bottom")
        lab.top.tpad <- label.top.height - label.top.size

    lab.bottom.tpad <- (label.bottom.height-label.bottom.size)/2
    if (label.bottom.valign=="top")
        lab.bottom.tpad <- 0
    if (label.bottom.valign=="bottom")
        lab.bottom.tpad <- label.bottom.height - label.bottom.size

    if (length(label.top) > 0)
        label.top.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\", \"horizontal-align\":\"%s\", \"padding-top\":%f}}",
                         label.top, label.top.font, label.top.size, label.top.weight,
                         label.top.color, label.top.halign, lab.top.tpad)
    if (length(label.bottom) > 0)
        label.bottom.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\",\"horizontal-align\":\"%s\", \"padding-top\":%f}}",
                         label.bottom, label.bottom.font, label.bottom.size, label.bottom.weight,
                         label.bottom.color, label.bottom.halign, lab.bottom.tpad)
    if (length(label.right) > 0)
        label.right.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\", \"horizontal-align\":\"%s\", \"padding-top\":%f}}",
                         label.right, label.right.font, label.right.size, label.right.weight,
                         label.right.color, label.right.halign, lab.right.tpad)
    if (length(label.left) > 0)
        label.left.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%fpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\", \"horizontal-align\":\"%s\", \"padding-top\":%f}}",
                         label.left, label.left.font, label.left.size, label.left.weight,
                         label.left.color, label.left.halign, lab.left.tpad)

    pad.left=0
    pad.right=0
    pad.top=0
    pad.bottom=0
    if (icon.fixedsize)
    {
        icon.width <- min(column.width/icon.ncol)
        icon.height <- min(row.height/icon.nrow)
        icon.halign <- matrix(icon.halign, n, m, byrow=T)
        l.coef <- c(left=0, center=0.5, right=1)
        r.coef <- c(left=1, center=0.5, right=0)

        pad.left  <- l.coef[icon.halign] * (column.width - (icon.width)*icon.ncol)
        pad.right <- r.coef[icon.halign] * (column.width - (icon.width)*icon.ncol)
        pad.top <- 0.5 * (row.height - icon.height*icon.nrow)
        pad.bottom <- pad.top
    }

    row.str <- sprintf("{\"type\":\"graphic\", \"value\":{\"proportion\":%f,\"numImages\":%d,
                         \"variableImage\":\"%s:%s\", %s \"numRows\":%d,
                         \"columnGutter\":%f, \"rowGutter\":%f, \"padding\":\"%f %f %f %f\"}}",
                        prop, K, direction, variable.image, base.image.str, icon.nrow,
                        pad.icon.col, pad.icon.row, pad.top, pad.right, pad.bottom, pad.left)
    row.str <- matrix(row.str, n, m)
    corner.tl <- NULL
    corner.tr <- NULL
    corner.bl <- NULL
    corner.br <- NULL
    empty.str <- "{\"type\":\"label\", \"value\":{\"text\":\"\"}}"
    if (any(nchar(label.left) > 0))
    {
        row.str <- cbind(label.left.str, row.str)
        corner.tl <- empty.str
        corner.bl <- empty.str
        if (label.left.width == 0)
            label.left.width <- 0.75 * label.left.size * max(nchar(label.left))
        column.width <- c(label.left.width, column.width)
    }
    if (any(nchar(label.right) > 0))
    {
        row.str <- cbind(row.str, label.right.str)
        corner.tr <- empty.str
        corner.br <- empty.str
        if (label.right.width == 0)
            label.right.width <- label.right.size * max(nchar(label.right))
        column.width <- c(column.width, label.right.width)
    }
    if (any(nchar(label.top) > 0))
        row.height <- c(label.top.height, row.height)
    if (any(nchar(label.bottom) > 0))
        row.height <- c(row.height, label.bottom.height)

    # Adding legend
    leg.rpad <- 0
    if (show.legend)
    {
        row.str <- cbind(row.str, matrix(empty.str, nrow(row.str), 3))
        leg.row <- floor(nrow(row.str)/2)
        leg.col <- ncol(row.str)
        leg.tpad <- (row.height[leg.row]-legend.size)/2
        row.str[leg.row, leg.col] <-  sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",\"font-family\":\"%s\",
                                                \"font-size\":\"%fpx\",\"font-weight\":\"%s\",\"font-color\":\"%s\",
                                                \"horizontal-align\":\"left\", \"padding-top\":%f}}",
                                                legend.text, legend.font, legend.size, legend.weight, legend.color, leg.tpad)
        row.str[leg.row, leg.col-1] <- sprintf("{\"type\":\"graphic\", \"value\":{\"proportion\":1,\"numImages\":1,
                         \"variableImage\":\"%s:%s\"}}", direction, variable.image)
        column.width <- c(column.width, 10, 20, label.size*nchar(legend.text))
        leg.rpad <- sum(tail(column.width, 2))
    }

    # Adding lines to make table
    lines.str <- ""
    if (show.lines)
    lines.str <- paste("\"lines\":{\"horizontal\":[", paste((0:n)+any(nchar(label.top)>1), collapse=","), "],
                       \"padding-left\":", 0.0*column.width[1],", \"padding-right\":", 0.0*column.width[m]+3*pad.col+leg.rpad, ",",
                       "\"style\": \"stroke:", line.color, ";stroke-width:", line.width, "\"}, ", sep="")


    # Putting it all together
    row.str <- apply(row.str, 1, paste, collapse=",")
    json.str <- paste("{\"width\":", sum(column.width+pad.col), ", \"height\":", sum(row.height+pad.row), ",",
            "\"table\":{\"rowHeights\":[", paste(row.height, collapse=","), "],",
            #"\"padding-top\":", margin.top, ",\"padding-right\":", margin.right,
            #",\"padding-bottom\":", margin.bottom, ",\"padding-left\":", margin.left, ",",
            "\"rowGutterLength\":", pad.row, ",\"columnGutterLength\":", pad.col, ",",
            "\"colWidths\":[", paste(column.width, collapse=","), "],", sep="")
    json.str <- paste(json.str, lines.str, "\"rows\":[[", sep="")
    if (any(nchar(label.top) > 0))
        json.str <- paste(json.str, paste(c(corner.tl, label.top.str, corner.tr), collapse=","), "],[", sep="")
    json.str <- paste(json.str, paste(row.str, collapse="],["), sep="")
    if (any(nchar(label.bottom) > 0))
        json.str <- paste(json.str, "],[", paste(c(corner.bl, label.bottom.str, corner.br), collapse=","), sep="")
    json.str <- paste(json.str, "]]}}", sep="")
    graphic(json.str)
}
