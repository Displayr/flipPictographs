#' StarBarChart
#'
#' @param x Number of filled stars.
#' @param K Number of stars in each row.
#'
#' @importFrom  rhtmlPictographs graphic
#' @export

StarBarChart <- function(x,
                         K,
                         x.labels=NULL,
                         label.font="arial",
                         label.size=10,
                         label.weight="normal",
                         label.color="#2C2C2C")
{
    if (length(K) != 1)
        stop("K must be a single integer\n")
    if (!is.null(x.labels) && length(x.labels) != length(x))
        stop("labels must be the same length as x\n")
    if (is.null(x.labels))
        x.labels <- sprintf("Label %d", 1:length(x))
    if (any(is.na(x)) || any(x > K) || any(x < 0))
        stop("x must be a number between 0 and K\n")

    base.image <- "http://wiki.q-researchsoftware.com/images/f/f2/Star_unfilled.svg"
    variable.image <- "horizontal:http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg"

    n <- length(x)
    prop <- x/K
    label.width <- label.size*max(nchar(x.labels))
    star.wd <- 25
    graphic.width <- star.wd * K
    cell.height <- max(label.size, star.wd)
    cell.pad.top <- (cell.height - label.size + 2)/2
    #cell.pad.top <- (cell.height - label.size)

    row.str <- sprintf("{\"type\":\"label\", \"value\":{\"text\":\"%s\",
                         \"font-family\":\"%s\",\"font-size\":\"%dpx\",\"font-weight\":\"%s\",
                         \"font-color\":\"%s\", \"padding-top\":%f}},
                        {\"type\":\"graphic\", \"value\":{\"percentage\":%f,\"numImages\":%d,
                         \"baseImage\":\"%s\", \"variableImage\":\"%s\", \"width\":100, \"numRows\":1}}",
                        x.labels, label.font, label.size, label.weight, label.color, cell.pad.top,
                        prop, K, base.image, variable.image)

    json.str <- paste("{\"width\":", label.width+graphic.width, ", \"height\":", cell.height*n, ",",
            "\"table\":{\"rowHeights\":[", paste(rep(cell.height,n), collapse=","), "],",
             "\"colwidths\":[", label.width, ",", graphic.width, "],",
            "\"align\":\"right\",",
            "\"rows\":[[", paste(row.str, collapse="],["), "]]}}", sep="")

    graphic(json.str)
}
