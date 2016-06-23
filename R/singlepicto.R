#' SinglePicto
#'
#' Creates a single pictograph. Allows customization of the number of icons
#' and dimensions.
#' @seealso PictoStdChart to create a chart or table of pictographs
#'
#' @param x Number of filled icons (out of \code{K = number.rows * number.columns})
#' @param number.rows The number of rows of icons.
#' @param number.columns The number of columns of stars.
#' @param image name of icon
#' @param base.image name of background icon. Set to "none" for no background.
#' @param direction Direction in which pictograph is filled (horizontal, vertical or radial).
#' @param auto.size Automatically sizes the plot based on the size of the window/slot.
#' @param width Width of a single star in pixels when \code{auto.size} is FALSE.
#'
#' @importFrom  rhtmlPictographs graphic
#' @export
SinglePicto <- function (x, number.rows, number.columns,
                         image="star.filled", base.image="star.empty",
                         direction="horizontal",
                         auto.size = FALSE, width = 25)
{
    image.height <- width*number.rows
    number.images <- number.columns * number.rows
    prop <- x/number.images
    if (prop < 0 | prop > 1)
        stop("x must be between 0 and ", number.images, "\n")
    if (number.rows == 0 || round(number.rows) != number.rows)
        stop("number.rows must be an integer greater than 0\n")
    if (number.columns == 0 || round(number.columns) != number.columns)
        stop("number.columns must be an integer greater than 0\n")

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

    base.image <- URL[base.image]
    variable.image <- paste(direction, ":", URL[image], sep="")

    json.string <- paste("{\"proportion\":", prop,
          ",\"numImages\":", number.images,
          ",\"numRows\":", number.rows,
          ",\"baseImage\":\"", base.image, "\", ",
          "\"variableImage\":\"", variable.image, "\", ",
          "\"width\":", number.columns*width,
          ",\"height\":", image.height,
          sep="")

    json.string <- if(auto.size) paste(json.string, ", \"preserveAspectRatio\":\"xMidYMid\"}", sep="")
            else paste(json.string, ",\"resizable\":\"false\"}", sep="")

    graphic(json.string)
}
