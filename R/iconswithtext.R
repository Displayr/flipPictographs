# Similar to single picto but has more flexibility with text labels.
# Arguments are designed to match with PrettyNumber
#' @importFrom verbs Sum
iconsWithText <- function (x,
                         total.icons = NA,
                         image = "star",
                         base.image = "",
                         is.custom.url = FALSE,
                         number.rows = NA,
                         number.cols = NA,
                         width.height.ratio = 1,
                         layout = NA,
                         scale = 1,
                         maximum.value = NA,
                         hide.base.image = FALSE,
                         fill.direction = "fromleft",
                         fill.icon.color = "black",
                         base.icon.color = "",
                         background.color = "transparent", # background.fill.opacity not supported
                         auto.size = TRUE,
                         icon.width = 50,
                         pad.row = 0,
                         pad.col = 0,
                         margin = 0,
                         margin.top = margin,  # not used
                         margin.right = margin,
                         margin.bottom = margin,
                         margin.left = margin,
                         global.font.family = "Arial",
                         global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                         text.overlay = "",
                         text.overlay.halign = "center",
                         text.overlay.valign = "middle",
                         text.overlay.pad = 0.0,
                         test.overlap.xpad = 0.0,
                         text.overlay.font.family = global.font.family,
                         text.overlay.font.color = global.font.color,
                         text.overlay.font.size = 10,
                         text.overlay.font.weight = "normal",
                         text.below = "",
                         text.below.halign = "center",
                         text.below.pad = 0.0,
                         text.below.xpad = 0.0,
                         text.below.font.family = global.font.family,
                         text.below.font.color = global.font.color,
                         text.below.font.size = 10,
                         text.below.font.weight = "normal",
                         text.above = "",
                         text.above.halign = "center",
                         text.above.pad = 0.0,
                         text.above.xpad = 0.0,
                         text.above.font.family = global.font.family,
                         text.above.font.color = global.font.color,
                         text.above.font.size = 10,
                         text.above.font.weight = "normal",
                         print.config = FALSE,
                         x.limit = 1000,
                         ...)
{
    if (!(length(x) == 1 && x >= 0))
        stop("Input data must be a single positive number\n")
    if (scale <= 0 && is.na(maximum.value))
        stop("Scale must be greater than zero\n")
    if (isTRUE(grepl("%", attr(scale, "statistic"))))
        scale <- scale/100
    if (!is.na(maximum.value) && scale != 1)
        warning("Parameter scale overridden by maximum value\n")
    if (!is.na(total.icons) && total.icons <= 0)
        stop("Total icons must be greater than zero\n")
    if (!is.na(maximum.value))
    {
        if (maximum.value <= 0)
            stop("Maximum value must be greater than zero\n")
        if (maximum.value < x)
            stop("Input data cannot be greater than 'Maximum value'. ",
                 "Change 'Display' to 'Pictograph (repeated icons)' to show more than 1 icon.\n")
        if (is.na(total.icons))
            total.icons <- maximum.value
        scale <- maximum.value/total.icons
    }

    # Some parameter substitutions for R GUI Controls
    if (is.custom.url)
    {
        fill.icon.color <- ""
        base.icon.color <- ""
        hide.base.image <- nchar(base.image) == 0
    } else
    {
        image <- gsub(" ", "", tolower(image))
    }

    fill.direction <- gsub(" ", "", tolower(fill.direction))
    if (auto.size)
        icon.width <- 50
    if (!is.na(total.icons) && total.icons == 1)
    {
        # Parameters not supplied in Pictographs - Single
        layout <- "Width-to-height ratio"
        pad.row <- 0
        pad.col <- 0
    }
    if (!is.na(layout))
    {
        if (layout != "Width-to-height ratio")
            width.height.ratio = 1
        if (layout != "Number of rows")
            number.rows = NA
        if (layout != "Number of columns")
            number.cols = NA
    }

    # Determine plot values
    if (!is.na(x.limit) && x/scale > x.limit)
    {
        scale <- scale * 10^{floor(log10(x/scale)) - 1}
        warning("The input value is too large to plot, and the Scale has been set to ", scale, ". Consider entering a larger Scale value in the inputs.\n")
    }
    x <- x/scale
    if (is.na(total.icons))
        total.icons <- ceiling(x)
    if (length(total.icons) != 1 && total.icons > 0)
        stop("The total icons must be a single numeric value and greater than zero\n")
    if (!is.na(number.rows) && (number.rows <= 0 || number.rows != ceiling(number.rows)))
        stop("The number of rows must be a positive integer\n")
    if (!is.na(number.rows))
        number.rows <- min(number.rows, total.icons)
    if (!is.na(number.cols) && (number.cols <= 0 || number.cols != ceiling(number.cols)))
        stop("The number of columns must be a positive integer\n")
    if (!is.na(number.cols))
        number.cols <- min(number.cols, total.icons)
    if (width.height.ratio <= 0)
        stop("The width-height ratio must be greater than zero\n")
    if (icon.width <= 0)
        stop("icon width must be greater than zero\n")

    prop <- x/total.icons
    if (prop < 0 | prop > 1)
        stop("Input data must be between 0 and total icons\n")
    if (round(total.icons) != total.icons)
        stop("The number of total icons must be an integer\n")

    # Determine layout based on which parameters are supplied
    layout.str <- ""
    icon.WHratio <- if (is.custom.url) getWidthHeightRatio(image) * (1 + pad.col) / (1 + pad.row)
                    else imageWHRatio[image] * (1 + pad.col) / (1 + pad.row)
    if (!is.na(number.rows)  && is.na(number.cols))
    {
        layout.str <- paste(",\"numRows\":", number.rows, sep="")
        number.cols <- ceiling(total.icons/number.rows)

    } else if (!is.na(number.cols))
    {
        layout.str <- paste(",\"numCols\":", number.cols, sep="")
        number.rows <- ceiling(total.icons/number.cols)
    } else
    {
        number.rows <- max(1, round(sqrt(icon.WHratio/width.height.ratio * total.icons)))
        if (number.rows > total.icons)
            number.rows <- total.icons
        number.cols <- ceiling(total.icons/number.rows)
        layout.str <- paste(",\"numRows\":", number.rows, sep="")
    }

    image.type <- "url"
    if (image %in% c("circle", "square"))
        image.type <- image

    base.image.str <- ""
    if (!hide.base.image)
    {
        if (nchar(base.icon.color) > 0)
            base.icon.color <- paste(base.icon.color, ":", sep="")
        base.image.url <- if (is.custom.url)
        {
            checkImageUrl(base.image)
            base.image
        }
        else
            imageURL[image]

        base.image.str <- if (nchar(base.image.url) == 0 && is.custom.url) ""
                          else paste(",\"baseImage\":\"", image.type, ":", base.icon.color, base.image.url, "\"", sep="")
    }

    image.url <- if (is.custom.url) image else imageURL[image]
    variable.image <- if (is.custom.url)
        paste(image.type, ":", fill.direction, ":", image.url, sep="")
    else
        paste(image.type, ":", fill.direction, ":", fill.icon.color, ":", image.url, sep="")

    # size of pictograph output
    dim.str <- ""
    icon.size.str <- ""
    if (auto.size)
        dim.str <- "\"rowHeights\":[\"proportion:1\"], \"colWidths\":[\"flexible:graphic\"]"
    else
    {
        dim.str <- "\"rowHeights\":[\"fixedsize:graphic\"], \"colWidths\":[\"fixedsize:graphic\"]"
        icon.size.str <- paste0(",\"imageWidth\":", icon.width)
    }

    # Text labels
    label.overlay.str <- ""
    label.above.str <- ""
    label.below.str <- ""

    # Adjust margins to fit text labels
    # padding format: top right bottom left
    pad.above.left <- pad.above.right <- pad.above.top <- pad.above.bottom <- 0
    pad.below.left <- pad.below.right <- pad.below.top <- pad.below.bottom <- 0
    if (any(nzchar(text.above)))
    {
        if (text.above.halign == "left")
            pad.above.left <- text.above.xpad
        if (text.above.halign == "right")
            pad.above.right <- text.above.xpad
    }
    if (any(nzchar(text.below)))
    {
        if (text.below.halign == "left")
            pad.below.left <- text.below.xpad
        if (text.below.halign == "right")
            pad.below.right <- text.below.xpad
    }
    margin.left <- margin.left + max(0, -pad.above.left, -pad.below.left)
    margin.right <- margin.right + max(0, pad.above.right, pad.below.right)

    if (any(nzchar(text.above)))
        label.above.str <- sprintf(paste0(", \"table-header\":{\"padding\": \"%f %f %f %f\", ",
            "\"text\":\"%s\", \"font-size\":\"%fpx\", \"font-family\":\"%s\", ",
            "\"font-color\":\"%s\", \"font-weight\":\"%s\", ",
            "\"horizontal-align\":\"%s\", \"vertical-align\":\"top\"}"),
            margin.top + 1, margin.right - pad.above.right + 1,
            max(0, text.above.pad) + 1, margin.left + pad.above.left + 1,
            cleanPictographLabels(text.above), text.above.font.size, text.above.font.family,
            text.above.font.color, text.above.font.weight, text.above.halign)

    if (any(nzchar(text.below)))
        label.below.str <- sprintf(paste0(", \"table-footer\":{\"padding\": \"%f %f %f %f\", ",
            "\"text\":\"%s\", \"font-size\":\"%fpx\", \"font-family\":\"%s\", ",
            "\"font-color\":\"%s\", \"font-weight\":\"%s\", ",
            "\"horizontal-align\":\"%s\", \"vertical-align\":\"bottom\"}"),
            max(text.below.pad, 0) + 1, margin.right - pad.below.right + 1,
            margin.bottom + 1, margin.left + pad.below.left + 1,
            cleanPictographLabels(text.below), text.below.font.size, text.below.font.family,
            text.below.font.color, text.below.font.weight, text.below.halign)

    if (any(nzchar(text.overlay)))
    {
        xpos <- if (text.overlay.halign == "left") 0
                else if (text.overlay.halign == "right") number.cols
                else number.cols/2
        ypos <- if (text.overlay.valign == "top") 0
                else if (text.overlay.valign == "bottom") number.rows
                else number.rows/2
        label.overlay.str <- sprintf(paste0(",\"floatingLabels\":[{\"position\":\"%f:%f\", ",
          "\"text\":\"%s\", \"font-size\":\"%fpx\", \"font-family\":\"%s\", ",
          "\"font-color\":\"%s\", \"font-weight\":\"%s\", \"horizontal-align\":\"%s\"}]"),
          ypos, xpos, cleanPictographLabels(text.overlay), text.overlay.font.size, text.overlay.font.family,
          text.overlay.font.color, text.overlay.font.weight, text.overlay.halign)
    }

    pad.around.icons <- sprintf(",\"padding\":\"%f %f %f %f\"",
        margin.top, margin.right, margin.bottom, margin.left)

    json.string <- paste0("{\"table\": {", dim.str,
          ",\"rows\":[[{\"type\":\"graphic\", \"value\":{",
          "\"proportion\":", prop, pad.around.icons,
          ",\"numImages\":", total.icons,
          label.overlay.str,
          icon.size.str,
          layout.str,
          ",\"rowGutter\":", pad.row,
          ",\"columnGutter\":", pad.col,
          ",\"variableImage\":\"", variable.image, "\"", base.image.str, "}}]]}",
          label.above.str, label.below.str,
          ",\"background-color\":\"", background.color, "\"}")

    if (print.config)
        cat(json.string)
    res <- graphic(json.string)
    class(res) <- c(class(res), "visualization-selector")
    return(res)
}

cleanPictographLabels <- function(x)
{
    # New line characters were causing errors in the JSON
    # Note these can be coded as \n or \r
    x <- gsub("\\s", " ", x)

    # Escape backslashes in labels
    x <- gsub("\\", "\\\\", x, fixed = TRUE)

    # These characters used to be shown as text but that is
    # probably not what the user wants to see
    x <- gsub("&nbsp;", " ", x)
    x <- gsub('"', '\\"', x, fixed = TRUE)
    return(x)
}
