#' Visualizes a number
#'
#' Shows a number as an htmlwidget. The number can be shown on an oval/rectangle or icon.
#' @inherit SinglePicto
#' @param x The number to display
#' @param display A string describing the visualization output. This can be simply "Number"; or
#'  a number on a shape ("Oval", "Rectangle", "Donut", "Gauge"); or a number on top of an "Icon";
#'  or a "Pictograph" (where the amount of icons filled reflects the size of \code{x}).
#' @param border.color Color of the border around "Oval" or "Rectangle")
#' @param border.opacity Opacity of border, which should be between 0 and 1.
#' @param border.width Width of the border as a proportion of the graphic dimensions.
#' @param border.resolution The number of points used to define the border of the circle.
#' @param base.color The color of the base graphic when a Pictograph (with the base image shown)
#'   or a Donut is displayed. For backwards compatibility, \code{base.icon.color} can also be used.
#' @param base.opacity Alpha transparency; only used when \code{display} is Donut.
#' @param fill.color Color of the shape (Oval or Rectangle) or the icon (for Icon or Pictograph)
#'   if custom.icon is not used.
#' @param fill.opacity Alpha transparency of the Oval or Rectangle.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param background.opacity Transparency of background (0 to 1). This is only valid for
#' Number, Oval or Rectangle.
#' @param label.data.number.type Format in which \code{x} should be shown. One of "Number", "Percentage", "Scientific".
#' @param label.data.decimals Integer; Number of decimals shown in data label.
#' @param label.data.1000.separator String placed to separate thousands.
#' By default this is a comma. Set to empty string to hide.
#' @param label.data.position This is only used for Icon or Pictograph. It can be one of
#' "Above", "Below" or "Overlay" (i.e. on to top of the icons). For oval/rectangle it
#' is always "Overlay". If it is set to "Above", then the \code{label.data} parameters
#' with override the \code{text.above} parameters. If set to "Below", it will override \code{text.below}.
#' @param label.data.halign Horizontal alignment of data label. One of "left", "center or "right".
#' @param label.data.valign Vertical alignment of data label. One of "top", "middle", "bottom".
#' This is ignored if \code{text.above.outside} or \code{text.below.outside} is false.
#' @param label.data.pad Vertical space between data label and the edge of the shape/icon in pixels.
#' @param label.data.xpad Horizontal space between data label and the edge of the shape/icon in pixels.
#' @param text.below Text to show below the Oval/Rectangle/Icon/Pictograph. For Oval and Rectangle
#' add "<br>" to add new lines to the text.
#' @param text.below.outside Whether \code{text.below} should be shown outside the Oval/Rectangle.
#' For Icon/Pictograph, this is always true.
#' @param text.below.pad Numeric; Vertical space between \code{text.below} and edge of shape/icon in pixels.
#' @param text.below.xpad Numeric; Horizontal space between \code{text.below} and edge of shape/icon in pixels.
#' @param text.below.halign Horizontal alignment of \code{text.below}. There is no control
#' for vertical alignment because it always aligns with the edge of the window.
#' @param text.below.font.family Font family of \code{text.below}.
#' @param text.below.font.color Font color of \code{text.below}.
#' @param text.below.font.size Font size of \code{text.below}.
#' @param text.below.font.weight Weight of \code{text.below}, i.e. one of "bold" or "normal".
#' @param text.above Text to show above the Oval/Rectangle/Icon/Pictograph.
#' @param text.above.outside Whether \code{text.above} should be shown outside the Oval/Rectangle.
#' For Icon/Pictograph, this is always true.
#' @param text.above.pad Numeric; Vertical space between \code{text.above} and edge of shape/icon in pixels.
#' @param text.above.xpad Numeric; Horizontal space between \code{text.above} and edge of shape/icon in pixels.
#' @param text.above.halign Horizontal alignment of \code{text.above}. There is no control
#' for vertical alignment because it always aligns with the edge of the window.
#' @param text.above.font.family Font family of \code{text.above}.
#' @param text.above.font.color Font color of \code{text.above}.
#' @param text.above.font.size Font size of \code{text.above}.
#' @param text.above.font.weight Weight of \code{text.above}, i.e. one of "bold" or "normal".
#' @param hover.text Optional text to show when the cursor hovers above widget.
#' @param hover.distance Deprecated.
#' @param hover.bg.color Color of the background of the hovertext.
#' @param hover.font.family Font family of \code{hover.text}.
#' @param hover.font.color Font color of \code{hover.text}.
#' @param hover.font.size Font size of \code{hover.text}.

#' @param hole.size Numeric between 0 and 1, specifying the size of the hole when \code{display}
#'      is "Donut" or "Gauge".
#' @param segment.gap Numeric between 0 and 1, specifying the gap between the segments
#'      in the gauge.
#' @param maximum.value Numeric value specifying the maximum that \code{x} will be expected to take.
#'      This value is used to show proportional data
#'      (i.e. \code{display} is "Donut", "Gauge" or "Pictograph (single icon)").
#' @param minimum.value Numeric value specifying the minimum value that \code{x} is expected to take.
#'      This value is only used in "Gauge" (for "Donut" and "Pictograph (single icon)" it is
#'      always set as zero.
#' @param tick.show Whether to show the \code{minimum.value} and \code{maximum.value} when
#'      \code{display} is "Gauge".
#' @param tick.outside Whether to show the ticks inside or outside the gauge.
#' @param tick.number.type Format in which \code{x} should be shown. One of "Automatic", "Number", "Percentage", "Scientific". If "Automatic" is used, then a percentage format will be used if \code{attr(x, "statistic")} is "\%". Otherwise a number format will be used.
#' @param tick.decimals Integer; Number of decimals shown in ticks
#' @param tick.1000.separator String placed to separate thousands. By default this is a comma. Set to empty string to hide.
#' @param tick.prefix Optional text to prepend to ticks.
#' @param tick.suffix Optional text to append to ticks.
#' @param tick.font.family Font family of \code{tick}.
#' @param tick.font.color Font color of \code{tick}.
#' @param tick.font.size Font size of \code{tick}.
#' @param font.unit Set to 'pt' (default) to get font sizing consistent with textboxes.
#' Otherwise fonts will be taken to be specified in pixels.
#' @param ... Other parameters passed to \code{iconWithText}.
#' @importFrom plotly plot_ly layout toRGB config add_pie add_trace
#' @export
#' @examples
#' VisualizeNumber(4.0, display = "Rectangle", text.above = "Above", text.above.outside = TRUE)
#' VisualizeNumber(7.0, display = "Oval", label.data.prefix = "Number: ", label.data.suffix = ".",
#'      label.data.valign = "bottom", label.data.halign = "right", label.data.pad = 30)
#' VisualizeNumber(Sys.Date(), text.above = "The date is", text.below = "today.",
#'      global.font.color = "white", text.above.font.size = 10, text.below.font.size = 10,
#'      label.data.font.size = 20, background.color = "grey", background.opacity = 1.0)
#' VisualizeNumber(-7, text.below = "FROZEN<br>FOODS", global.font.color = "white",
#'      text.above.font.size = 10, text.below.font.size = 10, label.data.font.size = 20,
#'      background.color = "grey", background.opacity = 1.0, text.above.outside = FALSE,
#'      text.below.outside = FALSE)
VisualizeNumber <- function(x,
                         display = c("Oval", "Rectangle", "Number", "Icon", "Donut", "Gauge", "Pictograph")[1],
                         fill.color = rgb(166, 197, 57, maxColorValue = 255),
                         fill.opacity = 0.4,
                         total.icons = NA,
                         global.font.family = "Arial",
                         global.font.color = "#808080",
                         label.data.number.type = c("Automatic", "Number", "Percentage", "Scientific")[1],
                         label.data.decimals  = 0,
                         label.data.1000.separator = ",",
                         label.data.position = "Overlay", # only used for icons
                         label.data.prefix = "",
                         label.data.suffix = "",
                         label.data.valign = "middle",
                         label.data.halign = "center",
                         label.data.pad = 0.0,
                         label.data.xpad = 0.0,
                         label.data.font.family = global.font.family,
                         label.data.font.color = global.font.color,
                         label.data.font.size = 16,
                         label.data.font.weight = "normal",
                         text.below = "",
                         text.below.outside = TRUE,
                         text.below.halign = "center",
                         text.below.pad = 0.0,
                         text.below.xpad = 0.0,
                         text.below.font.family = global.font.family,
                         text.below.font.color = global.font.color,
                         text.below.font.size = 10,
                         text.below.font.weight = "normal",
                         text.above = "",
                         text.above.outside = TRUE,
                         text.above.halign = "center",
                         text.above.pad = 0.0,
                         text.above.xpad = 0.0,
                         text.above.font.family = global.font.family,
                         text.above.font.color = global.font.color,
                         text.above.font.size = 10,
                         text.above.font.weight = "normal",
                         border.color = rgb(0.5, 0.5, 0.5),
                         border.opacity = 0.5,
                         border.width = 0.0,
                         border.resolution = 1000,
                         segment.gap = 0.000,
                         hole.size = 0.8,
                         tick.outside = TRUE,
                         tick.show = TRUE,
                         tick.font.family = global.font.family,
                         tick.font.color = global.font.color,
                         tick.font.size = 8,
                         tick.number.type = label.data.number.type,
                         tick.decimals = label.data.decimals,
                         tick.1000.separator = label.data.1000.separator,
                         tick.prefix = "",
                         tick.suffix = "",
                         minimum.value = 0.0,
                         maximum.value = NA,
                         base.icon.color = "", # backward compatability
                         base.color = base.icon.color,
                         base.opacity = fill.opacity,
                         background.color = rgb(1, 1, 1),
                         background.opacity = 0,
                         hover.text = "",
                         hover.distance = -1,
                         hover.bg.color = rgb(0.5,0.5,0.5),
                         hover.font.color = rgb(1,1,1),
                         hover.font.size = 9,
                         hover.font.family = global.font.family, 
                         font.unit = "pt",
                         margin.left = 0,
                         margin.right = 0,
                         margin.top = 0,
                         margin.bottom = 0,
                         ...)
{
    display <- switch(tolower(display), oval = "circle", circle = "circle", "number in an oval" = "circle",
                       rectangle = "rectangle", square = "rectangle", "number in a rectangle" = "rectangle",
                       number = "number",
                       donut = "donut", "number in a donut" = "donut", "number on a donut" = "donut",
                       gauge = "gauge", "number in a gauge" = "gauge", "number on a gauge" = "gauge",
                       bar = "bar", "number in a bar" = "bar", "number on a bar" = "bar",
                       icon = "icon", "number on an icon" = "icon",
                       "pictograph (single icon)" = "pictograph - single", "pictograph - single icon" = "pictograph - single",
                       "pictograph (repeated icons)" = "pictograph - repeated", "pictograph - repeated icons" = "pictograph - repeated",
                       "circle") # default

    if (tolower(font.unit) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        label.data.font.size = round(fsc * label.data.font.size, 0)
        text.above.font.size = round(fsc * text.above.font.size, 0)
        text.below.font.size = round(fsc * text.below.font.size, 0)
        tick.font.size = round(fsc * tick.font.size, 0)
        hover.font.size = round(fsc * hover.font.size, 0)
    }

    if (display == "number")
    {
        opacity <- 0.0
        border.width <- 0.0
    }
    if (border.width < 0 || border.width >=  0.5)
    {
        warning("Border width must be between 0 and 0.5.")
        border.width <- 0
    }

    # Construct formatted string of x
    if (label.data.number.type == "Automatic" && isTRUE(attr(x, "statistic") == "%"))
        label.data.number.type <- "Percentage"
    tmp.percent <- if (label.data.number.type == "Percentage") "%" else ""
    tmp.format <- if (label.data.number.type == "Scientific") "e" else "f"
    if (is.na(x) || is.null(x))
        label.str <- "NA"
    else
        label.str <- paste0(label.data.prefix,
        formatC(if (tmp.percent == "%") x * 100 else x, format = tmp.format,
            digits = label.data.decimals, big.mark = label.data.1000.separator),
        tmp.percent, label.data.suffix)
    if (is.na(x) || is.null(x))
        x <- 0

    if (display %in% c("icon", "pictograph - single", "pictograph - repeated"))
    {
        value <- if (display == "icon") 1.0 else x
        if (!is.numeric(value))
            stop("Input value for pictographs cannot be non-numeric.")
        if (display %in% c("icon", "pictograph - single"))
            total.icons <- 1.0
        if (label.data.position %in% c("Above icons", "Below icons"))
        {
            pos <- if (label.data.position == "Above icons") "above" else "below"
            assign(paste0("text.", pos), label.str)
            assign(paste0("text.", pos, ".halign"), label.data.halign)
            assign(paste0("text.", pos, ".valign"), label.data.valign)
            assign(paste0("text.", pos, ".pad"), label.data.pad)
            assign(paste0("text.", pos, ".xpad"), label.data.xpad)
            assign(paste0("text.", pos, ".font.family"), label.data.font.family)
            assign(paste0("text.", pos, ".font.color"), label.data.font.color)
            assign(paste0("text.", pos, ".font.size"), label.data.font.size)
            assign(paste0("text.", pos, ".font.weight"), label.data.font.weight)
            label.str <- ""
        }
        if (label.data.position == "None")
            label.str <- ""
        return(iconsWithText(value, fill.icon.color = fill.color,
            base.icon.color = base.color, maximum.value = maximum.value,
            total.icons = total.icons, ..., # other icon parameters?
            text.overlay = label.str, text.overlay.halign = tolower(label.data.halign),
            text.overlay.valign = tolower(label.data.valign),
            text.overlay.pad = label.data.pad, text.overlay.xpad = label.data.xpad,
            text.overlay.font.family = label.data.font.family,
            text.overlay.font.color = label.data.font.color,
            text.overlay.font.size = label.data.font.size,
            text.overlay.font.weight = tolower(label.data.font.weight),
            text.below = text.below, text.below.font.weight = tolower(text.below.font.weight),
            text.below.halign = tolower(text.below.halign),
            text.below.pad = text.below.pad, text.below.xpad = text.below.xpad,
            text.below.font.family = text.below.font.family,
            text.below.font.color = text.below.font.color,
            text.below.font.size = text.below.font.size, text.above = text.above,
            text.above.halign = tolower(text.above.halign),
            text.above.pad = text.above.pad, text.above.xpad = text.above.xpad,
            text.above.font.family = text.above.font.family, text.above.font.color = text.above.font.color,
            text.above.font.size = text.above.font.size,
            text.above.font.weight = tolower(text.above.font.weight),
            background.color = if (background.opacity > 0) background.color else "transparent",
            margin.top = margin.top, margin.right = margin.right,
            margin.bottom = margin.bottom, margin.left = margin.left))
    }

    if (display %in% c("donut", "gauge", "bar"))
    {
        if (!is.numeric(x) || !is.finite(x))
            stop("Input data is non-numeric")
        if (sum(nchar(base.color), na.rm = TRUE) == 0)
            base.color <- rgb(230, 230, 230, maxColorValue = 255)
        if (is.na(maximum.value))
            maximum.value <- 1
        if (display == "donut")
            minimum.value <- 0.0
        if (maximum.value <= minimum.value)
            stop("'Maximum value' (", maximum.value, ") must be greater than the 'Minimum value'(",
                 minimum.value, ")")
        prop <- (x - minimum.value)/(maximum.value - minimum.value)
        if (prop > 1)
            stop("Input data cannot be greater than 'Maximum value'.")
        if (prop < 0 && display == "donut")
            stop("Input data cannot be smaller than zero.")
        if (prop < 0 && display == "gauge")
            stop("Input data cannot be smaller than 'Minimum value'.")
    }

    if (display == "donut")
    {
        p <- plot_ly(values = c(prop, 1 - prop), hoverinfo = "skip", textinfo = "none",
                marker = list(colors = c(toRGB(fill.color, alpha = fill.opacity),
                toRGB(base.color, alpha = base.opacity)),
                line = list(width = border.width * 100,
                color = toRGB(border.color, alpha = border.opacity))))
        p <- add_pie(p, hole = hole.size, direction = "clockwise", sort = FALSE)
        shapes <- NULL

    } else if (display == "gauge")
    {
        p <- plot_ly(x = c(0, 1), y = c(0, 1), type = "scatter", mode = "none",
            cliponaxis = FALSE, hoverinfo = "skip")
        fill.segment <- pathToShape(segmentPath(c(0, prop - segment.gap), hole.size,
            border.resolution), fill.color, fill.opacity)
        bg.segment <- pathToShape(segmentPath(c(prop + segment.gap, 1), hole.size,
            border.resolution), base.color, base.opacity)
        shapes <- list(fill.segment, bg.segment)
    
    } else if (display == "bar")
    {
        p <- plot_ly(x = c(0,1), y = c(0, 1), type = "scatter", mode = "none",
                cliponaxis = FALSE, hoverinfo = "skip")
        fill.shape <- list(type = "rectangle", x0 = 0, x1 = prop,
                           y0 = 0, y1 = 1, yref = "y", xref = "x",
                           fillcolor = fill.color, opacity = fill.opacity, layer = "above",
                           line = list(width = 0))
        shapes <- fill.shape

    } else
    {
        p <- plot_ly(x = c(0,1), y = c(0, 1), type = "scatter", mode = "none",
            cliponaxis = FALSE, hoverinfo = "skip")
        border.path <- NULL
        if (border.width > 0)
        {
            border.path <- if (display == "rectangle") rectangleBorder(border.width)
                           else                        circleBorder(border.width, border.resolution)
        }
        fill.shape <- list(type = display, x0 = border.width, x1 = (1 - border.width),
                           y0 = border.width, y1 = 1 - border.width, yref = "y", xref = "x",
                           fillcolor = fill.color, opacity = fill.opacity, layer = "above",
                           line = list(width = 0))
        shapes <- list(pathToShape(border.path, border.color, border.opacity), fill.shape)
    }


    # Position data and text labels
    data.yanchor <- NA
    if (tolower(label.data.valign) == "middle")
    {
        if (isTextInside(text.above, text.above.outside) && isTextInside(text.below, text.below.outside))
            data.yanchor <- "middle"
        else if (isTextInside(text.above, text.above.outside))
            data.yanchor <- "top"
        else if (isTextInside(text.below, text.below.outside))
            data.yanchor <- "bottom"
    }
    annot.data <- setText(label.str, tolower(label.data.valign), tolower(label.data.halign),
                           font = list(family = label.data.font.family, color = label.data.font.color,
                           size = label.data.font.size), label.data.font.weight,
                           xmax = if (display == "bar") prop else 1.0,
                           xshift = label.data.xpad, yshift = label.data.pad, yanchor = data.yanchor)
    if (display == "bar" && label.data.halign == "right")
        annot.data$x <- prop
    if (sum(nchar(hover.text), na.rm = TRUE) > 0)
        annot.data$hovertext <- hover.text

    if (isTRUE(data.yanchor == "middle") && isTextInside(text.above, text.above.outside))
        text.above.pad <- text.above.pad + (getVerticalSpace(annot.data))/2
    annot.above <- setText(text.above, "top", tolower(text.above.halign),
                           font = list(family = text.above.font.family, color = text.above.font.color,
                           size = text.above.font.size), text.above.font.weight,
                           xmax = if (display == "bar") prop else 1.0,
                           text.above.outside, xshift = text.above.xpad, yshift = text.above.pad)

    if (isTRUE(data.yanchor == "middle") && isTextInside(text.below, text.below.outside))
        text.below.pad <- text.below.pad + (getVerticalSpace(annot.data))/2
    annot.below <- setText(text.below, "bottom", tolower(text.below.halign),
                           font = list(family = text.below.font.family, color = text.below.font.color,
                           size = text.below.font.size), text.below.font.weight,
                           xmax = if (display == "bar") prop else 1.0,
                           text.below.outside, xshift = text.below.xpad, yshift = text.below.pad)

    tick0 <- NULL
    tick1 <- NULL
    if (display == "gauge" && tick.show)
    {
        tmp.vals <- c(minimum.value, maximum.value)
        tmp.percent <- if (tick.number.type == "Percentage") "%" else ""
        tmp.format <- if (tick.number.type == "Scientific") "e" else "f"
        tick.str <- paste0(tick.prefix,
            formatC(if (tmp.percent == "%") tmp.vals * 100 else tmp.vals, format = tmp.format,
                digits = tick.decimals, big.mark = tick.1000.separator),
            tmp.percent, tick.suffix)

        tick.font = list(family = tick.font.family, size = tick.font.size, color = tick.font.color)
        tick.align <- if (tick.outside) "top" else "bottom"
        pos <- (1 - hole.size)/4
        tick0 <- list(text = tick.str[1], font = tick.font, x = pos, y = 0, yshift = 0,
                    yanchor = tick.align, xanchor = "center", showarrow = FALSE)
        tick1 <- list(text = tick.str[2], font = tick.font, x = 1 - pos, y = 0, yshift = 0,
                    yanchor = tick.align, xanchor = "center", showarrow = FALSE)
    }

    # Adjust margins so that labels do not fall off
    margin.top <- margin.top +
                  text.above.outside * getVerticalSpace(annot.above, direction = "top")
    margin.bottom <- margin.bottom +
                     max(text.below.outside * getVerticalSpace(annot.below, direction = "bottom"),
                         tick.outside * getVerticalSpace(tick0), tick.outside * getVerticalSpace(tick1))
    margin.left <- margin.left +
                   max(getLeftSpace(annot.above), getLeftSpace(annot.data), getLeftSpace(annot.below))
    margin.right <- margin.right +
                    max(getRightSpace(annot.above), getRightSpace(annot.data), getRightSpace(annot.below))


    p <- layout(p, margin = list(l = margin.left, r = margin.right, t = margin.top,
                 b = margin.bottom, pad = 0, autoexpand = TRUE), showlegend = FALSE,
                 xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE,
                              range = c(0,1), fixedrange = display != "gauge"),
                 yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE,
                              range = c(0,1), fixedrange = display != "gauge", scaleratio = 0.5,
                              scaleanchor = if (display == "gauge") "x" else NULL),
                 plot_bgcolor = toRGB(rgb(0,0,0), alpha = 0.0),
                 paper_bgcolor = toRGB(background.color, alpha = background.opacity),
                 shapes = shapes, annotations = list(annot.data, annot.above, annot.below, tick0, tick1),
                 hoverlabel = list(bgcolor = hover.bg.color, bordercolor = hover.bg.color,
                              font = list(color = hover.font.color, size = hover.font.size, 
                              family = hover.font.family)),
                 hovermode = "closest", hoverdistance = hover.distance)

    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p
}

setText <- function(text, yalign, xalign, font, font.weight,    # parameters always supplied
    outside = NA, yshift = 0, xshift = 0, yanchor = NA, xmax = 1.0)
{
    if (sum(nchar(text), na.rm = TRUE) == 0)
        return (NULL)

    xpos <- switch(xalign, left = 0.0, center = xmax/2, right = xmax)
    if (!is.na(outside) && !outside)
        ypos <- 0.5
    else
        ypos <- switch(yalign, bottom = 0.0, middle = 0.5, top = 1.0)

    eval(yanchor)
    if (is.na(outside) && is.na(yanchor))                       # aligning text inside the shape
        yanchor <- yalign
    else if (is.na(yanchor))                                    # aligning text outside the shape
        yanchor <- switch(yalign, top = "bottom", bottom = "top", "middle")
    if (yanchor == "top")
        yshift <- -1 * yshift

    if (tolower(font.weight) == "bold")
        text <- paste0("<b>", text, "</b>")

    #cat("x:", xpos, "y:", ypos, text, "yalign:", yalign, "yanchor:", yanchor, "outside:", outside, "\n")
    return(list(text = text, font = font, x = xpos, y = ypos,
                showarrow = FALSE, xshift = xshift, yshift = yshift,
                xanchor = xalign, yanchor = yanchor))
}

getVerticalSpace <- function(annot, direction = "any")
{
    if (is.null(annot))
        return(0.0)
    if (direction == "top" && (annot$yshift < 0 || annot$y < 1))
        return(0.0)
    if (direction == "bottom" && (annot$yshift > 0 || annot$y > 0))
        return(0.0)
    nline <- sum(gregexpr("<br>", annot$text)[[1]] > -1) + 1
    return (abs(annot$yshift) + (annot$font$size * nline) + 5)
}

getLeftSpace <- function(annot)
{
    if (is.null(annot) || annot$x > 0.0)
        return (0.0)
    else
        return(max(0.0, -1 * annot$xshift))
}

getRightSpace <- function(annot)
{
    if (is.null(annot) || annot$x < 1.0)
        return (0.0)
    else
        return(max(0.0, annot$xshift))
}

isTextInside <- function(text, outside)
{
    if (outside)
        return(FALSE)
    if (sum(nchar(text), na.rm = TRUE) == 0)
        return(FALSE)
    return(TRUE)
}

