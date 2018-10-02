#' Visualizes a number
#'
#' Shows a number as an htmlwidget. The number can be shown on an oval/rectangle or icon.
#' @inherit SinglePicto
#' @param x The number to display
#' @param display A string describing the visualization output. This can be simply "Number"; or
#'  a number on a shape ("Oval" or "Rectangle"); or a number on top of an "Icon";
#'  or a "Pictograph" (where the amount of icons filled reflects the size of \code{x}).
#' @param border.color Color of the border around "Oval" or "Rectangle")
#' @param border.opacity Opacity of border, which should be between 0 and 1.
#' @param border.width Width of the border in pixels.
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
#' @param label.data.pad Space between data label and the edge of the shape/icon in pixels.
#' @param text.below Text to show below the Oval/Rectangle/Icon/Pictograph. For Oval and Rectangle
#' add "<br>" to add new lines to the text.
#' @param text.below.outside Whether \code{text.below} should be shown outside the Oval/Rectangle.
#' For Icon/Pictograph, this is always true.
#' @param text.below.pad Numeric; Space between \code{text.below} and edge of shape/icon in pixels.
#' @param text.below.halign Horizontal alignment of \code{text.below}. There is no control
#' for vertical alignment because it always aligns with the edge of the window.
#' @param text.below.font.family Font family of \code{text.below}.
#' @param text.below.font.color Font color of \code{text.below}.
#' @param text.below.font.size Font size of \code{text.below}.
#' @param text.below.font.weight Weight of \code{text.below}, i.e. one of "bold" or "normal".
#' @param text.above Text to show above the Oval/Rectangle/Icon/Pictograph.
#' @param text.above.outside Whether \code{text.above} should be shown outside the Oval/Rectangle.
#' For Icon/Pictograph, this is always true.
#' @param text.above.pad Numeric; Space between \code{text.above} and edge of shape/icon in pixels.
#' @param text.above.halign Horizontal alignment of \code{text.above}. There is no control
#' for vertical alignment because it always aligns with the edge of the window.
#' @param text.above.font.family Font family of \code{text.above}.
#' @param text.above.font.color Font color of \code{text.above}.
#' @param text.above.font.size Font size of \code{text.above}.
#' @param text.above.font.weight Weight of \code{text.above}, i.e. one of "bold" or "normal".
#' @param ... Other parameters passed to \code{iconWithText}.
#' @importFrom plotly plot_ly layout toRGB config
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
                         display = c("Oval", "Rectangle", "Number", "Icon", "Pictograph")[1],
                         fill.color = rgb(0, 0, 1),
                         fill.opacity = 0.4,
                         total.icons = NA,
                         global.font.family = "Arial",
                         global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                         label.data.number.type = c("Number", "Percentage", "Scientific")[1],
                         label.data.decimals  = 0,
                         label.data.1000.separator = ",",
                         label.data.position = "Overlay", # only used for icons
                         label.data.prefix = "",
                         label.data.suffix = "",
                         label.data.valign = "middle",
                         label.data.halign = "center",
                         label.data.pad = 0.0,
                         label.data.font.family = global.font.family,
                         label.data.font.color = global.font.color,
                         label.data.font.size = 10,
                         label.data.font.weight = "normal",
                         text.below = "",
                         text.below.outside = TRUE,
                         text.below.halign = "center",
                         text.below.pad = 0.0,
                         text.below.font.family = global.font.family,
                         text.below.font.color = global.font.color,
                         text.below.font.size = 10,
                         text.below.font.weight = "normal",
                         text.above = "",
                         text.above.outside = TRUE,
                         text.above.halign = "center",
                         text.above.pad = 0.0,
                         text.above.font.family = global.font.family,
                         text.above.font.color = global.font.color,
                         text.above.font.size = 10,
                         text.above.font.weight = "normal",
                         border.color = rgb(0.5, 0.5, 0.5),
                         border.opacity = 0.5,
                         border.width = 5,
                         background.color = rgb(1, 1, 1),
                         background.opacity = 0,
                         margin.left = 0,
                         margin.right = 0,
                         margin.top = 0,
                         margin.bottom = 0,
                         ...)
{
    display <- switch(tolower(display), oval = "circle", circle = "circle", "number in an oval" = "circle",
                       rectangle = "rectangle", square = "rectangle", "number in a rectangle" = "rectangle",
                       number = "number", icon = "icon",
                       "pictograph (single icon)" = "pictograph - single", "pictograph - single icon" = "pictograph - single",
                       "pictograph (repeated icons)" = "pictograph - repeated", "pictograph - repeated icons" = "pictograph - repeated",  
                       "circle") # default
    
    if (display == "number")
    {
        opacity <- 0.0
        border.width <- 0.0
    }

    # Construct formatted string of x
    tmp.percent <- if (label.data.number.type == "Percentage") "%" else ""
    tmp.format <- if (label.data.number.type == "Scientific") "e" else "f"
    label.str <- paste0(label.data.prefix,
        formatC(if (tmp.percent == "%") x * 100 else x, format = tmp.format,
            digits = label.data.decimals, big.mark = label.data.1000.separator),
        tmp.percent, label.data.suffix)

    if (display %in% c("icon", "pictograph - single", "pictograph - repeated"))
    {
        value <- if (display == "icon") 1.0 else x
        if (display %in% c("icon", "pictograph - single"))
            total.icons <- 1.0
        if (label.data.position %in% c("Above icons", "Below icons"))
        {
            pos <- if (label.data.position == "Above icons") "above" else "below"
            assign(paste0("text.", pos), label.str)
            assign(paste0("text.", pos, ".halign"), label.data.halign)
            assign(paste0("text.", pos, ".valign"), label.data.valign)
            assign(paste0("text.", pos, ".font.family"), label.data.font.family)
            assign(paste0("text.", pos, ".font.color"), label.data.font.color)
            assign(paste0("text.", pos, ".font.size"), label.data.font.size)
            assign(paste0("text.", pos, ".font.weight"), label.data.font.weight)
            label.str <- ""
        }
        if (label.data.position == "None")
            label.str <- ""
        return(iconsWithText(value, fill.icon.color = fill.color, 
            total.icons = total.icons, ..., # other icon parameters?
            text.overlay = label.str, text.overlay.halign = tolower(label.data.halign),
            text.overlay.valign = tolower(label.data.valign), text.overlay.pad = label.data.pad,
            text.overlay.font.family = label.data.font.family, text.overlay.font.color = label.data.font.color,
            text.overlay.font.size = label.data.font.size, text.overlay.font.weight = label.data.font.weight,
            text.below = text.below, text.below.font.weight = text.below.font.weight,
            text.below.halign = tolower(text.below.halign), text.below.pad = text.below.pad,
            text.below.font.family = text.below.font.family, text.below.font.color = text.below.font.color,
            text.below.font.size = text.below.font.size, text.above = text.above,
            text.above.halign = tolower(text.above.halign), text.above.pad = text.above.pad,
            text.above.font.family = text.above.font.family, text.above.font.color = text.above.font.color,
            text.above.font.size = text.above.font.size, text.above.font.weight = text.above.font.weight,
            background.color = if (background.opacity > 0) background.color else "transparent",
            margin.top = margin.top, margin.right = margin.right, margin.bottom = margin.bottom, margin.left = margin.left))
    }

    p <- plot_ly(x = c(0,1), y = c(0, 1), type = "scatter", mode = "none", visible = FALSE, 
            cliponaxis = FALSE, hoverinfo = "skip")

    data.yanchor <- NA
    if (isTextInside(text.above, text.above.outside) && isTextInside(text.below, text.below.outside))
        data.yanchor <- "middle"
    else if (isTextInside(text.above, text.above.outside))
        data.yanchor <- "top"
    else if (isTextInside(text.below, text.below.outside))
        data.yanchor <- "bottom"
    annot.data <- setText(label.str, tolower(label.data.valign), tolower(label.data.halign),
                           font = list(family = label.data.font.family, color = label.data.font.color,
                           size = label.data.font.size), label.data.font.weight,
                           xshift = label.data.pad, yshift = label.data.pad, yanchor = data.yanchor)

    if (data.yanchor == "middle" && isTextInside(text.above, text.above.outside))
        text.above.pad <- text.above.pad + (getVerticalSpace(annot.data))/2
    annot.above <- setText(text.above, "top", tolower(text.above.halign), 
                           font = list(family = text.above.font.family, color = text.above.font.color,
                           size = text.above.font.size), text.above.font.weight,
                           text.above.outside, yshift = text.above.pad)
    
    if (data.yanchor == "middle" && isTextInside(text.below, text.below.outside))
        text.below.pad <- text.below.pad + (getVerticalSpace(annot.data))/2
    annot.below <- setText(text.below, "bottom", tolower(text.below.halign), 
                           font = list(family = text.below.font.family, color = text.below.font.color,
                           size = text.below.font.size), text.below.font.weight,
                           text.below.outside, yshift = text.below.pad)

    margin.top <- margin.top + text.above.outside * getVerticalSpace(annot.above)
    margin.bottom <- margin.bottom + text.below.outside * getVerticalSpace(annot.below)
    
    # Padding is needed to avoid truncating the border
    # But this is approximate because the units are relative, but border width is in pixels
    cpad <- border.width/100

    p <- layout(p, margin = list(l = margin.left, r = margin.right, t = margin.top, 
                 b = margin.bottom, pad = 0, autoexpand = FALSE),
                 xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, range = c(-cpad,1+cpad)),
                 yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, range = c(-cpad,1+cpad)),
                 plot_bgcolor = toRGB(rgb(0,0,0), alpha = 0.0),
                 paper_bgcolor = toRGB(background.color, alpha = background.opacity),
                 shapes = list(type = display, x0 = 0, x1 = 1, y0 = 0, y1 = 1, yref = "y", xref = "x",
                             fillcolor = fill.color, opacity = fill.opacity, layer = "above",
                             line = list(color = toRGB(border.color, alpha = border.opacity),
                                         width = border.width)),
                 annotations = list(annot.data, annot.above, annot.below),
                 hovermode = FALSE)

    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p
}

setText <- function(text, yalign, xalign, font, font.weight,    # parameters always supplied
    outside = NA, yshift = 0, xshift = 0, yanchor = NA)
{
    if (sum(nchar(text), na.rm = TRUE) == 0)
        return (NULL)

    xpos <- switch(xalign, left = 0.0, center = 0.5, right = 1.0)
    if (!is.na(outside) && !outside)
        ypos <- 0.5
    else
        ypos <- switch(yalign, bottom = 0.0, middle = 0.5, top = 1.0)
   
    if (is.na(outside) && is.na(yanchor))                       # aligning text inside the shape
        yanchor <- yalign 
    else if (is.na(yanchor))                                    # aligning text outside the shape
        yanchor <- switch(yalign, top = "bottom", bottom = "top", "middle")

    if (yanchor == "middle") # which direction??
        yshift <- 0
    if (yanchor == "top")
        yshift <- -1 * yshift
    if (xalign == "right")
        xshift <- -1 * xshift

    if (tolower(font.weight) == "bold")
        text <- paste0("<b>", text, "</b>")

    #cat(text, "yanchor:", yanchor, "\n")
    return(list(text = text, font = font, x = xpos, y = ypos,
                showarrow = FALSE, xshift = xshift, yshift = yshift,
                xanchor = xalign, yanchor = yanchor))
}

getVerticalSpace <- function(annot)
{
    if (is.null(annot))
        return(0.0)
    nline <- sum(gregexpr("<br>", annot$text)[[1]] > -1) + 1
    return (abs(annot$yshift) + (annot$font$size * nline) + 5)
}

isTextInside <- function(text, outside)
{
    if (outside)
        return(FALSE)
    if (sum(nchar(text), na.rm = TRUE) == 0)
        return(FALSE)
    return(TRUE)
}

