% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizenumber.R
\name{VisualizeNumber}
\alias{VisualizeNumber}
\title{Visualizes a number}
\usage{
VisualizeNumber(
  x,
  display = c("Oval", "Rectangle", "Number", "Icon", "Donut", "Gauge", "Pictograph")[1],
  fill.color = rgb(166, 197, 57, maxColorValue = 255),
  fill.opacity = 0.4,
  total.icons = NA,
  global.font.family = "Arial",
  global.font.color = "#808080",
  label.data.number.type = c("Automatic", "Number", "Percentage", "Scientific")[1],
  label.data.decimals = 0,
  label.data.1000.separator = ",",
  label.data.position = "Overlay",
  label.data.prefix = "",
  label.data.suffix = "",
  label.data.valign = "middle",
  label.data.halign = "center",
  label.data.pad = 0,
  label.data.xpad = 0,
  label.data.font.family = global.font.family,
  label.data.font.color = global.font.color,
  label.data.font.size = 16,
  label.data.font.weight = "normal",
  text.below = "",
  text.below.outside = TRUE,
  text.below.halign = "center",
  text.below.pad = 0,
  text.below.xpad = 0,
  text.below.font.family = global.font.family,
  text.below.font.color = global.font.color,
  text.below.font.size = 10,
  text.below.font.weight = "normal",
  text.above = "",
  text.above.outside = TRUE,
  text.above.halign = "center",
  text.above.pad = 0,
  text.above.xpad = 0,
  text.above.font.family = global.font.family,
  text.above.font.color = global.font.color,
  text.above.font.size = 10,
  text.above.font.weight = "normal",
  border.color = rgb(0.5, 0.5, 0.5),
  border.opacity = 0.5,
  border.width = 0,
  border.resolution = 1000,
  segment.gap = 0,
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
  minimum.value = 0,
  maximum.value = NA,
  base.icon.color = "",
  base.color = base.icon.color,
  base.opacity = fill.opacity,
  background.color = rgb(1, 1, 1),
  background.opacity = 0,
  hover.text = "",
  hover.distance = 0.2,
  hover.bg.color = rgb(0.5, 0.5, 0.5),
  hover.font.color = rgb(1, 1, 1),
  hover.font.size = 9,
  hover.font.family = global.font.family,
  font.unit = "pt",
  margin.left = 0,
  margin.right = 0,
  margin.top = 0,
  margin.bottom = 0,
  ...
)
}
\arguments{
\item{x}{The number to display}

\item{display}{A string describing the visualization output. This can be simply "Number"; or
a number on a shape ("Oval", "Rectangle", "Donut", "Gauge"); or a number on top of an "Icon";
or a "Pictograph" (where the amount of icons filled reflects the size of \code{x}).}

\item{fill.color}{Color of the shape (Oval or Rectangle) or the icon (for Icon or Pictograph)
if custom.icon is not used.}

\item{fill.opacity}{Alpha transparency of the Oval or Rectangle.}

\item{total.icons}{Total number of icons. Defaults to \code{total.icons=ceiling(x/scale)}.}

\item{global.font.family}{Character; font family for all occurrences of any
font attribute for the chart unless specified individually.}

\item{global.font.color}{Global font color as a named color in character format
(e.g. "black") or an a hex code.}

\item{label.data.number.type}{Format in which \code{x} should be shown.
One of "Number", "Percentage", "Percentage (no sign)", Scientific".}

\item{label.data.decimals}{Integer; Number of decimals shown in data label.}

\item{label.data.1000.separator}{String placed to separate thousands.
By default this is a comma. Set to empty string to hide.}

\item{label.data.position}{This is only used for Icon or Pictograph. It can be one of
"Above", "Below" or "Overlay" (i.e. on to top of the icons). For oval/rectangle it
is always "Overlay". If it is set to "Above", then the \code{label.data} parameters
with override the \code{text.above} parameters. If set to "Below", it will override \code{text.below}.}

\item{label.data.prefix}{String to prepend data label.}

\item{label.data.suffix}{String to append to data label.}

\item{label.data.valign}{Vertical alignment of data label. One of "top", "middle", "bottom".
This is ignored if \code{text.above.outside} or \code{text.below.outside} is false.}

\item{label.data.halign}{Horizontal alignment of data label. One of "left", "center or "right".}

\item{label.data.pad}{Vertical space between data label and the edge of the shape/icon in pixels.}

\item{label.data.xpad}{Horizontal space between data label and the edge of the shape/icon in pixels.}

\item{label.data.font.family}{Font in which the data labels are displayed.}

\item{label.data.font.color}{Font color of data labels.}

\item{label.data.font.size}{Font size of data labels.}

\item{label.data.font.weight}{Weight of data labels, i.e. one of \code{"bold"} or \code{"normal"}.}

\item{text.below}{Text to show below the Oval/Rectangle/Icon/Pictograph. For Oval and Rectangle
add "<br>" to add new lines to the text.}

\item{text.below.outside}{Whether \code{text.below} should be shown outside the Oval/Rectangle.
For Icon/Pictograph, this is always true.}

\item{text.below.halign}{Horizontal alignment of \code{text.below}. There is no control
for vertical alignment because it always aligns with the edge of the window.}

\item{text.below.pad}{Numeric; Vertical space between \code{text.below} and edge of shape/icon in pixels.}

\item{text.below.xpad}{Numeric; Horizontal space between \code{text.below} and edge of shape/icon in pixels.}

\item{text.below.font.family}{Font family of \code{text.below}.}

\item{text.below.font.color}{Font color of \code{text.below}.}

\item{text.below.font.size}{Font size of \code{text.below}.}

\item{text.below.font.weight}{Weight of \code{text.below}, i.e. one of "bold" or "normal".}

\item{text.above}{Text to show above the Oval/Rectangle/Icon/Pictograph.}

\item{text.above.outside}{Whether \code{text.above} should be shown outside the Oval/Rectangle.
For Icon/Pictograph, this is always true.}

\item{text.above.halign}{Horizontal alignment of \code{text.above}. There is no control
for vertical alignment because it always aligns with the edge of the window.}

\item{text.above.pad}{Numeric; Vertical space between \code{text.above} and edge of shape/icon in pixels.}

\item{text.above.xpad}{Numeric; Horizontal space between \code{text.above} and edge of shape/icon in pixels.}

\item{text.above.font.family}{Font family of \code{text.above}.}

\item{text.above.font.color}{Font color of \code{text.above}.}

\item{text.above.font.size}{Font size of \code{text.above}.}

\item{text.above.font.weight}{Weight of \code{text.above}, i.e. one of "bold" or "normal".}

\item{border.color}{Color of the border around "Oval" or "Rectangle")}

\item{border.opacity}{Opacity of border, which should be between 0 and 1.}

\item{border.width}{Width of the border as a proportion of the graphic dimensions.}

\item{border.resolution}{The number of points used to define the border of the circle.}

\item{segment.gap}{Numeric between 0 and 1, specifying the gap between the segments
in the gauge.}

\item{hole.size}{Numeric between 0 and 1, specifying the size of the hole when \code{display}
is "Donut" or "Gauge".}

\item{tick.outside}{Whether to show the ticks inside or outside the gauge.}

\item{tick.show}{Whether to show the \code{minimum.value} and \code{maximum.value} when
\code{display} is "Gauge".}

\item{tick.font.family}{Font family of \code{tick}.}

\item{tick.font.color}{Font color of \code{tick}.}

\item{tick.font.size}{Font size of \code{tick}.}

\item{tick.number.type}{Format in which \code{x} should be shown.
One of "Automatic", "Number", "Percentage", "Scientific". If "Automatic" is used,
then a percentage format will be used if \code{attr(x, "statistic")} or
\code{attr(x, "format")} is "\%". Otherwise a number format will be used.}

\item{tick.decimals}{Integer; Number of decimals shown in ticks}

\item{tick.1000.separator}{String placed to separate thousands. By default this is a comma. Set to empty string to hide.}

\item{tick.prefix}{Optional text to prepend to ticks.}

\item{tick.suffix}{Optional text to append to ticks.}

\item{minimum.value}{Numeric value specifying the minimum value that \code{x} is expected to take.
This value is only used in "Gauge", "Bar" and "Pictograph (single icon)".
For "Donut" it is always assumed to be zero.}

\item{maximum.value}{Numeric value specifying the maximum that \code{x} will be expected to take.
This value is used to show proportional data
(i.e. \code{display} is "Donut", "Gauge" or "Pictograph (single icon)").}

\item{base.icon.color}{Color of the unfilled icons when \code{hide.base.image == FALSE}. Defaults to grey (#CCCCCC). Only applicable for built-in icons.}

\item{base.color}{The color of the base graphic when a Pictograph (with the base image shown)
or a Donut is displayed. For backwards compatibility, \code{base.icon.color} can also be used.}

\item{base.opacity}{Alpha transparency; only used when \code{display} is Donut.}

\item{background.color}{Color of the graphic background}

\item{background.opacity}{Transparency of background (0 to 1). This is only valid for
Number, Oval or Rectangle.}

\item{hover.text}{Optional text to show when the cursor hovers above widget.}

\item{hover.distance}{Deprecated.}

\item{hover.bg.color}{Color of the background of the hovertext.}

\item{hover.font.color}{Font color of \code{hover.text}.}

\item{hover.font.size}{Font size of \code{hover.text}.}

\item{hover.font.family}{Font family of \code{hover.text}.}

\item{font.unit}{Set to 'pt' (default) to get font sizing consistent with textboxes.
Otherwise fonts will be taken to be specified in pixels.}

\item{margin.left}{Spacing on the left of the graphic.}

\item{margin.right}{Spacing on the right of the graphic.}

\item{margin.top}{Spacing on top of graphic. This value overrides \code{margin}.}

\item{margin.bottom}{Spacing below graphic.}

\item{...}{Other parameters passed to \code{iconWithText}.}
}
\description{
Shows a number as an htmlwidget. The number can be shown on an oval/rectangle or icon.
}
\examples{
VisualizeNumber(4.0, display = "Rectangle", text.above = "Above", text.above.outside = TRUE)
VisualizeNumber(7.0, display = "Oval", label.data.prefix = "Number: ", label.data.suffix = ".",
     label.data.valign = "bottom", label.data.halign = "right", label.data.pad = 30)
VisualizeNumber(Sys.Date(), text.above = "The date is", text.below = "today.",
     global.font.color = "white", text.above.font.size = 10, text.below.font.size = 10,
     label.data.font.size = 20, background.color = "grey", background.opacity = 1.0)
VisualizeNumber(-7, text.below = "FROZEN<br>FOODS", global.font.color = "white",
     text.above.font.size = 10, text.below.font.size = 10, label.data.font.size = 20,
     background.color = "grey", background.opacity = 1.0, text.above.outside = FALSE,
     text.below.outside = FALSE)
}
\seealso{
PictographChart to create a chart or table of pictographs
}
