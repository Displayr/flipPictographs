% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pictographchart.R, R/pictostdchart.R
\name{PictographChart}
\alias{PictographChart}
\alias{PictoStdChart}
\title{Create chart of pictographs}
\usage{
PictographChart(
  x,
  image = "stickman",
  base.image = "",
  is.custom.url = FALSE,
  hide.base.image = FALSE,
  total.icons = NA,
  scale = NA,
  mode = "table",
  icon.palette = "Strong colors",
  icon.colors = "black",
  icon.custom.color = NA,
  icon.custom.gradient.start = NA,
  icon.custom.gradient.end = NA,
  icon.custom.palette = NA,
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
  hide.label.bottom = (mode != "column"),
  hide.label.top = (mode == "column"),
  label.color.asIcon = FALSE,
  label.left = NA,
  label.top = NA,
  label.bottom = NA,
  label.right = NA,
  label.pad = 0,
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
  label.data.bigmark = ",",
  label.data.prefix = "",
  label.data.suffix = "",
  label.data.100prc = FALSE,
  label.data.position = ifelse(mode == "bar", "Next to bar", "footer"),
  label.data.font.weight = "normal",
  label.data.font.size = 0.8 * label.font.size,
  label.data.font.color = label.font.color,
  label.data.align.horizontal = "default",
  label.data.type = "None",
  data.above.label = FALSE,
  ...
)

PictoStdChart(...)
}
\arguments{
\item{x}{Data to plot. Can be vector, matrix or data.frame.}

\item{image}{Name of icon (e.g. \code{"star", "stickman",...}) or URL to icon image if \code{is.custom.url}.}

\item{base.image}{URL of image to use as base image. Only used if \code{is.custom.url = TRUE} and \code{hide.base.image = FALSE}.}

\item{is.custom.url}{Whether the image parameter is a url supplied by the user.}

\item{hide.base.image}{Turns off background image (on by default). In general, the base image should only be shown if the input data is a proportion.}

\item{total.icons}{Maximum number of icons in each table cell. By default, it will be determine based on \code{ceiling(x)}.}

\item{scale}{Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.}

\item{mode}{Can be set to one of \code{"table", "bar", "column"}. For options \code{bar} and \code{column}, the chart is constrained to look like a bar or column chart. e.g For \code{mode  =  "column"}, 1-dimensional vectors/matrices are re-shaped to have multiple columns, labels are put below the graphis and icons are arranged vertically. Option \code{mode  =  "table"} is the most general and does not impose constraints.}

\item{icon.palette}{Name of palette used to color icons. Only applicable for in-built icons}

\item{icon.colors}{Vector of colors for icons. Only applicable when \code{icon.palette = "Custom color"} or {"Custom palette"} and in-built icons used (Deprecated).}

\item{icon.custom.color}{A single color which is used if \code{icon.palette} is set to \code{"Custom color"}.}

\item{icon.custom.gradient.start}{Character; starting color of gradient if \code{icon.palette} is set to \code{"Custom gradient"}.}

\item{icon.custom.gradient.end}{Character; last color of gradient if \code{icon.palette} is set to \code{"Custom gradient"}.}

\item{icon.custom.palette}{Character; comma separated list of colors to be used if \code{icon.palette} is set to \code{"Custom palette"}.}

\item{fill.direction}{Direction in which icons are filled. One of \code{"From left", "From right", "From top", "From bottom"}.}

\item{show.lines}{Add horizontal lines between each row}

\item{layout}{May be one of \code{"Number of rows"} or \code{"Number of columns"}. This parameter controls how the configuration of icons is specified. If no string is supplied, it will be automatically determined depending on whether \code{icon.nrow} or \code{icon.ncol} is supplied.}

\item{icon.nrow}{Configuration of icons in each table cell. Can be a single value or a vector with length equal to the number of rows.}

\item{icon.ncol}{Configuration of icons in each table cell. Can be a single value or a vector with length equal to the number of columns.}

\item{fix.icon.nrow}{When \code{mode="bar" and hide.base.image=T}, set to \code{FALSE} to allow the bars to contain varying number of rows.}

\item{table.by.row}{By default, when a 2-dimensional table is supplied, values in each column will be shown in the same color. Set to \code{TRUE} to color values by row.}

\item{row.names.to.remove}{List of rownames to exclude from the chart. This can be in the form of a vector or a comma-separated string. This variable is ignored if the input data has no rownames.}

\item{column.names.to.remove}{List of colnames to exclude from the chart.}

\item{show.legend}{Show legend (true or false).}

\item{legend.text}{Text shown with legend. If this string is empty, it will be automatically filled in using \code{scale}. (To hide text completely, use \code{legend.text  =  " "})}

\item{legend.icon.color}{Color of icon shown in legend.}

\item{pad.legend}{Horizontal spacing between the chart and the legend.}

\item{hide.label.right}{Suppress labels on right of graphics.}

\item{hide.label.left}{Suppress labels on left of graphics. By default, if \code{label.left} is not supplied, labels are taken from the rownames of \code{x}.}

\item{hide.label.bottom}{Suppress labels below graphics (shown by default when \code{mode = "column"}).}

\item{hide.label.top}{Suppress labels above graphics.}

\item{label.color.asIcon}{When set to \code{TRUE}, row and data labels are shown in the same color as the icons.}

\item{label.left}{Length must be equal to length (if \code{x} is a vector) or number of rows (if \code{x} is matrix or data.frame) as x. If no value is supplied, labels will be read from names/rowname of \code{x}. To suppress labels, use \code{label.left  =  NULL}.}

\item{label.top}{By default, labels are read from column names of \code{x}.}

\item{label.bottom}{Optional labels below graphic cells. The length of the labels must be the same as the number of columns in \code{x}.}

\item{label.right}{Optional labels to the right of graphic cells. The length of the labels must be the same as the number of rows in \code{x}.}

\item{label.pad}{Numeric specifying padding around the labels. Alternatively, the user can individually specify \code{label.left.pad} (horizontal space between left row label and icons), \code{label.right.pad} (horizontal space between right row label and icons).}

\item{label.left.pad, label.right.pad}{Horizontal spacing between row labels and icons.}

\item{label.left.align.horizontal, label.right.align.horizontal, label.top.align.horizontal, label.bottom.align.horizontal}{Horizontal alignment of row and column labels. One of \code{"left", "right"} or \code{"center"}.}

\item{label.left.align.vertical, label.right.align.vertical, label.top.align.vertical, label.bottom.align.vertical}{Vertical alignment of row and column labels. One of \code{"top", "bottom"} or \code{"center"}.}

\item{width.height.ratio}{Width-to-height ratio used to adjust row heights and column widths so they match the aspect ratio of the icon. Mis-specfication does not distort icon, but graphic will have extra spacing. When set to zero, row.height and column.width are unchanged, otherwise initial values are decreased to match \code{width.height.ratio}.}

\item{label.top.height}{Height of top label row in pixels.}

\item{label.font.family}{Controls font-family of all labels. To modify only the font of one label use \code{label.left.font.family, label.top.font.family}, etc.}

\item{label.font.size}{Size of label text.}

\item{label.font.color}{Colour of labels. This can be a string or a 6-digit hex code.}

\item{label.left.font.color, label.right.font.color, label.top.font.color, label.bottom.font.color}{Font color of row/column labels which overrides the global \code{label.font.color} setting.}

\item{label.left.font.size, label.right.font.size, label.top.font.size, label.bottom.font.size}{Font size of row/column labels which overrides the global \code{label.font.size} setting.}

\item{label.left.font.weight, label.right.font.weight, label.top.font.weight, label.bottom.font.weight}{Font weight of row/column labels which overrides the global \code{label.font.weight} setting.}

\item{show.label.data}{Boolean indicating whether or not to show data labels.}

\item{customize.label.data}{Boolean indicating whether of not users want to customize data labels. By default this is on, but when set to \code{FALSE}, parameters \code{label.data.digits, label.data.100prc, label.data.prefix, label.data.suffix} is ignored.}

\item{label.data.digits}{Number of digits to show after decimal place.}

\item{label.data.bigmark}{Option to prettify large numbers. By default a comma is placed after a thousand.}

\item{label.data.prefix}{String to prepend data label.}

\item{label.data.suffix}{String to append to data label.}

\item{label.data.100prc}{Option to show data labels multiplied by 100. This is useful when reporting percentages.}

\item{label.data.position}{When \code{show.label.data}, the position of the data labels can be one of \code{"Above icons", "Below icons"} (all modes) or \code{"Next to bar", "Above row label", "Below row label"} (bar mode only). Note that the last two options will overrride \code{sublabel.left} and \code{sublabel.right}}

\item{label.data.font.weight}{Weight of data labels, i.e. one of \code{"bold"} or \code{"normal"}.}

\item{label.data.font.size}{Font size of data labels.}

\item{label.data.font.color}{Font color of data labels.}

\item{label.data.align.horizontal}{Horizontal alignment of data labels.}

\item{label.data.type}{Does nothing. Retained for backwards compatibility.}

\item{data.above.label}{Set to \code{TRUE}, to place data labels above row labels.}

\item{...}{Arguments to pass to pictoChart}
}
\description{
\code{PictographChart} Create a chart to visually represent the data in a table or vector using the number of icons displayed

\code{PictoStdChart} Deprecated function for backwards-compatibility
}
\examples{
xx <- c(First = 3, Second = 6, Third=2)
PictographChart(xx, image="circle", mode="bar")
PictographChart(xx, image="elephant", hide.base.image=TRUE, show.label.data=TRUE, mode="bar")
PictographChart(xx, total.icons=10, mode="bar", fill.direction="fromright", is.custom.url=TRUE,
   image="http://wiki.q-researchsoftware.com/images/a/a9/Stick_woman_dark_red.png",
   base.image="http://wiki.q-researchsoftware.com/images/7/78/Stick_man_light_grey.png")
}
