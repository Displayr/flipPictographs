# Draw a segment of a half circle that fills a page from [0,0] to [1,1]
segmentPath <- function(x, hole.size, border.resolution, debug = FALSE)
{
    radius <- 0.5
    thetas <- pi * (1 - seq(from = x[1], to = x[2], length = border.resolution))
    xx.o <- radius * cos(thetas) + 0.5
    xx.i <- rev(radius * hole.size * cos(thetas) + 0.5)
    yy.o <- 2 * radius * sin(thetas)
    yy.i <- rev(2 * radius * hole.size * sin(thetas))
    path <- paste(paste("M", xx.o[1], yy.o[1]), paste("L", xx.o, yy.o, collapse = " "),
                  paste("L", xx.i[1], yy.i[1]), paste("L", xx.i, yy.i, collapse = " "), "Z")
    
    if (debug)
        cat(path, "\n")
    return(path)
}


# For drawing the border of a full circle filling page with corners [0,0] and [1,1]
circleBorder <- function(border.width, border.resolution, debug = FALSE)
{
    thetas <- pi * seq(from = -1, to = 1, length = border.resolution)
    xx.o <- 0.5 * cos(thetas) + 0.5
    xx.i <- rev(0.5 * (1 - 2*border.width) * cos(thetas) + 0.5)
    yy.o <- 0.5 * sin(thetas) + 0.5
    yy.i <- rev(0.5 * (1 - 2*border.width) * sin(thetas) + 0.5)

    path <- paste(paste("M", xx.o[1], yy.o[1]),
                  paste("L", xx.o, yy.o, collapse = " "),
                  paste("L", xx.i, yy.i, collapse = " "))
    if (debug)
        cat(path, "\n")
    return(path)
}

# For drawing the border of a rectangle filling page with corners [0,0] and [1,1]
rectangleBorder <- function(border.width, debug = FALSE)
{
    path <- paste("M 0 0 V 1 H 1 V 0 Z M", border.width, border.width,
        "V", 1-border.width, "H", 1-border.width, "V", border.width, "H", 1 - border.width)

    if (debug)
        cat(path, "\n")
    return(path)
}

pathToShape <- function(path, color, opacity)
{
    if (is.null(path))
        return(NULL)
    return(list(type = "path", path = path, line = list(width = 0),
                fillcolor = color, opacity = opacity))
}


