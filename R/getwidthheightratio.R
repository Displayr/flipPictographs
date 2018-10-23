#' @importFrom httr GET content
#' @importFrom bmp read.bmp
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom grDevices as.raster
getWidthHeightRatio <- function(image.url)
{
   # Download custom image to compute width-height ratio
    response <- GET(image.url)
    if (inherits(response, "try-error") || response$status_code != 200)
        stop("Error (status code ", response$status_code, ") retrieving image ", image.url)

    tmp.type <- response$headers$'content-type'
    if ("text/html" %in% tmp.type)
        stop("Image type is text/html. Ensure the image url is correct and not redirected.")

    whratio <- NA
    if (grepl("svg", tmp.type))
    {

        # No warning is given because recoloring option is not available for pngs or jpegs
        # if (!any(grepl("fill", tmp.image)))
        #    warning("SVG image is missing fill attribute. Icon cannot be recolored\n")

        tmp.image <- content(response, as = "text", encoding = "UTF-8")
        tmp.image <- unlist(strsplit(split="<", tmp.image))[1:5]
        tmp.str <- regmatches(tmp.image, regexpr("viewBox=\"[0-9 .-]+", tmp.image))
        tmp.dim <- suppressWarnings(as.numeric(unlist(strsplit(split=" ", tmp.str))))
        whratio <- tmp.dim[3]/tmp.dim[4]

        if (is.na(whratio))
        {
            warning("SVG image is missing viewBox attribute. Aspect ratio may not be preserved.")
            tmp.w <- regmatches(tmp.image, regexpr("\\swidth=\"[0-9 .]+", tmp.image))
            tmp.h <- regmatches(tmp.image, regexpr("\\sheight=\"[0-9 .]+", tmp.image))
            if (length(tmp.w) != 0 && length(tmp.h) != 0)
            {
                ww <- as.numeric(gsub("\"", "", strsplit(split="=", tmp.w)[[1]][2]))
                hh <- as.numeric(gsub("\"", "", strsplit(split="=", tmp.h)[[1]][2]))
                whratio <- ww/hh
            }
        }

    } else
    {
        tmp.file <- NULL
        tmp.image <- content(response, as = "raw", encoding = "UTF-8")
        if (grepl("png", tmp.type))
            tmp.file <- readPNG(tmp.image)
        if (grepl("jpeg|jpg", tmp.type))
            tmp.file <- readJPEG(tmp.image)
        if (grepl("bmp", tmp.type))
            tmp.file <- as.raster(read.bmp(tmp.file), max=255)

        tmp.dim <- dim(tmp.file)
        whratio <- tmp.dim[2]/tmp.dim[1]
    }

    if (is.na(whratio))
    {
        whratio <- 1
        warning("Could not determine width-height ratio from image. Defaulting to 1.\n")
    }
    return(whratio)
}
