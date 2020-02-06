#' @importFrom httr GET content
#' @importFrom bmp read.bmp
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom grDevices as.raster
getWidthHeightRatio <- function(image.url)
{
   # Download custom image to compute width-height ratio
    response <- try(GET(image.url), silent = TRUE)
    if (inherits(response, "try-error"))
        stop("Could not retrieve image from '", image.url, "'. Check that url is correct.")
    if(response$status_code != 200)
        stop("Error (status code ", response$status_code, ") retrieving image ", image.url)
    tmp.type <- response$headers$'content-type'
    if (any(grepl("text/html", tmp.type, fixed = TRUE)))
        stop("The url content type is 'text/html'. Ensure the image url is correct and not redirected.")
    # Give warning because sometimes chrome can fix this, but will show as blank in IE
    unknown.type <- !any(grepl("image", tmp.type, fixed = TRUE))
    if (unknown.type)
        warning("URL content type is '", tmp.type,
        "'. This may not display properly in all browsers.")

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
        else if (grepl("jpeg|jpg", tmp.type))
            tmp.file <- readJPEG(tmp.image)
        else if (grepl("bmp", tmp.type))
            tmp.file <- as.raster(read.bmp(tmp.file), max=255)
        else if (grepl("png", image.url))
            tmp.file <- readPNG(tmp.image)
        else if (grepl("jpeg|jpg", image.url))
            tmp.file <- readJPEG(tmp.image)
        else if (grepl("bmp", image.url))
            tmp.file <- as.raster(read.bmp(tmp.file), max=255)


        tmp.dim <- dim(tmp.file)
        if (length(tmp.dim) >= 1)
            whratio <- tmp.dim[2]/tmp.dim[1]
    }

    if (is.null(whratio) || is.na(whratio))
    {
        whratio <- 1
        if (!unknown.type)
            warning("Could not determine width-height ratio from image. Defaulting to 1.\n")
    }
    return(whratio)
}
