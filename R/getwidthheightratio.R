#' @importFrom RCurl getURLContent
#' @importFrom bmp read.bmp
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom grDevices as.raster
getWidthHeightRatio <- function(image.url)
{
   # Download custom image to compute width-height ratio
    tmp.image <- getURLContent(image.url)
    tmp.type <- attr(tmp.image, "Content-Type")

    whratio <- NA
    if (grepl("svg", tmp.type))
    {
        tmp.w <- regmatches(tmp.image, regexpr("\\swidth=\"[0-9 .]+", tmp.image))
        tmp.h <- regmatches(tmp.image, regexpr("\\sheight=\"[0-9 .]+", tmp.image))
        if (length(tmp.w) != 0 && length(tmp.h) != 0)
        {
            ww <- as.numeric(gsub("\"", "", strsplit(split="=", tmp.w)[[1]][2]))
            hh <- as.numeric(gsub("\"", "", strsplit(split="=", tmp.h)[[1]][2]))
            whratio <- ww/hh
        }

        if (is.na(whratio))
        {
            tmp.str <- regmatches(tmp.image, regexpr("viewBox=\"[0-9 .]+", tmp.image))
            tmp.dim <- suppressWarnings(as.numeric(unlist(strsplit(split=" ", tmp.str))))
            whratio <- tmp.dim[3]/tmp.dim[4]
        }

    } else
    {
        tmp.file <- NULL
        if (grepl("png", tmp.type))
            tmp.file <- readPNG(tmp.image)
        if (grepl("jpeg", tmp.type))
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
