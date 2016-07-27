pictoStack <- function(x, image, mode, col1, col2, ...)
{
    # Assume scaling and conversions performed already using PictoStdChart
    if (nchar(col1)==0 || nchar(col2)==0)
        stop("Colors not specified\n")

    # By default, tables are stacked in bars
    if (mode=="column")
        x <- t(x)

    n <- if (is.null(nrow(x))) length(x)
         else nrow(x)
    m <- if (is.null(ncol(x))) 1
         else ncol(x)

    # Set up colorscale
    c.rgb <- colorRamp(c(col1, col2))(seq(0,1,length=m))
    c.hex <- rgb(c.rgb[,1], c.rgb[,2], c.rgb[,3], maxColorValue=255)
    c.hex <- c(c.hex, "")

    # Compute transformed matrices, as for a barchart
    m2 <- ceiling(max(apply(x, 1, sum)))
    x2 <- matrix(0, n, m2)
    rownames(x2) <- rownames(x)
    c.fg <- matrix(c.hex[1], n, m2)
    c.bg <- matrix("", n, m2)

    for (i in 1:n)
    {
        i.cum <- cumsum(x[i,])
        k <- which.max(i.cum > 0)
        for (j in 1:m2)
        {
            if (k > m)
                next
            c.fg[i,j] <- c.hex[k]
            x2[i,j] <- max(0, min(1, i.cum[k] - j + 1), 0)
            while (k <= m && i.cum[k] < j)
                k <- k +1
            c.bg[i,j] <- c.hex[k]
        }
    }

    pad.col <- 0
    pad.row <- 5
    if (mode=="column")
    {
        pad.col <- 5
        pad.row <- 0
        x2 <- t(x2[,m2:1])
        c.fg <- t(c.fg[,m2:1])
        c.bg <- t(c.bg[,m2:1])
    }
    c.fg <- paste(c.fg, ":", imageURL[image], sep="")
    c.bg <- ifelse(nchar(c.bg) > 0, paste(c.bg, ":", imageURL[image], sep=""), "")

    return(PictoChart(x2, image.type="url",
                      variable.image=c.fg, base.image=c.bg, pad.col=pad.col, pad.row=pad.row,
                      K=1, icon.nrow=1, icon.ncol=1, wh.ratio=0, ...))
}


