require(flipPictographs)

# default icon is star
s1 <- SinglePicto(2.5, 5)
s1b <- SinglePicto(2.5, 5, icon.width=10)
s2 <- SinglePicto(2.5, 5, hide.base.image = T)

# for a fixed icon.width, size of icons remain constant
s3 <- SinglePicto(4.5, 6, icon.width=10)

s4 <- SinglePicto(4.5, 6, 3)
s4b <- SinglePicto(4.5, 6, 3, fill.direction="fromleft")
s4c <- SinglePicto(4.5, 6, 3, fill.direction="fromright")
s4d <- SinglePicto(4.5, 6, 3, fill.direction="fromtop")
s4e <- SinglePicto(4.5, 6, 3, fill.direction="frombottom")

# Dimensions change depending on aspect ratio of image
s5 <- SinglePicto(4.5, 6, 3, image="stickwoman")

s6 <- SinglePicto(2.5, 5, 5, image="stickman", background.color="red")

# Autosize
s7 <- SinglePicto(2.5, 4, 4, image="stickman", background.color="red", auto.size = T)

# Margins
s8  <- SinglePicto(2.5, 5, image="stickman", background.color="red", auto.size = T)
s8a <- SinglePicto(2.5, 5, image="stickman", background.color="red", auto.size = T, margin.top=10)
s8b <- SinglePicto(2.5, 5, image="stickman", background.color="red", auto.size = T, pad.col=0.9)
s8c <- SinglePicto(2.5, 5, image="stickman", background.color="red", auto.size = F)
s8d <- SinglePicto(2.5, 5, image="stickman", background.color="red", auto.size = F, margin=10)
s8e <- SinglePicto(2.5, 5, image="stickman", background.color="red", auto.size = F, margin.top=10, margin.left=10)

# Check ratios of built in images
i.list <- names(flipPictographs:::imageURL)
s9 <- list()
n <- length(i.list)
for (i in 1:n)
    s9[[i]] <- SinglePicto(9, 9, 3, image=i.list[i])
