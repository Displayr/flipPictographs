require(flipPictographs)

# default icon is star
s1 <- SinglePicto(2.5, 1, 5)
s1b <- SinglePicto(2.5, 1, 5, width=10)
s2 <- SinglePicto(2.5, 1, 5, hide.base.image = T)

# for a fixed width, size of icons remain constant
s3 <- SinglePicto(4.5, 2, 3, width=10)

s4 <- SinglePicto(4.5, 2, 3)
s4b <- SinglePicto(4.5, 2, 3, direction="fromleft")
s4c <- SinglePicto(4.5, 2, 3, direction="fromright")
s4d <- SinglePicto(4.5, 2, 3, direction="fromtop")
s4e <- SinglePicto(4.5, 2, 3, direction="frombottom")

# Dimensions change depending on aspect ratio of image
s5 <- SinglePicto(4.5, 2, 3, image="stickwoman")

s6 <- SinglePicto(2.5, 5, 1, image="stickman", bg.color="red")

# Autosize
s7 <- SinglePicto(2.5, 4, 1, image="stickman", bg.color="red", auto.size = T)

# Text sizes
s8 <- SinglePicto(2.5, 1, 5, text.type="percentage")
s8b <- SinglePicto(2.5, 1, 5, text.type="count", auto.size = T)
