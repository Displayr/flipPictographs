require(flipPictographs)

# default icon is star
s1 <- SinglePicto(2.5, 1, 5)
s1b <- SinglePicto(2.5, 1, 5, width=10)
s2 <- SinglePicto(2.5, 1, 5, hide.base.image = T)

# for a fixed width, size of icons remain constant
s3 <- SinglePicto(4.5, 2, 3)

s4 <- SinglePicto(4.5, 2, 3, direction="vertical")

# Dimensions change depending on aspect ratio of image
s5 <- SinglePicto(4.5, 2, 3, image="people.red")

s6 <- SinglePicto(2.5, 5, 1, image="people", bg.color="red")

# Autosize
s7 <- SinglePicto(2.5, 2, 3, image="people", bg.color="red", auto.size = T)
