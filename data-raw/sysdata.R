# This script contains the data about the image data used in
# PictoStdChart() and SinglePicto()
# Data is stored as a data.frame available only internally
# Note that PictoChart takes URLs, and does not use imageURL

imageWHRatio <- c(baby=0.8,barn=1.2,book=1,building=1,
                 church=0.8,citygate=1, cow=1.2,dna=0.5,
                 fuel=1,globe=1,government=0.8,graduation=1.3,
                 gun=1.2,house=1,idea=1,law=1,medicine=1,
                 police=0.9,renewenergy=1,road=0.8,rocket=1,
                 soldier=0.5,star=1,stickman=0.5,stickwoman=0.5,
                 testtube=1,tools=1,trafficlight=0.6,
                 train=0.9,tree=1,tv=1,waterdrop=0.7)

image.names <- names(imageWHRatio)
imageURL <- sprintf("https://dl.dropboxusercontent.com/u/539177224/%s_grey.svg", image.names)
names(imageURL) <- image.names

save(imageURL, imageWHRatio, file = "R/sysdata.rda")
