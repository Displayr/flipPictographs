# This script contains the data about the image data used in
# PictoStdChart() and SinglePicto()
# Data is stored as a data.frame available only internally
# Note that PictoChart takes URLs, and does not use imageURL

imageWHRatio <- c(apple=0.9,baby=0.8,banana=1,bank=1,barn=1.2,book=1,
                  bread=1,building=1,cake=1,car=1.4,cash=0.9,
                  chicken=1,church=0.8,citygate=1, computer=1.2,cow=1.2,
                  cross=1, cup=1,cutlery=0.7,dna=0.5,elephant=1.5,
                  fuel=1,globe=1,government=0.8,graduation=1.3,
                  gun=1.2,heart=1,house=1,idea=1,law=1,medicine=1,money=1,
                  police=0.9,renewenergy=1,road=0.8,rocket=1,
                  soldier=0.5,soup=1,sport=0.8,stack=18,star=1,stickman=0.5,stickwoman=0.5,
                  testtube=1,thumbsup=1,thumbsdown=1,tick=1.1,
                  tomato=1,tools=1,trafficlight=0.6,
                  train=0.7,tree=0.7,truck=1.5,tv=1,user=0.9,waterdrop=0.7,weight=1,
                  circle=1, square=1)

image.names <- names(imageWHRatio)
imageURL <- sprintf("https://dl.dropboxusercontent.com/u/539177224/%s_grey.svg", image.names)
names(imageURL) <- image.names
imageURL["circle"] <- ""
imageURL["square"] <- ""

save(imageURL, imageWHRatio, file = "R/sysdata.rda")
