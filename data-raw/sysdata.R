# This script contains the data about the image data used in
# PictoStdChart() and SinglePicto()
# Data is stored as a data.frame available only internally
# Note that PictoChart takes URLs, and does not use imageURL

imageURL <- c(star="https://dl.dropboxusercontent.com/u/539177224/star_grey.svg",
              stickman="https://dl.dropboxusercontent.com/u/539177224/stickman_grey.svg",
              stickwoman="https://dl.dropboxusercontent.com/u/539177224/stickwoman_grey.svg",
              cow="https://dl.dropboxusercontent.com/u/539177224/cow_grey.svg",
              graduation="https://dl.dropboxusercontent.com/u/539177224/graduation_grey.svg",
              house="https://dl.dropboxusercontent.com/u/539177224/house%20_grey.svg",
              train="https://dl.dropboxusercontent.com/u/539177224/train_grey.svg")

imageWHRatio <- c(star=1, stickman=0.5, stickwoman=0.5, cow=0.75,
                  graduation=1, house=1, train=1)
save(imageURL, imageWHRatio, file = "R/sysdata.rda")
