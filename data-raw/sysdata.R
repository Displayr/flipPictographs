# This script contains the data about the image data used in
# PictoStdChart() and SinglePicto()
# Data is stored as a data.frame available only internally
# Note that PictoChart takes URLs, and does not use imageURL

imageURL <- rbind(star = c("http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg",
                           "http://wiki.q-researchsoftware.com/images/f/f2/Star_unfilled.svg"),
                  people = c("http://wiki.q-researchsoftware.com/images/9/98/Stick_man_black.svg",
                             "http://wiki.q-researchsoftware.com/images/8/89/Stick_man_light_grey.svg"),
                  people.red = c("http://wiki.q-researchsoftware.com/images/0/00/Stick_man_dark_red.svg",
                                  "http://wiki.q-researchsoftware.com/images/8/89/Stick_man_light_grey.svg"),
                  drink = c("http://wiki.q-researchsoftware.com/images/3/3a/Cocktail.svg",
                             "http://wiki.q-researchsoftware.com/images/a/a5/Cocktail_light_grey.svg"))

colnames(imageURL) <- c("fg", "bg")
imageWHRatio <- c(star=1, people=0.5, people.red=0.5, drink=0.75)
save(imageURL, imageWHRatio, file = "R/sysdata.rda")
