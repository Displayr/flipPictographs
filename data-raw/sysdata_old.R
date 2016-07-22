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
                             "http://wiki.q-researchsoftware.com/images/a/a5/Cocktail_light_grey.svg"),
                  graduation = c("http://wiki.q-researchsoftware.com/images/2/24/Graduation_black.svg",
                                 "http://wiki.q-researchsoftware.com/images/7/78/Graduation_grey.svg"),
                  city = c("http://wiki.q-researchsoftware.com/images/2/26/City_black.svg",
                           "http://wiki.q-researchsoftware.com/images/e/ee/City_grey.svg"),
                  building = c("http://wiki.q-researchsoftware.com/images/f/ff/Building_black.svg",
                               "http://wiki.q-researchsoftware.com/images/7/70/Building_grey.svg"),
                  house = c("http://wiki.q-researchsoftware.com/images/9/9e/House_black.svg",
                          "http://wiki.q-researchsoftware.com/images/f/fb/House_grey.svg"),
                  barn = c("http://wiki.q-researchsoftware.com/images/7/75/Barn_black.svg",
                           "http://wiki.q-researchsoftware.com/images/1/1e/Barn_grey.svg"),
                  citygate = c("http://wiki.q-researchsoftware.com/images/0/03/Citygate_black.svg",
                               "http://wiki.q-researchsoftware.com/images/8/82/Citygate_grey.svg"))

colnames(imageURL) <- c("fg", "bg")
imageWHRatio <- c(star=1, people=0.5, people.red=0.5, drink=0.75,
                  graduation=1, city=1, building=1, house=1, barn=1, citygate=1)
save(imageURL, imageWHRatio, file = "R/sysdata.rda")
