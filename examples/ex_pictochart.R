#require("rhtmlPictographs")
#require("flipChartBasics")
#source("pictochart.R")
#source("pictostdchart.R")
require(flipPictographs)

# Images
star.filled <-  "http://wiki.q-researchsoftware.com/images/9/91/Star_filled.svg"
star.empty <- "http://wiki.q-researchsoftware.com/images/f/f2/Star_unfilled.svg"
ppl.filled <- "http://wiki.q-researchsoftware.com/images/9/98/Stick_man_black.svg"
ppl.red <- "http://wiki.q-researchsoftware.com/images/0/00/Stick_man_dark_red.svg"
ppl.grey <- "http://wiki.q-researchsoftware.com/images/8/89/Stick_man_light_grey.svg"
wine.filled <- "http://www.iconsdb.com/icons/preview/black/bar-2-xxl.png"
drink.filled <- "http://wiki.q-researchsoftware.com/images/3/3a/Cocktail.svg"
drink.grey <- "http://wiki.q-researchsoftware.com/images/a/a5/Cocktail_light_grey.svg"
drop.pic <- "http://wiki.q-researchsoftware.com/images/7/70/Water-drop.jpg"
sheep <- "http://wiki.q-researchsoftware.com/images/6/6a/Sheep-black.jpeg"
pig <- "http://wiki.q-researchsoftware.com/images/d/d4/Pig-black.png"
cow <- "http://wiki.q-researchsoftware.com/images/3/32/Cow-black.png"
chicken <- "http://wiki.q-researchsoftware.com/images/7/7d/Chicken-black.png"
fish <- "http://wiki.q-researchsoftware.com/images/d/d7/Fish-blue.png"

# Simple vector (movie ratings)
x1 <- c(First=1, Second=2, Third=3)
p1 <- PictoChart(x1, variable.image=star.filled, base.image=star.empty) #, column.width=100)
#p1b <- PictoStdChart(x1, image="star.filled", base.image="star.empty", show.legend=T)
#p1c <- PictoStdChart(x1, image="star.filled", base.image="star.empty",
#                     show.legend=T, transpose=T, show.lines=T)
#p1c <- PictoStdChart(x1, image="star.filled", base.image="star.empty",
#                     show.legend=T, transpose=T, show.lines=T, hide.label.top = T)


# Simple dataframe (drink size popularity) and autosize
x2 <- data.frame(Large=1:4, Medium=5:8, Small=9:12)
rownames(x2) <- 1:4
p2 <- PictoChart(x2-0.5, drink.filled, K=c(4,8,12))
                 #icon.nrow=c(1,2,3))  #, column.width=200,
                #label.left.size=8)#, label.left.width=50)

#  Cell padding (migration)
x3 <- data.frame(In=c(676,595,8645,17149,6759,3330,2256,1203),
                Out=c(1933,625,1854,15946,12471,4044,2224,1513))
rownames(x3) <- c('5-11','12-17','18-24','25-34','35-44','45-54','55-64','65 and over')

p3 <- PictoChart(x3/1000, ppl.filled, K=ceiling(x3/1000),
                icon.fixedsize = T, icon.halign=c("right", "left"),
                label.top=c("Migration in", "Migration out"), label.top.halign=c("right", "left"),
                label.left=rownames(x3), label.left.size=5,
                label.right=rownames(x3), label.right.size=5)

# Control sizing (wine consumption/production)
# 2014 data from wineinstitute.org (units=1000 L)
x4 <- data.frame(
    US=c(3217.5,3021),
    France=c(2790,4670),
    Italy=c(2040,4473),
    Germany=c(2020,849),
    China=c(1580,1117.8))
is.exporter <- x4[2,] > x4[1,]
K  <- apply(x4/100,2,max)
self.consumed <- apply(x4/100, 2, min)
extype <- ifelse(is.exporter, "Exporter", "Net importer")
excol <- ifelse(is.exporter, "black", "red")

p4 <- PictoChart(self.consumed, K=ceiling(K), variable.image = ppl.filled,
                 base.image=c(ppl.red,ppl.grey,ppl.grey,ppl.red,ppl.red),
                 icon.fixedsize = T,
                 label.left=names(self.consumed),
                 label.right=extype, label.right.color=excol)

# icon column and rows (water requirements)
# units = days and cm/day
x5 <- rbind(Rice=c(93,1.075),
            Wheat=c(88,0.425),
            Cotton=c(202, 0.525),
            Potato=c(88, 0.75),
            Tobacco=c(98,0.75))
tot.water <- x5[,1] * x5[,2]
nmonths <- floor(x5[,1]/30)
p5 <- PictoChart(tot.water, drop.pic, icon.fixedsize = T,
                 icon.nrow=nmonths, row.height=nmonths*30, icon.ncol=tot.water/nmonths,
                 pad.icon.row=0, pad.icon.col=0, pad.row=20)


# Multiple graphics (meat consumption)
# units =  Kilograms/capita , 2014
x6 <- rbind(Algeria=c(4,0,6.2,6.7),
            Argentina=c(41.6,8.7,35.1,1.2),
            Australia=c(21.6,20.1,39.6,9),
            Brazil=c(27, 11.9,38.7,1.7),
            Canada=c(18,17.1,33,0.9))
colnames(x6) <- c("Beef","Pork","Poultry","Sheep")
p6 <- PictoChart(x6/10, rep(c(cow,pig,chicken,sheep), each=nrow(x6)), show.lines=T,
                 label.top.halign="left")
#p6b <- PictoStdChart(x6, image="cow", base.image="none",
#                     show.lines=T, show.legend=T, scale=10)
#p6c <- PictoStdChart(x6, image="cow", base.image="none", transpose=T, direction="vertical",
#                     show.lines=T, show.legend=T, scale=10)

# Columns
x7 <- sample(1:10, 12, replace=T)
names(x7) <- format(seq(from=as.Date("2016-1-1"), length=12, by="month"), "%b")
p7 <- PictoChart(t(x7)+0.5, fish, direction="vertical",
                 icon.ncol = 1, column.width=10, label.top.size=2, label.top.height=2)

# Pyramids
x8 <- rbind(Top=c(1,5), Upper=c(3,11), Mid=c(15,20), Bottom=c(25,30))
colnames(x8) <- c("Women", "Total")
#p8 <- PictoStdChart(x8, image="ppl.red", base.image="ppl.grey", read.KfromX = T, icon.halign="center")
#p8b <- PictoStdChart(x8, image="ppl.red", base.image="ppl.grey", read.KfromX = T)
#p8c <- PictoStdChart(x8, image="ppl.red", base.image="ppl.grey", read.KfromX = T, transpose=F, direction="vertical")
#p8d <- PictoStdChart(x8, image="ppl.red", base.image="ppl.grey", read.KfromX = T, transpose=F, direction="vertical", icon.autosize=T)


# Aggregating
n <- 50
x9.y <- data.frame(A=1:n, B=(1:n)+0.5)
x9.x <- sample(1:4, n, replace=T)
#p9 <- PictoStdChart(x9.y, drink.filled, drink.grey, groupBy=x9.x, transpose=F)

#p10 <- PictoStdChart(x=x6/10, image="cow", base.image="none", show.legend=0, show.lines=TRUE)
#p10a <- PictoStdChart(x=x6/10, image="cow", base.image="none", show.legend=T, show.lines=T)
#p10b <- PictoStdChart(x=x6/10, image="cow", base.image="none", show.legend=T, show.lines=T, legend.text="10kg per capita")
#p10c <- PictoStdChart(x=x6/10, image="cow", base.image="none", show.legend=T, show.lines=T, legend.color="red")
#p10d <- PictoStdChart(x=x6/10, image="cow", base.image="none", show.legend=T, show.lines=T, legend.color="red", hide.label.left=T)
#p10e <- PictoStdChart(x=x6/10, image="cow", base.image="none", show.legend=T, show.lines=T, legend.color="red", hide.label.top=T)

x11 <- data.frame(A=1:10, B=1:5)
#p11 <- PictoStdChart(x11, image="ppl.red", base.image="ppl.grey", show.legend=T, show.lines=T)
#p11a <- PictoStdChart(x11[,1], image="ppl.red", base.image="ppl.grey", show.legend=T, show.lines=T)
#p11b <- PictoStdChart(x11[,1], image="ppl.red", base.image="ppl.grey", show.legend=T, show.lines=T, hide.label.left=T)

#library(base64enc)
#im.uri <- dataURI(file="C:/Users/carmen/Documents/Projects/rhtmlPictographs/theSrc/images/stickman_grey.svg")
#im2 <- gsub("data:;", "image/svg+xml;", im.uri)
#p12 <- PictoChart(x=c(A=1.5, B=2), variable.image=im2)
