# In StdRPage set hide.base.image=T

x1 <- c(First=1, SecondLonger=4.5, Third=3)
x2 <- 100*x1

p1 <- PictoStdChart(x1, mode="bar")

p2 <- PictoStdChart(x1)
p2b <- PictoStdChart(x1, show.legend=T, mode="bar")
p2c <- PictoStdChart(x1, show.legend=T, mode="bar", pad.legend=3, legend.icon.color="red")
p2d <- PictoStdChart(x1, show.legend=T, mode="bar", pad.legend=3, background.color="green")
p2e <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green")
p2f <- PictoStdChart(x2, show.legend=T, mode="bar", background.color="green", hide.label.right=F, pad.legend=0)

# Padding
p3a <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green", pad.icon.row=0.5)
p3c <- PictoStdChart(x1, total.icons=10, icon.ncol=2, show.legend=T, mode="bar", pad.legend=3, background.color="green", pad.icon.row=0.5, show.lines=T)
p3b <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green", pad.icon.col=0.5)

# Data labels (error)
p4a <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green", label.data.type="count")
p4b <- PictoStdChart(x1, show.legend=T, mode="bar", label.data.type="percentage")

p5 <- PictoStdChart(x1, show.legend=T, mode="bar", hide.label.right = F)
p5b <- PictoStdChart(x1, show.legend=T, mode="bar", hide.label.right = F, hide.label.left=T)
p5c <- PictoStdChart(x1, show.legend=T, mode="bar", hide.label.right = F, hide.label.left=F)


# Sublabels (direct)
lab2 <- letters[1:3]
p6 <- PictoStdChart(x1, label.left2=lab2, label.font.family="Impact")
p6b <- PictoStdChart(x1, label.left2=lab2, label.left.font.family="Impact", label.left.font.size=20)
p6c <- PictoStdChart(x1, label.left2=lab2, label.left.font.family="Impact", label.left.font.size=20, label.left2.font.size=5)

p7 <- PictoStdChart(x1, mode="bar", hide.label.right = F, label.font.family = "Impact")
p7b <- PictoStdChart(x1, mode="bar", hide.label.right = F, label.font.family = "Impact",
                     label.right2=lab2, label.right.font.size=20)
p7c <- PictoStdChart(x1, mode="bar", hide.label.right = F, label.font.family = "Impact",
                     label.right2=lab2, label.right.font.size=20, label.right2.font.size=5)

# Moving data label to sublabels
p8 <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On left")
p8b <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On left", label.data.font.size=20)
p8c <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On left", label.data.font.size=20, label.font.family="Impact")
p8d <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On left", label.data.font.size=20, label.font.family="Impact", label.data.onTop=T)


p9 <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On right")
p9b <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On right", label.data.font.size=20)
p9c <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On right", label.data.font.size=20, label.font.family="Impact")
p9d <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On right", label.data.font.size=20, label.font.family="Impact", label.data.onTop=T)
p9e <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On right", label.data.font.size=20, label.font.family="Impact",
                     label.data.onTop=T, hide.label.right=F) #ugly!
p9f <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On right", label.data.font.size=20, label.font.family="Impact",
                     label.data.onTop=T, hide.label.right=F, label.right.align.horizontal = "left", label.data.align.horizontal = "left")

# Label colors
p10 <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On left", label.color.asIcon = T,
                     label.data.font.size=20, label.font.family="Impact",
                     label.data.onTop=T, label.left.align.horizontal = "right", label.data.align.horizontal = "right")

p10b <- PictoStdChart(x1, mode="bar", label.data.type="count", label.data.position="On left",
                     label.color.asIcon = T, icon.colors = "red,green,blue", icon.palette = "Other colors",
                     label.data.font.size=20, label.font.family="Impact",
                     label.data.onTop=T, label.left.align.horizontal = "right", label.data.align.horizontal = "right")



# Need to see what happens with fonts after the image slot is resized!
# Check other icons with WHratio != 1

# Examples
dat1 <- c(Upper=3, UpperMid=5, LowerMid=6, Lower=7)
ex1 <- PictoStdChart(dat1, mode="bar", hide.base.image=T, hide.label.left=T,
                     label.data.type="count", label.data.position="On left")

dat3 <- c('2012'=0.53, '2014'=0.41)
ex3 <- PictoStdChart(dat3, mode="bar", scale=0.01, icon.nrow=3,
                     hide.base.image=F, label.color.asIcon = T,
                     icon.palette="Other colors", icon.colors="grey,blue",
                     label.data.type="percentage",label.data.position = "On right")
