xx <- data.frame(A=1:3, B=2:4, C=3:5)
rownames(xx) <- c("i", "ii", "iii")

p1 <- PictoStdChart(xx)

# Scaling
p2 <- PictoStdChart(xx*10, show.legend=T)

# Layout
x3 <- c(Mixed=2, Bus=9, Cyclists=14, Pedestrians=19,
        SingleLaneBus=20, LightRail=22, DoubleLaneBus=43,
        HeavyRail=80, SuburbanRail=100)

p3 <- PictoStdChart(x3, transpose=T, image="stickman", scale=1, hide.base.image = T,
                    icon.ncol=8, fill.direction="frombottom",
                    label.data.type="count", label.data.align.horizontal = "center")

p3b <- PictoStdChart(x3, transpose=T, image="stickman", scale=1, hide.base.image = T,
                    icon.ncol=8, fill.direction="frombottom", hide.label.top = T,
                    label.data.type="count", label.data.align.horizontal = "center")


# Spacing
# This example has been implemented, but the load it too heavy to run
# Most of the load problems are from the AJAX recoloring request
x4 <- matrix(c(2500,50,70,560,100,650,650,2500,1700,450,
               2500,2400,750,650,125,1000,90,840,150,720,
               4650,1830,1440,1200,1170,2500,4250,2700,5000,200),
             byrow=T, ncol=5)
p4 <- PictoStdChart(x4, icon.ncol=10, scale=50, total.icons=500,
                    label.data.type="count", label.data.align.horizontal="right",
                    pad.row=200, pad.col=200)
