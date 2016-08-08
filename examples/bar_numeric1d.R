# In StdRPage set hide.base.image=T

x1 <- c(First=1, SecondLonger=4.5, Third=3)
p1 <- PictoStdChart(x1, mode="bar")

x2 <- 100*x1
p2 <- PictoStdChart(x1)
p2b <- PictoStdChart(x1, show.legend=T, mode="bar")
p2c <- PictoStdChart(x1, show.legend=T, mode="bar", pad.legend=3, legend.icon.color="red")
p2d <- PictoStdChart(x1, show.legend=T, mode="bar", pad.legend=3, background.color="green")
p2e <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green")

# Padding
p3a <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green", pad.icon.row=0.5)
p3b <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green", pad.icon.col=0.5)

# Data labels (error)
p4a <- PictoStdChart(x1, total.icons=10, show.legend=T, mode="bar", pad.legend=3, background.color="green", label.data.type="count")
p4b <- PictoStdChart(x1, show.legend=T, mode="bar", label.data.type="percentage")

# Need to see what happens with fonts after the image slot is resized!
# Check other icons with WHratio != 1
