# In StdRPage set hide.base.image=T

x1 <- c(First=1, SecondLonger=4.5, Third=3)
p1 <- PictoStdChart(x1, mode="column")

x2 <- 100*x1
p2 <- PictoStdChart(x1)
p2b <- PictoStdChart(x1, show.legend=T, mode="column")
p2c <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, legend.icon.color="red")
p2d <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green", pad.col=0)
p2e <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green", pad.icon.col=0.5)
p2f <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green", pad.icon.row=0.5)
p2g <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green", pad.col=20)
p2h <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green", pad.row=20, pad.icon.row=0.5)

# Padding between column
p3 <- PictoStdChart(x2, mode="column", image="circle", hide.base.image = T, fill.direction = "frombottom",
                    icon.ncol=2, pad.col=100, icon.palette = "Blues", show.legend=T)
