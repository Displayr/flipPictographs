# In StdRPage set hide.base.image=T

x1 <- c(First=1, SecondLonger=4.5, Third=3)
p1 <- PictoStdChart(x1, mode="column")

x2 <- 100*x1
p2 <- PictoStdChart(x1)
p2b <- PictoStdChart(x1, show.legend=T, mode="column")
p2c <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, legend.icon.color="red")
p2d <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green")
p2e <- PictoStdChart(x1, show.legend=T, mode="column", pad.legend=3, background.color="green", pad.icon.col=0.5)

#0 Need to see what happens with fonts after the image slot is resized!
