require(flipPictographs)

# Bar and column charts
x1 <- c(A=1, Bbbbbb=3.6, Cc=2.8)
q1 <- PictoStdChart(x1, show.legend=T)
q1b <- PictoStdChart(x1*100, show.legend=T)
q1c <- PictoStdChart(x1, mode="column", bg.color="red")
q1d <- PictoStdChart(x1, mode="column", show.lines=T, show.legend=T, icon.nrow=5)
q1e <- PictoStdChart(x1, mode="none", show.lines=T, show.legend=T)
q1f <- PictoStdChart(x1, mode="column", show.lines=T, show.legend=T, text.type="percentage")
q1g <- PictoStdChart(x1, mode="bar", show.lines=T, show.legend=T, text.type="percentage")

# Pyramids
x8 <- rbind(Top=c(1,0.5,5), Upper=c(3,5,11), Mid=c(15,17,20), Bottom=c(25,12,30))
colnames(x8) <- c("Women", "Old", "Total")
q8 <- PictoStdChart(x8, image="stickman", read.KfromX = T,
                    icon.halign="center", show.legend=T)
q8b <- PictoStdChart(x8, image="stickman", read.KfromX = T, transpose=T,
                    mode="column", show.legend=T)
q8c <- PictoStdChart(x8, image="stickman", read.KfromX = T, direction="fromright",
                    icon.halign="center", show.legend=T)
q8d <- PictoStdChart(x8, image="stickman", read.KfromX = T, direction="frombottom",
                    icon.halign="center", show.legend=T)
q8e <- PictoStdChart(x8, image="stickman", read.KfromX = T, direction="frombottom",
                    icon.halign="center", show.legend=T, transpose=T)
q8f <- PictoStdChart(x8, image="stickman", read.KfromX = T, direction="frombottom",
                    icon.halign="center", show.legend=T, mode="column", transpose=T)

# Aggregating
n <- 50
x9.y <- data.frame(A=1:n, B=(1:n)+0.5)
x9.x <- sample(1:4, n, replace=T)
q9 <- PictoStdChart(x9.y, groupBy=x9.x, image="waterdrop", transpose=F)
q9b <- PictoStdChart(x9.y, groupBy=x9.x, image="waterdrop", transpose=T, bg.color="green", pad.icon.col=0.5)

# Names and aggregating
q10 <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), transpose=F)
q10b <- PictoStdChart(x=cbind(Test=1:30), groupBy=rep(1:3, each=10), mode="bar")
q11 <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), mode="column", transpose=T)
q11b <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), mode="column", transpose=F, text.type="count")
q11c <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), mode="column", text.type="percentage")
