require(flipPictographs)

# Bar and column charts
x1 <- c(A=1, Bbbbbb=3.6, Cc=2.8)
q1 <- PictoStdChart(x1, show.legend=T)
q1b <- PictoStdChart(x1*100, show.legend=T)
q1c <- PictoStdChart(x1, show.as.column = T, bg.color="red")
q1d <- PictoStdChart(x1, show.as.column = T, show.lines=T, show.legend=T, icon.nrow=5)
q1e <- PictoStdChart(x1, show.as.column = F, show.lines=T, show.legend=T)

# Pyramids
x8 <- rbind(Top=c(1,0.5,5), Upper=c(3,5,11), Mid=c(15,17,20), Bottom=c(25,12,30))
colnames(x8) <- c("Women", "Old", "Total")
q8 <- PictoStdChart(x8, image="people.red", read.KfromX = T,
                    icon.halign="center", show.legend=T)
q8b <- PictoStdChart(x8, image="people.red", read.KfromX = T, transpose=T,
                    show.as.column = T, show.legend=T)
q8c <- PictoStdChart(x8, image="people.red", read.KfromX = T, direction="fromright",
                    icon.halign="center", show.legend=T)
q8d <- PictoStdChart(x8, image="people.red", read.KfromX = T, direction="frombottom",
                    icon.halign="center", show.legend=T)
q8e <- PictoStdChart(x8, image="people.red", read.KfromX = T, direction="frombottom",
                    icon.halign="center", show.legend=T, transpose=T)
q8f <- PictoStdChart(x8, image="people.red", read.KfromX = T, direction="frombottom",
                    icon.halign="center", show.legend=T, show.as.column = T, transpose=T)

# Aggregating
n <- 50
x9.y <- data.frame(A=1:n, B=(1:n)+0.5)
x9.x <- sample(1:4, n, replace=T)
q9 <- PictoStdChart(x9.y, groupBy=x9.x, image="drink", transpose=F)
q9b <- PictoStdChart(x9.y, groupBy=x9.x, image="drink", transpose=T, bg.color="green", pad.icon.col=0.5)

# Names and aggregating
q10 <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), transpose=F)
q10b <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), transpose=T)
q11 <- PictoStdChart(x=1:30, groupBy=rep(1:3, each=10), show.as.column=T, transpose=T)
