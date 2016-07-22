require(flipPictographs)

x2 <- cbind(A=1:3, B=2:4, C=c(1,0,1))
rownames(x2) <- c("R1", "R2", "R3")
t1 <- PictoStdChart(x2, stack=T, gradient.col1="deepskyblue", gradient.col2 = "orange")
t2 <- PictoStdChart(x2+0.5, stack=T,gradient.col1="deepskyblue", gradient.col2 = "orange")
t2b <- PictoStdChart(x2+0.5, stack=T, mode="column",gradient.col1="deepskyblue", gradient.col2 = "orange")
t3 <- PictoStdChart(x2, stack=T, mode="column",gradient.col1="deepskyblue", gradient.col2 = "orange")

x4 <- cbind(A=1:3, B=2:4, C=c(0,0,1))
t4 <- PictoStdChart(x4, stack=T, mode="column")

x5 <- cbind(A=4:1, B=1:4, C=c(1,0,0,1))
t5 <- PictoStdChart(x5, stack=T, mode="column", show.legend=T)

x6 <- rbind(A=1:3, B=c(3, 2.5, 0.5))
t6 <- PictoStdChart(x6, stack=T, show.legend=T)
