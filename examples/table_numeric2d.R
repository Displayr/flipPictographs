xx <- data.frame(A=1:3, B=2:4, C=3:5)
rownames(xx) <- c("i", "ii", "iii")

p1 <- PictoStdChart(xx)

# Scaling
p2 <- PictoStdChart(xx*10, show.legend=T)
