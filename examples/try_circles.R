require(flipPictographs)

n <- 5
c.rgb <- colorRamp(c("red", "blue"))((1:n)/n)
c.hex <- rgb(c.rgb[,1], c.rgb[,2], c.rgb[,3], max=255)
x1 <- cbind(A=1:n, B=(1:n)+2, C=(1:n)+3)
rownames(x1) <- 1:n

t1 <- PictoChart(x1/(n+3), K=1, image.type="circle", direction="radial",
                 variable.image=c.hex, base.image="grey",
                 text.type="percentage", text.position = "overlay")

t1b <- PictoChart(x1/(n+3), K=1, image.type="circle", direction="radial",
                 variable.image=matrix(c.hex[c(1,3,5)], 5, 3, byrow=T), base.image="grey",
                 text.type="percentage", text.position = "overlay")

t2 <- PictoChart(x1/(n+3), K=1, image.type="circle", direction="radial",
                 variable.image="red", base.image="grey",
                 text.type="percentage", text.position = "overlay",
                 column.width = 20+5*n, row.height = 20+5*(1:n), show.lines=F)
n2 <- 3
t3 <- PictoChart(x1/(n+3), K=1, image.type="square", direction="radial",
                 variable.image="red", base.image="grey",
                 text.type="percentage", text.position = "overlay",
                 column.width = 20+5*(1:n2), row.height = 20+5*n2, show.lines=F)
