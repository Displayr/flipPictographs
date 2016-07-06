require(flipPictographs)

# Barcharts
# Categorical variables
f1 <- rep(c('a','b','c','d'), times=c(5,5,2,4))
b1 <- PictoStdChart(f1, mode="bar", show.legend=T)

f2 <- c(First=4, S=3, Last=1)
b2 <- PictoStdChart(f2, mode="bar", show.legend=T)

f3 <- cbind(A=0.01, B=0.5, C=0.1)
b3 <- PictoStdChart(f3, mode="bar", show.legend=T, hide.base.image=T)
