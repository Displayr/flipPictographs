require(flipPictographs)

f1 <- factor(sample(1:4, size=100, replace=T), labels=c("None", "Some", "Mostly", "All"))
pf1 <- PictoStdChart(f1)

#f2 <- factor(sample(1:4, size=100, replace=T), labels=c("None", "Some", "Mostly", "All"))
#df1 <- data.frame(f1, f2)
