require(flipPictographs)

# Handling a vector of factors
# K is set to be the length of the factor excluding NAs
# This is appropriate for both pick-one questions

f1 <- factor(sample(1:4, size=150, replace=T), labels=c("None", "Some", "Mostly", "All"))
pf1 <- PictoStdChart(f1)
pf1b <- PictoStdChart(f1, hide.base.image = T)
pf1c <- PictoStdChart(f1, hide.base.image = T, label.data.type="percentage", show.legend=T, label.data.align.horizontal="left")
pf1d <- PictoStdChart(f1, hide.base.image = T, label.data.type="count", show.legend=T)
pf1e <- PictoStdChart(f1, hide.base.image = T, label.data.type="proportion", show.legend=T)

# For bar and column the defaults set on the Std R Page should be
# show.legend=T, hide.base.image=T

b1 <- PictoStdChart(f1, mode="bar", show.legend=T,
                    label.data.type="percentage", label.data.position="footer", label.data.align.horizontal="center")

b2 <- PictoStdChart(f1, mode="bar", show.legend=T, gradient.col1="blue", gradient.col2="orange",
                    label.font="helvetica", label.size=16, label.weight="bold",
                    label.data.type="percentage", label.data.align.horizontal="left")

c1 <- PictoStdChart(f1, mode="column", show.legend=T, gradient.col1="blue", gradient.col2="orange",
                    label.data.type="percentage", label.data.position="footer", label.data.align.horizontal="left")

# What about pick-any questions?
# Also need to look at function calls using the 'by' parameter
