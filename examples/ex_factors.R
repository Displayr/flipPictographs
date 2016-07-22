require(flipPictographs)

# Handling a vector of factors
# K is set to be the length of the factor excluding NAs
# This is appropriate for both pick-one and pick-any questions

f1 <- factor(sample(1:4, size=150, replace=T), labels=c("None", "Some", "Mostly", "All"))
pf1 <- PictoStdChart(f1)
pf1b <- PictoStdChart(f1, hide.base.image = T)
pf1c <- PictoStdChart(f1, hide.base.image = T, text.type="percentage", show.legend=T, text.halign="left")
pf1d <- PictoStdChart(f1, hide.base.image = T, text.type="count", show.legend=T)
pf1e <- PictoStdChart(f1, hide.base.image = T, text.type="proportion", show.legend=T)

# For bar and column the defaults set on the Std R Page should be
# show.legend=T, hide.base.image=T

b1 <- PictoStdChart(f1, mode="bar", show.legend=T,
                    text.type="percentage", text.position="footer", text.halign="center")

b2 <- PictoStdChart(f1, mode="bar", show.legend=T, gradient.col1="blue", gradient.col2="orange",
                    label.font="helvetica", label.size=16, label.weight="bold",
                    text.type="percentage", text.halign="left")

c1 <- PictoStdChart(f1, mode="column", show.legend=T, gradient.col1="blue", gradient.col2="orange",
                    text.type="percentage", text.position="footer", text.halign="left")
