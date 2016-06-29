context("PictoStdChart")

x1 <- c(A=1, Bbbbbb=3.6, Cc=2.8)
test_that("Simple barchart", {
    expect_error( PictoStdChart(x1, show.legend=T), NA)
})
test_that("Scaling works", {
    expect_error( PictoStdChart(x1*100, show.legend=T), NA)
})
test_that("Simple column chart", {
    expect_error( PictoStdChart(x1, show.as.column = T), NA)
})

x8 <- rbind(Top=c(1,5), Upper=c(3,11), Mid=c(15,20), Bottom=c(25,30))
colnames(x8) <- c("Women", "Total")
test_that("Parameter read.KfromX accepted", {
    expect_error( PictoStdChart(x8, image="people.red", read.KfromX = T,
                    icon.halign="center", show.legend=T), NA)
})
test_that("Parameter read.KfromX works with columns", {
    expect_error( PictoStdChart(x8, image="people.red", read.KfromX = T, transpose=T,
                    show.as.column = T, show.legend=T), NA)
})

n <- 50
x9.y <- data.frame(A=1:n, B=(1:n)+0.5)
x9.x <- sample(1:4, n, replace=T)
test_that("Parameter groupBy works", {
    expect_error( PictoStdChart(x9.y, groupBy=x9.x, image="drink", transpose=F), NA)
})

test_that("Parameter groupBy work with columns", {
    expect_error( PictoStdChart(x9.y, groupBy=x9.x, image="drink", transpose=T), NA)
})
