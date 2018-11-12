UTMcoord = data.frame(X = c( runif(5, min = 9, max = 11), runif(5, min = 8, max = 12), runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)),
                      Y = c( runif(5, min = 9, max = 11), runif(5, min = 80, max = 120), runif(5, min = 8, max = 12), runif(5, min = 90, max = 110)))
UTMcoord = UTMcoord + 1000
CoordRel = data.frame(X = c(rep(0, 10), rep(100, 10)),
                      Y = c(rep( c(rep(0, 5), rep(100, 5)), 2)))

context("correct coord GPS")
test_that("correct coord GPS", {
  
  
  correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0,100), rangeY = c(0, 100))
  
  
})