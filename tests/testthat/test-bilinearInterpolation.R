context("Bilinear interpolation")


test_that("bilinearInterpolation error", {
  fromCornerCoord <- data.frame(x=1:5,y=1:5)
  toCornerCoord <- data.frame(x=1:4,y=1:4)
  expect_error(bilinearInterpolation(coord = NULL, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord) , "fromCornerCoord and toCornerCoord must have 4 rows representing the 4 corners of the plot")
  expect_error(bilinearInterpolation(coord = NULL, fromCornerCoord = toCornerCoord, toCornerCoord = fromCornerCoord) , "fromCornerCoord and toCornerCoord must have 4 rows representing the 4 corners of the plot")
  fromCornerCoord <- data.frame(x=c(0,0,10,10), y=c(0,10,10,0))
  toCornerCoord <- fromCornerCoord+50
  coord <- c(5,5)
  expect_error(bilinearInterpolation(coord = coord, fromCornerCoord = toCornerCoord, toCornerCoord = fromCornerCoord) , "tree coordinates must be a data.frame, a matrix or a data.table")
  coord <- data.frame(x=5,y=5)
  fromCornerCoord[3,1] <- 11
  expect_error(bilinearInterpolation(coord = coord, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord) , "You may consider using trustGPScorners = F")
})

test_that("bilinearInterpolation function", {
  
  fromCornerCoord <- expand.grid(X = c(0, 100), Y = c(0, 50))
  rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
  toCornerCoord <- as.matrix(fromCornerCoord) %*% rot_mat
  toCornerCoord <- sweep(toCornerCoord, 2, c(50,100), FUN = "+")
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
  coord <- data.frame(x=c(25,60),y=c(25,40))
  projCoord = bilinearInterpolation(coord = coord, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord)
  #plot(coord, xlim=c(-10,150),ylim=c(-5,200), col="blue") ; points(fromCornerCoord) ; points(projCoord , col="purple") ; points(toCornerCoord, col="red")
  
  expect_is(projCoord,"data.frame")
  expect_equal(nrow(projCoord), nrow(coord))
  expect_equal(names(projCoord), c("X","Y"))
  
  expect_equal(projCoord, data.frame(X=c(59.15064,81.96152),Y=c(134.1506,164.6410)) , tolerance = 1e-6)
  
  
  # Corner deformation
  set.seed(52)
  toCornerCoord <- toCornerCoord + runif(1,-10,10) 
  # toCornerCoord <- fromCornerCoord
  # toCornerCoord[2,] = c(103,-5)
  # toCornerCoord[3,] = c(10,52)
  # toCornerCoord[4,] = c(95,48)
  # toCornerCoord <- sweep(toCornerCoord, 2, c(50,100), FUN = "+")
  projCoord = bilinearInterpolation(coord = coord, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord)
  expect_equal(projCoord, data.frame(X=c(52.39103,75.20192),Y=c(127.3910,157.8814)) , tolerance = 1e-6)
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
  #plot(fromCornerCoord, xlim=c(-10,150),ylim=c(-5,200)) ; points(coord , col="blue") ; points(projCoord , col="purple") ; points(toCornerCoord, col="red")
  
  
  # If the origin is not (0;0)
  fromCornerCoord <- expand.grid(X = c(50, 150), Y = c(50, 100))
  toCornerCoord <- sweep(fromCornerCoord, 2, c(60,60), FUN = "+")
  coord <- data.frame(x=c(25,60)+50,y=c(25,40)+50)
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5)) + 50
  projCoord = bilinearInterpolation(coord = coord, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord)
  #plot(fromCornerCoord , xlim=c(45,210),ylim=c(45,160) , asp=1) ; points(coord , col="blue") ; points(projCoord , col="purple") ; points(toCornerCoord, col="red")
  expect_equivalent(projCoord, coord+60)
  
  # To negative coordinates : 
  fromCornerCoord <- expand.grid(X = c(0, 100), Y = c(0, 50))
  rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
  toCornerCoord <- as.matrix(fromCornerCoord) %*% rot_mat
  toCornerCoord <- sweep(toCornerCoord, 2, c(150,100), FUN = "-")
  coord <- data.frame(x=c(25,60),y=c(25,40))
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
  projCoord = bilinearInterpolation(coord = coord, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord)
  #plot(fromCornerCoord , xlim=c(-200,110), ylim=c(-110,60) , asp=1) ; points(coord , col="blue") ; points(projCoord , col="purple") ; points(toCornerCoord, col="red")
  expect_equal(projCoord, data.frame(X=c(-140.8494 ,-118.0385 ),Y=c(-65.84936,-35.35898)) , tolerance = 1e-6)
  
})

