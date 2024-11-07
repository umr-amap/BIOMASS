context("Bilinear interpolation")


test_that("bilinear_interpolation error", {
  from_corner_coord <- data.frame(x=1:5,y=1:5)
  to_corner_coord <- data.frame(Xproj=1:4,Yproj=1:4)
  expect_error(bilinear_interpolation(coord = NULL, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord) , "from_corner_coord and to_corner_coord must have 4 rows representing the 4 corners of the plot")
  expect_error(bilinear_interpolation(coord = NULL, from_corner_coord = to_corner_coord, to_corner_coord = from_corner_coord) , "from_corner_coord and to_corner_coord must have 4 rows representing the 4 corners of the plot")
  from_corner_coord <- data.frame(x=c(0,0,10,10), y=c(0,10,10,0))
  to_corner_coord <- from_corner_coord+50
  coord <- c(5,5)
  expect_error(bilinear_interpolation(coord = coord, from_corner_coord = to_corner_coord, to_corner_coord = from_corner_coord) , "tree coordinates must be a data.frame, a matrix or a data.table")
  coord <- data.frame(x=5,y=5)
  # Error when from_corner_coord is not a rectangle
  from_corner_coord[3,1] <- 11
  expect_error(bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord) , "You may consider using trustGPScorners = F")
})

test_that("bilinear_interpolation function", {
  
  # Test when matrices are supplied and to_corner_coord colnames are not
  from_corner_coord <- as.matrix(expand.grid(X = c(0, 100), Y = c(0, 50)))
  rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
  to_corner_coord <- as.matrix(from_corner_coord) %*% rot_mat
  to_corner_coord <- sweep(to_corner_coord, 2, c(50,100), FUN = "+")
  coord <- as.matrix(data.frame(x=c(25,60),y=c(25,40)))
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
  #plot(coord, xlim=c(-10,150),ylim=c(-5,200), col="blue") ; points(from_corner_coord) ; points(proj_coord , col="purple") ; points(to_corner_coord, col="red")
  
  expect_is(proj_coord,"data.frame")
  expect_equal(nrow(proj_coord), nrow(coord))
  expect_equal(names(proj_coord), c("x_interp","y_interp"))
  expect_equal(proj_coord, data.frame(x_interp=c(59.15064,81.96152),y_interp=c(134.1506,164.6410)) , tolerance = 1e-6)
  
  # Test when to_corner_coord colnames are supplied 
  colnames(to_corner_coord) <- c("Xproj","Yproj")
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  expect_equal(names(proj_coord), c("Xproj","Yproj"))
  
  # Corner deformation
  set.seed(52)
  to_corner_coord <- to_corner_coord + runif(1,-10,10) 
  # to_corner_coord <- from_corner_coord
  # to_corner_coord[2,] = c(103,-5)
  # to_corner_coord[3,] = c(10,52)
  # to_corner_coord[4,] = c(95,48)
  # to_corner_coord <- sweep(to_corner_coord, 2, c(50,100), FUN = "+")
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  expect_equal(proj_coord, data.frame(Xproj=c(52.39103,75.20192),Yproj=c(127.3910,157.8814)) , tolerance = 1e-6)
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
  #plot(from_corner_coord, xlim=c(-10,150),ylim=c(-5,200)) ; points(coord , col="blue") ; points(proj_coord , col="purple") ; points(to_corner_coord, col="red")
  
  # If the origin is not (0;0)
  from_corner_coord <- expand.grid(X = c(50, 150), Y = c(50, 100))
  to_corner_coord <- sweep(from_corner_coord, 2, c(60,60), FUN = "+")
  coord <- data.frame(x=c(25,60)+50,y=c(25,40)+50)
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5)) + 50
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  #plot(from_corner_coord , xlim=c(45,210),ylim=c(45,160) , asp=1) ; points(coord , col="blue") ; points(proj_coord , col="purple") ; points(to_corner_coord, col="red")
  expect_equivalent(proj_coord, coord+60)
  
  # To negative coordinates : 
  from_corner_coord <- expand.grid(X = c(0, 100), Y = c(0, 50))
  rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
  to_corner_coord <- as.matrix(from_corner_coord) %*% rot_mat
  to_corner_coord <- sweep(to_corner_coord, 2, c(150,100), FUN = "-")
  coord <- data.frame(x=c(25,60),y=c(25,40))
  #coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  #plot(from_corner_coord , xlim=c(-200,110), ylim=c(-110,60) , asp=1) ; points(coord , col="blue") ; points(proj_coord , col="purple") ; points(to_corner_coord, col="red")
  expect_equal(proj_coord, data.frame(x_interp=c(-140.8494 ,-118.0385 ),y_interp=c(-65.84936,-35.35898)) , tolerance = 1e-6)
  
  # If the origin is the NE corner :
  to_corner_coord <- sweep(from_corner_coord, 2, c(60,60), FUN = "+")
  from_corner_coord <- from_corner_coord[c(4,3,2,1),]
  coord <- data.frame(x=c(25,60),y=c(25,40))
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  expect_equal(proj_coord, data.frame(X=c(135,100 ),Y=c(85,70)))
  
  # Test when there are multiple columns
  coord <- data.frame(coord)
  coord$col <- rep("",2)
  from_corner_coord <- data.frame(from_corner_coord)
  from_corner_coord$col <- rep("",4)
  to_corner_coord <- data.frame(to_corner_coord)
  to_corner_coord$col <- rep("",4)
  proj_coord = bilinear_interpolation(coord = coord, from_corner_coord = from_corner_coord, to_corner_coord = to_corner_coord)
  
})

