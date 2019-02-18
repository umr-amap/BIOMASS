set.seed(2)

xy <- data.frame(x = runif(10, min = 0, max = 200), y = runif(10, min = 0, max = 200))

# cut the plot in multiple part
coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
coord[1:4, ] <- coord[1:4, ] + 5000
coord[5:8, ] <- coord[5:8, ] + 6000
corner <- rep(c(1, 2, 4, 3), 2)
plot <- rep(c("plot1", "plot2"), each = 4)

cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)

plot = rep(rep(c("plot1", "plot2"), each = 5))

dim = c(200, 200)

context("Attribute the trees to the GPS coordinate")
test_that("error", {
  
  expect_error(attributeTreeCoord(xy, rep("plot1", 5), dim, cut), "length")
  expect_error(attributeTreeCoord(xy, plot, dim, cut[, 2:5]), "compulsory")
  expect_error(attributeTreeCoord(xy, rep("aa", length(plot)), dim, cut), "plot")
  expect_error(attributeTreeCoord(xy, plot, c(5, 10, 15), cut), "dimension")
  
})

test_that("function", {
  
  out = attributeTreeCoord(xy, plot, dim, cut)
  expect_is(out, "data.frame")
  expect_equal(names(out), c("Xproj", "Yproj"))
  expect_true(all(!is.na(out)))
  
  
  expand.grid(X = c(0, 200), Y = c(0, 200))
  
})











