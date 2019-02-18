set.seed(2)

xy <- data.frame(x = runif(10, min = 0, max = 200), y = runif(10, min = 0, max = 200))

# cut the plot in multiple part
coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
coord[1:4, ] <- coord[1:4, ] + 5000
coord[5:8, ] <- coord[5:8, ] + 6000
corner <- rep(c(1, 2, 4, 3), 2)
plot <- rep(c("plot1", "plot2"), each = 4)

cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)

plot <- rep(rep(c("plot1", "plot2"), each = 5))

dim <- c(200, 200)

context("Attribute the trees to the GPS coordinate")
test_that("error", {
  expect_error(attributeTreeCoord(xy, rep("plot1", 5), dim, cut), "length")
  expect_error(attributeTreeCoord(xy, plot, dim, cut[, 2:5]), "compulsory")
  expect_error(attributeTreeCoord(xy, rep("aa", length(plot)), dim, cut), "plot")
  expect_error(attributeTreeCoord(xy, plot, c(5, 10, 15), cut), "dimension")
})

test_that("function", {
  out <- attributeTreeCoord(xy, plot, dim, cut)
  expect_is(out, "data.frame")
  expect_equal(names(out), c("Xproj", "Yproj"))
  expect_true(all(!is.na(out)))

  # for the verification of the good work of this function
  xy <- as.data.frame(expand.grid(X = c(0, 100, 200), Y = c(0, 100, 200)))
  xy <- rbind(xy, xy)

  plot <- rep(rep(c("plot1", "plot2"), each = 9))

  out <- attributeTreeCoord(xy, plot, dim, cut)
  expect_equal(out, xy + rep(c(5000, 6000), each = 9), check.attributes = F)

  nCorner <- numberCorner(
    projCoord = coord,
    plot = rep(c("plot1", "plot2"), each = 4),
    origin = rep(c(T, F, F, F), 2),
    clockWise = F
  )

  out1 <- attributeTreeCoord(xy, plot, dim, nCorner)

  expect_equal(out1, out)


  # if the corner is in a different order and clockwise
  nCorner <- numberCorner(
    projCoord = coord,
    plot = rep(c("plot1", "plot2"), each = 4),
    origin = rep(c(F, F, T, F), 2),
    clockWise = T
  )

  out1 <- attributeTreeCoord(xy, plot, dim, nCorner)
  expect_failure(expect_equal(out1, out)) # check to see if the result is different from the begining
  
  # Do the table we expect to have for the result out1 (verified)
  X <- rep(seq(5000, 5200, by = 100), 3)
  X <- c(X, X + 1000)

  Y <- rep(seq(5200, 5000, by = -100), each = 3)
  Y <- c(Y, Y + 1000)

  expect_equal(out1, data.frame(X = X, Y = Y), check.attributes = F)
})
