coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000
plot <- rep("plot1", 4)


context("number corner")
test_that("number corner", {
  corner <- numberCorner(projCoord = coord, plot = plot, origin = c(F, F, T, F), clockWise = T)
  expect_is(corner, "data.frame")
  expect_equal(dim(corner), c(4, 4))

  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(F, F, T, F), clockWise = T)$corner,
    c(4, 3, 1, 2)
  )
  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(F, F, T, F), clockWise = F)$corner,
    c(2, 3, 1, 4)
  )
  expect_equal(
    numberCorner(projCoord = coord[c(1, 4, 3, 2), ], plot = plot, origin = c(F, F, T, F), clockWise = F)$corner,
    c(2, 4, 1, 3)
  )
  expect_equal(
    numberCorner(projCoord = coord[c(1, 4, 3, 2), ], plot = plot, origin = c(F, F, T, F), clockWise = T)$corner,
    c(4, 2, 1, 3)
  )
  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(F, T, F, F), clockWise = T)$corner,
    c(2, 1, 3, 4)
  )
  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(F, T, F, F), clockWise = F)$corner,
    c(4, 1, 3, 2)
  )
})

test_that("number corner error", {
  expect_error(numberCorner())
  expect_error(numberCorner(longlat = "fdz", projCoord = "fze"))
  expect_error(numberCorner(longlat = coord[1:2, ], plot = plot, origin = c(F, F, T, F), clockWise = T))
  expect_error(numberCorner(projCoord = coord[1:2, ], plot = plot, origin = c(F, F, T, F), clockWise = T))
  expect_error(numberCorner(projCoord = coord, plot = plot[1:2], origin = c(F, F, T, F), clockWise = T))

  expect_error(numberCorner(projCoord = coord, plot = plot, origin = c(F, T, T, F), clockWise = T))

  plot[2] <- "plot2"
  expect_error(numberCorner(projCoord = coord, plot = plot, origin = c(F, F, T, F), clockWise = T))
})
