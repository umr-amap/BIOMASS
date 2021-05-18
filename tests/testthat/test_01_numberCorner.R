coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000
plot <- rep("plot1", 4)


context("number corner")
test_that("number corner", {
  corner <- numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE)
  expect_is(corner, "data.frame")
  expect_equal(dim(corner), c(4, 4))

  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE)$corner,
    c(4, 3, 1, 2)
  )
  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = FALSE)$corner,
    c(2, 3, 1, 4)
  )
  expect_equal(
    numberCorner(projCoord = coord[c(1, 4, 3, 2), ], plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = FALSE)$corner,
    c(2, 4, 1, 3)
  )
  expect_equal(
    numberCorner(projCoord = coord[c(1, 4, 3, 2), ], plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE)$corner,
    c(4, 2, 1, 3)
  )
  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, TRUE, FALSE, FALSE), clockWise = TRUE)$corner,
    c(2, 1, 3, 4)
  )
  expect_equal(
    numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, TRUE, FALSE, FALSE), clockWise = FALSE)$corner,
    c(4, 1, 3, 2)
  )
})

test_that("number corner error", {
  expect_error(numberCorner())
  expect_error(numberCorner(longlat = "fdz", projCoord = "fze"))
  expect_error(numberCorner(longlat = coord[1:2, ], plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE))
  expect_error(numberCorner(projCoord = coord[1:2, ], plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE))
  expect_error(numberCorner(projCoord = coord, plot = plot[1:2], origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE))

  expect_error(numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, TRUE, TRUE, FALSE), clockWise = TRUE))

  plot[2] <- "plot2"
  expect_error(numberCorner(projCoord = coord, plot = plot, origin = c(FALSE, FALSE, TRUE, FALSE), clockWise = TRUE))
})
