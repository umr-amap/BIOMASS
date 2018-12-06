set.seed(2)

projCoord <- data.frame(
  X = c(runif(5, min = 9, max = 11), runif(5, min = 8, max = 12), runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)),
  Y = c(runif(5, min = 9, max = 11), runif(5, min = 80, max = 120), runif(5, min = 8, max = 12), runif(5, min = 90, max = 110))
)
projCoord <- projCoord + 1000


coordRel <- data.frame(
  X = c(rep(0, 10), rep(100, 10)),
  Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
)

context("correct coord GPS")
test_that("correct coord GPS in UTM", {

  # whith max dist equal 10
  expect_message(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100)),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressMessages(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100))
  )
  expect_equal(dim(corr), c(9, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 15
  expect_message(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressMessages(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15)
  )
  expect_equal(dim(corr), c(6, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 20 there isn't outliers anymore
  expect_failure(expect_message(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 20),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  corr <- correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 20)
  expect_is(corr, "list")
  expect_length(corr, 2)
  expect_equal(names(corr), c("corner", "polygon"))
  expect_equal(dim(corr$corner), c(4, 2))
  expect_is(corr$polygon, "SpatialPolygons")
})



test_that("correct coord GPS in long lat", {
  skip_if_not_installed("proj4")
  longlat <- as.data.frame(proj4::project(projCoord,
    proj = "+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
    inverse = T
  ))

  # whith max dist equal 10
  expect_message(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100)),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressMessages(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100))
  )
  expect_equal(dim(corr), c(9, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 15
  expect_message(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressMessages(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15)
  )
  expect_equal(dim(corr), c(6, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 20 there isn't outliers anymore
  expect_failure(expect_message(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 20),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  corr <- correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 20)
  expect_is(corr, "list")
  expect_length(corr, 2)
  expect_equal(names(corr), c("corner", "polygon"))
  expect_equal(dim(corr$corner), c(4, 2))
  expect_is(corr$polygon, "SpatialPolygons")
})



test_that("correct coord GPS error", {
  expect_error(correctCoordGPS(), "one system of coordinate")
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = 52, rangeY = 53),
    "length equal to 2"
  )
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = c(15, 0)),
    "one double"
  )
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 40), rangeY = c(0, 40)),
    "The coordRel must be inside the range"
  )
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = rbind(coordRel, c(40, 40)), rangeX = c(0, 100), rangeY = c(0, 100)),
    "same dimension"
  )

  expect_error(
    correctCoordGPS(longlat = c(15, 12), projCoord = projCoord),
    "too much arguments"
  )
})
