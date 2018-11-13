set.seed(2)

UTMcoord <- data.frame(
  X = c(runif(5, min = 9, max = 11), runif(5, min = 8, max = 12), runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)),
  Y = c(runif(5, min = 9, max = 11), runif(5, min = 80, max = 120), runif(5, min = 8, max = 12), runif(5, min = 90, max = 110))
)
UTMcoord <- UTMcoord + 1000


CoordRel <- data.frame(
  X = c(rep(0, 10), rep(100, 10)),
  Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
)

context("correct coord GPS")
test_that("correct coord GPS in UTM", {

  # whith max dist equal 10
  expect_message(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100)),
    "Be carefull, you may have GNSS measurement outlayers"
  )
  corr <- suppressMessages(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100))
  )
  expect_equal(dim(corr), c(9, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 15
  expect_message(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 15),
    "Be carefull, you may have GNSS measurement outlayers"
  )
  corr <- suppressMessages(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 15)
  )
  expect_equal(dim(corr), c(6, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 20 there isn't outlayers anymore
  expect_failure(expect_message(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 20),
    "Be carefull, you may have GNSS measurement outlayers"
  ))
  corr <- correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 20)
  expect_is(corr, "list")
  expect_length(corr, 2)
  expect_equal(names(corr), c("corner", "polygon"))
  expect_equal(dim(corr$corner), c(4, 2))
  expect_is(corr$polygon, "SpatialPolygons")
})



test_that("correct coord GPS in long lat", {
  skip_if_not_installed("proj4")
  longlat <- as.data.frame(proj4::project(UTMcoord,
    proj = "+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
    inverse = T
  ))

  # whith max dist equal 10
  expect_message(
    correctCoordGPS(longlat = longlat, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100)),
    "Be carefull, you may have GNSS measurement outlayers"
  )
  corr <- suppressMessages(
    correctCoordGPS(longlat = longlat, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100))
  )
  expect_equal(dim(corr), c(9, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 15
  expect_message(
    correctCoordGPS(longlat = longlat, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 15),
    "Be carefull, you may have GNSS measurement outlayers"
  )
  corr <- suppressMessages(
    correctCoordGPS(longlat = longlat, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 15)
  )
  expect_equal(dim(corr), c(6, 3))
  expect_is(corr, "data.frame")

  # with max dist equal 20 there isn't outlayers anymore
  expect_failure(expect_message(
    correctCoordGPS(longlat = longlat, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 20),
    "Be carefull, you may have GNSS measurement outlayers"
  ))
  corr <- correctCoordGPS(longlat = longlat, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = 20)
  expect_is(corr, "list")
  expect_length(corr, 2)
  expect_equal(names(corr), c("corner", "polygon"))
  expect_equal(dim(corr$corner), c(4, 2))
  expect_is(corr$polygon, "SpatialPolygons")
})



test_that("correct coord GPS error", {
  expect_error(correctCoordGPS(), "one system of coordinate")
  expect_error(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = 52, rangeY = 53),
    "length equal to 2"
  )
  expect_error(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 100), rangeY = c(0, 100), MaxDist = c(15, 0)),
    "one double"
  )
  expect_error(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = CoordRel, rangeX = c(0, 40), rangeY = c(0, 40)),
    "The coordRel must be inside the range"
  )
  expect_error(
    correctCoordGPS(UTMcoord = UTMcoord, CoordRel = rbind(CoordRel, c(40, 40)), rangeX = c(0, 100), rangeY = c(0, 100)),
    "same dimension"
  )

  expect_error(
    correctCoordGPS(longlat = c(15, 12), UTMcoord = UTMcoord),
    "too much arguments"
  )
})
