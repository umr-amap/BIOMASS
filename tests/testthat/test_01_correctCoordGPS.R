set.seed(2)

projCoord <- data.frame(
  X = c(runif(5, min = 9, max = 11), runif(5, min = 8, max = 12), runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)),
  Y = c(runif(5, min = 9, max = 11), runif(5, min = 80, max = 120), runif(5, min = 8, max = 12), runif(5, min = 90, max = 110))
)
projCoord$X <- projCoord$X + 200000
projCoord$Y <- projCoord$Y + 9000000


coordRel <- data.frame(
  X = c(rep(0, 10), rep(100, 10)),
  Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
)

context("correct coord GPS")
test_that("correct coord GPS in UTM", {

  # with max dist equal 10
  expect_warning(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100),rmOutliers = FALSE),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressWarnings(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100),rmOutliers = FALSE)
  )
  expect_is(corr, "list")
  expect_length(corr, 4)
  expect_equal(names(corr), c("cornerCoords", "correctedCoord","polygon", "outliers"))

  expect_is(corr$cornerCoords, "data.frame")
  expect_is(corr$correctedCoord, "data.frame")
  expect_is(corr$polygon, "SpatialPolygons")
  expect_is(corr$outliers, "integer")

  expect_length(corr$outliers, 6)
  expect_equal(dim(corr$cornerCoords), c(4, 2))


  # with max dist equal 15
  expect_warning(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15,rmOutliers = FALSE),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressWarnings(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15,rmOutliers = FALSE)
  )
  expect_length(corr$outliers, 6)

  # with max dist equal 20 there isn't outliers anymore
  expect_failure(expect_warning(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 20,rmOutliers = FALSE),
    "Be carefull, you may have GNSS measurement outliers"
  ))


  # with rmOutliers = TRUE
  expect_failure(expect_warning(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), rmOutliers = TRUE),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  corr_2 <- suppressWarnings(
    correctCoordGPS(
      projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100),
      maxDist = 15, rmOutliers = TRUE
    )
  )

  expect_failure(expect_equal(corr$corner, corr_2$corner))
  expect_failure(expect_equal(corr$polygon, corr_2$polygon))
})

test_that("correct coord GPS error", {
  expect_error(correctCoordGPS(), "Give at least one set of coordinates")
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = 52, rangeY = 53,rmOutliers = FALSE),
    "must be of length 2"
  )
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = c(15, 0),rmOutliers = FALSE),
    "Your argument maxDist must be of length 1"
  )
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = coordRel, rangeX = c(0, 40), rangeY = c(0, 40),rmOutliers = FALSE),
    "coordRel must be inside the X and Y ranges"
  )
  expect_error(
    correctCoordGPS(projCoord = projCoord, coordRel = rbind(coordRel, c(40, 40)), rangeX = c(0, 100), rangeY = c(0, 100),rmOutliers = FALSE),
    "same dimension"
  )
  skip_if_not_installed("proj4")
  expect_error(
    correctCoordGPS(longlat = c(15, 12), projCoord = projCoord,rmOutliers = FALSE),
    "Give only one set of coordinates"
  )
})

test_that("correct coord GPS in long lat", {
  skip_if_not_installed("proj4") 
  longlat <- as.data.frame(proj4::project(projCoord,
    proj = "+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
    inverse = TRUE
  ))

  # with max dist equal 10
  expect_warning(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100),rmOutliers = FALSE),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressWarnings(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100),rmOutliers = FALSE)
  )
  expect_is(corr, "list")
  expect_length(corr, 5)
  expect_equal(names(corr), c("cornerCoords", "correctedCoord","polygon", "outliers", "codeUTM"))

  expect_is(corr$cornerCoords, "data.frame")
  expect_is(corr$correctedCoord, "data.frame")
  expect_is(corr$polygon, "SpatialPolygons")
  expect_is(corr$outliers, "integer")
  expect_is(corr$codeUTM, "character")

  expect_length(corr$outliers, 6)
  expect_equal(dim(corr$cornerCoords), c(4, 2))


  # with max dist equal 15
  expect_warning(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15,rmOutliers = FALSE),
    "Be carefull, you may have GNSS measurement outliers"
  )
  corr <- suppressWarnings(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 15,rmOutliers = FALSE)
  )
  expect_length(corr$outliers, 6)

  # with max dist equal 20 there isn't outliers anymore
  expect_failure(expect_warning(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), maxDist = 20,rmOutliers = FALSE),
    "Be carefull, you may have GNSS measurement outliers"
  ))


  # with rmOutliers = TRUE
  expect_failure(expect_warning(
    correctCoordGPS(longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100), rmOutliers = TRUE),
    "Be carefull, you may have GNSS measurement outliers"
  ))

  corr_2 <- suppressWarnings(
    correctCoordGPS(
      longlat = longlat, coordRel = coordRel, rangeX = c(0, 100), rangeY = c(0, 100),
      maxDist = 15, rmOutliers = TRUE
    )
  )

  expect_failure(expect_equal(corr$corner, corr_2$corner))
  expect_failure(expect_equal(corr$polygon, corr_2$polygon))
})



