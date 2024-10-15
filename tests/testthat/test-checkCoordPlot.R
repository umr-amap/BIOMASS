set.seed(2)

projCoord <- data.frame(
  X = c(runif(5, min = 9, max = 11), runif(5, min = 8, max = 12), runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)),
  Y = c(runif(5, min = 9, max = 11), runif(5, min = 80, max = 120), runif(5, min = 8, max = 12), runif(5, min = 90, max = 110))
)
projCoord$X <- projCoord$X + 200000
projCoord$Y <- projCoord$Y + 9000000

relCoord <- data.frame(
  X = c(rep(0, 10), rep(100, 10)),
  Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
)
cornerID <- rep(c("SW","NW","SE","NE"),e=5)

context("correct coord GPS")
test_that("checkCoordPlot error", {
  expect_error(checkPlotCoord(), "Give at least one set of coordinates")
  expect_error(checkPlotCoord(longlat=projCoord, projCoord=projCoord),"Give only one set of coordinates")
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=c(0,0)),"relCoord must be a matrix or a data frame")
  expect_error(checkPlotCoord(longlat=c(0,0), relCoord=relCoord),"longlat must be a matrix or a data frame")
  expect_error(checkPlotCoord(projCoord=c(0,0), relCoord=relCoord),"projCoord must be a matrix or a data frame")
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=relCoord, trustGPScorners=NULL),"The trustGPScorners argument must be TRUE or FALSE")
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=relCoord, trustGPScorners=T, maxDist=c(10,20) ),"The maxDist argument must be of length 1")
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=rbind(relCoord, c(40, 40)),trustGPScorners=T),"same dimension")
  wrongRelCoord <- relCoord ; wrongRelCoord[1,1] <- NA
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=wrongRelCoord, trustGPScorners=T,cornerID = cornerID),"Missing values are detected in relCoord. Please remove them and call the function again")
  wrongProjCoord <- projCoord ; wrongProjCoord[1,1] <- NA
  expect_error(checkPlotCoord(projCoord=wrongProjCoord, relCoord=relCoord, trustGPScorners=T,cornerID = cornerID),"Missing values are detected in projCoord. Please remove them and call the function again")
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=relCoord, trustGPScorners=T),"The argument cornerID is needed if trustGPScorners is TRUE and if multiple measurements of each corner have been realized")
  wrongRelCoord[1,1] <- 10
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=wrongRelCoord, trustGPScorners=T,cornerID = cornerID),"relCoord instead of 4")
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=relCoord, trustGPScorners=T,cornerID = c("a","b")),"cornerID must be the same length as the number of rows in relCoord")
  wrongCornerID <- cornerID ; wrongCornerID[1] <- "SN"
  expect_error(checkPlotCoord(projCoord=projCoord, relCoord=relCoord, trustGPScorners=T,cornerID = wrongCornerID),"cornerID instead of 4")
})


test_that("checkPlotCoord outputs and outliers", {
  # with max dist equal 10
  expect_warning(
    checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F, rmOutliers = F, cornerID = cornerID, drawPlot = F, maxDist = 10),"Be carefull, you may have GNSS measurement outliers"
  )
  outputs <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F, rmOutliers = T, cornerID = cornerID, drawPlot = F, maxDist = 10)
  expect_is(outputs, "list")
  expect_length(outputs, 3)
  expect_equal(names(outputs), c("cornerCoord", "polygon", "outliers"))
  expect_is(outputs$cornerCoord, "data.frame")
  expect_is(outputs$polygon, "sfc_POLYGON")
  expect_is(outputs$outliers, "data.frame")
  
  expect_equal(dim(outputs$outliers), c(10,3))
  expect_equal(dim(outputs$cornerCoord), c(4, 6))
  
  # with max dist equal 25 there isn't outliers anymore
  expect_failure(expect_warning(
    checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F, rmOutliers = F, cornerID = cornerID, drawPlot = F, maxDist = 25),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  
  # with rmOutliers = TRUE
  expect_failure(expect_warning(
    checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F, rmOutliers = T, cornerID = cornerID, drawPlot = F, maxDist = 25),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  outputs_2 <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F, rmOutliers = T, cornerID = cornerID, drawPlot = F, maxDist = 15)
  
  expect_failure(expect_equal(outputs$cornerCoord, outputs_2$cornerCoord))
  expect_failure(expect_equal(outputs$polygon, outputs_2$polygon))
})


test_that("checkPlotCoord in long lat", {
  longlat <- as.data.frame(proj4::project(projCoord,
                                          proj = "+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                          inverse = TRUE
  ))
  outputs_longlat <- checkPlotCoord(longlat = longlat, relCoord = relCoord, trustGPScorners = F,rmOutliers = T, cornerID = cornerID, drawPlot = F, maxDist = 10)
  outputs_projCoord <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F,rmOutliers = T, cornerID = cornerID, drawPlot = F, maxDist = 10)
  expect_is(outputs_longlat, "list")
  expect_length(outputs_longlat, 4)
  expect_equal(names(outputs_longlat), c("cornerCoord", "polygon", "outliers","codeUTM"))
  expect_equal(outputs_longlat[1:3],outputs_projCoord)
})

test_that("checkPlotCoord, trustGPScorners", {
  projCoord0 <- data.frame(
    X = rep(c(10,10,100,100), e=7) + 200000,
    Y = rep(c(10,100,100,10), e=7) + 9000000
  )
  projCoord$X <- projCoord0$X  + rep(sample(-3:3),4)
  projCoord$Y <- projCoord0$Y + rep(sample(-3:3),4)
  relCoord <- data.frame(
    X = rep(c(0,0,95,95),e=7),
    Y = rep(c(0,95,95,0),e=7)
  )
  cornerID <- rep(c("SW","NW","SE","NE"),e=7)
  
  expect_warning(
    checkPlotCoord(projCoord = projCoord[-(1:3),], relCoord = relCoord[-(1:3),], trustGPScorners = T, rmOutliers = T, cornerID = cornerID[-(1:3)], drawPlot = F),"At least one corner has less than 5 measurements. We suggest using the argument trustGPScorners = FALSE"
  )
  res_trustGPScorners_T <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = T, rmOutliers = T, cornerID = cornerID, drawPlot = F)
  expect_equivalent(res_trustGPScorners_T$cornerCoord[,1:2] , unique(projCoord0))
  
  res_trustGPScorners_F <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord, trustGPScorners = F, rmOutliers = T, cornerID = cornerID, drawPlot = F)
  expect_equal(res_trustGPScorners_T$cornerCoord[,3:6] , res_trustGPScorners_F$cornerCoord[,3:6])
  expect_equal(res_trustGPScorners_T$cornerCoord[,1:2], round(res_trustGPScorners_F$cornerCoord[,1:2],digits = -1))
  
  # Test when there's only 4 measures and trustGPScorners = F
  expect_equal(res_trustGPScorners_F , checkPlotCoord(projCoord = projCoord0[c(1,8,15,22),], relCoord = relCoord[c(1,8,15,22),], trustGPScorners = F, rmOutliers = T, drawPlot = F, cornerID = cornerID[c(1,8,15,22)]))
})

test_that("checkPlotCoord, origin corner", {
  projCoord0 <- data.frame(
    X = rep(c(0,0,100,100), e=7) + 200000,
    Y = rep(c(0,100,100,0), e=7) + 9000000
  )
  projCoord$X <- projCoord0$X  + rep(sample(-3:3),4)
  projCoord$Y <- projCoord0$Y + rep(sample(-3:3),4)
  
  cornerID <- rep(c("SW","NW","SE","NE"),e=7)
  
  # Test when the origin is not the South-East corner
  relCoord_NE <- data.frame(
    X = rep(c(100,100,0,0),e=7),
    Y = rep(c(100,0,0,100),e=7)
  )
  relCoord_SW <- data.frame(
    X = rep(c(0,0,100,100),e=7),
    Y = rep(c(0,100,100,0),e=7)
  )
  res_NE <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord_NE, trustGPScorners = T, rmOutliers = T, cornerID = cornerID, drawPlot = F)
  res_SW <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord_SW, trustGPScorners = T, rmOutliers = T, cornerID = cornerID, drawPlot = F)
  
  expect_equal(res_SW$cornerCoord[c("Xrel","Yrel","cornerNum")] , res_NE$cornerCoord[c("Xrel","Yrel","cornerNum")])
  expect_equivalent(res_SW$cornerCoord[c("X","Y","cornerID")] , res_NE$cornerCoord[c(3,4,1,2),c("X","Y","cornerID")])
  
  # Test when the origin of the relative coordinates is not(0;0)
  relCoord_SW <- relCoord_SW + 200
  res_SW_200 <- checkPlotCoord(projCoord = projCoord, relCoord = relCoord_SW, trustGPScorners = T, rmOutliers = T, cornerID = cornerID, drawPlot = F)
  expect_equal(res_SW$cornerCoord[c("X","Y","cornerID","cornerNum")] , res_SW_200$cornerCoord[c("X","Y","cornerID","cornerNum")])
  expect_equal(res_SW$cornerCoord[c("Xrel","Yrel")]+200 , res_SW_200$cornerCoord[c("Xrel","Yrel")])
})

