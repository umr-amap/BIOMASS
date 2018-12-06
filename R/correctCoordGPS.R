#' Correct the GPS coordinate
#'
#' @description
#' If you have multiple GPS coordinate for one plot, you can use this function to correct them and take the corner of the plot
#' with the polygon associated.
#'
#' @details
#' You must give one parameter between longlat and UTMcoord
#'
#'
#' @param longlat (optionnal) data frame with the coordinate in longitude latitude (eg. cbind(longitude, latitude)).
#' @param projCoord (optionnal) data frame with the projected coordinate in X Y
#' @param coordRel data frame with the relative coordinate in the same order than the longlat or UTMcoord
#' @param rangeX a vector of length equal 2 with the min of the range for X
#' @param rangeY a vector of length equal 2 with the min of the range for Y
#' @param maxDist a double with the maximum for the distance you want to use to make outliers
#' @param drawPlot a logical if you want to draw the plot
#' @param rmOutliers a logical if you want to remove the outliers
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#'
#' @return If you haven't outliers or rmOutliers = TRUE, there will be a list with :
#' \describe{
#'    \item{corner}{a matrix with the coordinate of the corners whith no order in particular}
#'    \item{polygon}{a spatial polygone}
#' }
#' However if you have outliers and rmOutliers = FALSE, there will be data frame with :
#' \describe{
#'    \item{outliers}{the line of your longlat or UTMcoord data frame, where it's an outliers}
#'    \item{X}{the UTM coordinate X outlayer}
#'    \item{Y}{the UTM coordinate X outlayer}
#' }
#'
#'
#' @export
#'
#' @importFrom data.table between
#' @importFrom sp Polygon Polygons SpatialPolygons plot
#' @importFrom graphics points
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#'
#' @examples
#' UTMcoord <- data.frame(
#'   X = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 8, max = 12),
#'     runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)
#'   ),
#'   Y = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 80, max = 120),
#'     runif(5, min = 8, max = 12), runif(5, min = 90, max = 110)
#'   )
#' )
#' UTMcoord <- UTMcoord + 1000
#' CoordRel <- data.frame(
#'   X = c(rep(0, 10), rep(100, 10)),
#'   Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
#' )
#' 
#' aa <- correctCoordGPS(
#'   UTMcoord = UTMcoord, CoordRel = CoordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100)
#' )
#' bb <- correctCoordGPS(
#'   UTMcoord = UTMcoord, CoordRel = CoordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100), rmOutliers = TRUE
#' )
#' \dontrun{
#' correctCoordGPS(
#'   UTMcoord = UTMcoord, CoordRel = CoordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100), drawPlot = TRUE
#' )
#' }
#' 
correctCoordGPS <- function(longlat = NULL, projCoord = NULL, coordRel, rangeX, rangeY, maxDist = 10, drawPlot = F, rmOutliers = F) {


  # parameters verification -------------------------------------------------


  if (is.null(longlat) && is.null(projCoord)) {
    stop("Give me at least one system of coordinate")
  }
  if (!is.null(longlat) && !is.null(projCoord)) {
    stop("You are giving too much arguments : longlat and projCoord")
  }

  if (length(rangeX) != 2 || length(rangeY) != 2) {
    stop("The rangeX and/or rangeY must be length equal to 2")
  }
  if (length(maxDist) != 1) {
    stop("Your argument maxDist must be one double")
  }

  if (!all(between(coordRel[, 1], lower = rangeX[1], upper = rangeX[2]) &
    between(coordRel[, 2], lower = rangeY[1], upper = rangeY[2]))) {
    stop("The coordRel must be inside the range")
  }
  if ((!is.null(longlat) && dim(longlat) != dim(coordRel)) || (!is.null(projCoord) && dim(projCoord) != dim(coordRel))) {
    stop("Your argument of coordinate and relative coordinate aren't the same dimension")
  }



  # function ----------------------------------------------------------------



  # Transform the geographic coordinate into UTM coordinate
  if (!is.null(longlat)) {
    projCoord <- latlong2UTM(longlat)[, c("X", "Y")]
  }

  # Transformation CoordRel to CoordAbs
  res <- procrust(projCoord, coordRel)
  coordAbs <- as.matrix(coordRel) %*% res$rotation
  coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")

  # Calculate the distances between the GNSS measurement and the CoordAbs
  dist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[, 2] - projCoord[, 2])^2)
  outliers <- which(dist > maxDist)


  # retransform the coordRel without the outliers
  if (rmOutliers) {
    res <- procrust(projCoord[-outliers, ], coordRel[-outliers, ])
    coordAbs <- as.matrix(coordRel) %*% res$rotation
    coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")
  }

  # Create the matrix of ralatif corners
  cornerCoord <- as.matrix(expand.grid(X = sort(rangeX), Y = sort(rangeY)))
  cornerCoord <- cornerCoord[c(1, 2, 4, 3), ] # switch between the line 3 and 4

  # Transform the matrix of relatif corners to absolute corner
  cornerCoord <- as.matrix(cornerCoord) %*% res$rotation
  cornerCoord <- sweep(cornerCoord, 2, res$translation, FUN = "+")

  # Create a polygon
  p <- Polygon(rbind(cornerCoord, cornerCoord[1, ]))
  ps <- Polygons(list(p), 1)
  sps <- SpatialPolygons(list(ps))



  # draw plot ---------------------------------------------------------------


  if (drawPlot) {
    plot(projCoord[-outliers, ],
      col = "grey30", main = "Plot drawing",
      xlim = range(projCoord[, 1], coordAbs[, 1]),
      ylim = range(projCoord[, 1], coordAbs[, 2]),
      asp = 1, xlab = "X", ylab = "Y", axes = F, frame.plot = F
    )
    grid(col = "grey80", lty = 1)
    axis(side = 1, lty = "blank", las = 1)
    axis(side = 2, lty = "blank", las = 1)
    plot(sps, add = T)
    points(coordAbs, col = "black", pch = 15, cex = 1.3)
    points(projCoord[outliers, ], col = "red", pch = 4, cex = 1)

    legend("center", c("GPS measurements", ifelse(rmOutliers, "outliers (discarded)", "outliers"), "Corrected coord"),
      col = c("grey30", "red", "black"),
      pch = c(1, 4, 15), bg = "grey90"
    )
  }


  # return ------------------------------------------------------------------



  if (length(outliers) != 0 & (nrow(projCoord) - length(outliers)) > 3 & !rmOutliers) {
    message(
      "Be carefull, you may have GNSS measurement outliers. \n",
      "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument)."
    )
    return(cbind(outliers, projCoord[outliers, ]))
  }


  return(list(corner = cornerCoord, polygon = sps))
}
