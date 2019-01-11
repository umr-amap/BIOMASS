#' Correct the GPS coordinates
#'
#' @description
#' This function build the most probable GPS coordinates of the plot corners from multiple GPS measurements.
#'
#' @details
#' You must give one parameter between longlat and projCoord
#'
#'
#' @param longlat (optionnal) data frame with the coordinate in longitude latitude (eg. cbind(longitude, latitude)).
#' @param projCoord (optionnal) data frame with the projected coordinate in X Y
#' @param coordRel data frame with the relative coordinate in the same order than the longlat or projCoord
#' @param rangeX a vector of length 2 giving the range for plot relative X coordinates
#' @param rangeY a vector of length 2 giving the range for plot relative Y coordinates
#' @param maxDist a numeric giving the maximum distance above which GPS measurements should be considered as outliers (by default 10 m)
#' @param drawPlot a logical if you want to display a graphical representation
#' @param rmOutliers a logical if you want to remove the outliers from coordinates calculation
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#'
#' @return If you there are no outliers or rmOutliers = TRUE, a list with:
#'    - `corner`: a matrix with the coordinates of the corners
#'    - `polygon`: a spatial polygon
#'    - `outliers`: Coordinates lines considered as outliers, if any
#'
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
#' projCoord <- data.frame(
#'   X = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 8, max = 12),
#'     runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)
#'   ),
#'   Y = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 80, max = 120),
#'     runif(5, min = 8, max = 12), runif(5, min = 90, max = 110)
#'   )
#' )
#' projCoord <- projCoord + 1000
#' coordRel <- data.frame(
#'   X = c(rep(0, 10), rep(100, 10)),
#'   Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
#' )
#' 
#' aa <- correctCoordGPS(
#'   projCoord = projCoord, coordRel = coordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100)
#' )
#' bb <- correctCoordGPS(
#'   projCoord = projCoord, coordRel = coordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100), rmOutliers = TRUE
#' )
#' \dontrun{
#' correctCoordGPS(
#'   projCoord = projCoord, coordRel = coordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100), drawPlot = TRUE
#' )
#' }
#' 
correctCoordGPS <- function(longlat = NULL, projCoord = NULL, coordRel, rangeX, rangeY,
                            maxDist = 10, drawPlot = FALSE, rmOutliers = FALSE) {


  # parameters verification -------------------------------------------------


  if (is.null(longlat) && is.null(projCoord)) {
    stop("Give at least one set of coordinates: longlat or projCoord")
  }
  if (!is.null(longlat) && !is.null(projCoord)) {
    stop("Give only one set of coordinates: longlat or projCoord")
  }

  if (length(rangeX) != 2 || length(rangeY) != 2) {
    stop("The rangeX and/or rangeY must be of length 2")
  }
  if (length(maxDist) != 1) {
    stop("Your argument maxDist must be of length 1")
  }

  if (!all(between(coordRel[, 1], lower = rangeX[1], upper = rangeX[2]) &
    between(coordRel[, 2], lower = rangeY[1], upper = rangeY[2]))) {
    stop("coordRel must be inside the X and Y ranges")
  }
  if ((!is.null(longlat) && any(dim(longlat) != dim(coordRel))) || (!is.null(projCoord) && any(dim(projCoord) != dim(coordRel)))) {
    stop("GPS and relative coordinates are not of the same dimension")
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

  # Calculate the distances between the GNSS measurements and the CoordAbs
  dist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[, 2] - projCoord[, 2])^2)
  outliers <- which(dist > maxDist)


  # retransform the coordRel without the outliers
  if (rmOutliers) {
    res <- procrust(projCoord[-outliers, ], coordRel[-outliers, ])
    coordAbs <- as.matrix(coordRel) %*% res$rotation
    coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")
  }

  # Create the matrix of corners to return the projected coordinate of the corner of the plot
  cornerCoord <- as.matrix(expand.grid(X = sort(rangeX), Y = sort(rangeY)))
  # switch between the lines 3 and 4 because the corner 3 and 4 are invert in the expand.grid
  cornerCoord <- cornerCoord[c(1, 2, 4, 3), ]

  # Project the corner matrix in the projected coordinate
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
      ylim = range(projCoord[, 2], coordAbs[, 2]),
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



  if (length(outliers) != 0 & !rmOutliers) {
    warning(
      "Be carefull, you may have GNSS measurement outliers. \n",
      "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument)."
    )
  }


  return(list(corner = cornerCoord, polygon = sps, outliers = outliers))
}
