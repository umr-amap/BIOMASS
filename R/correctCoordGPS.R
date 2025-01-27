#' Correct the GPS coordinates
#'
#' @description
#' This function builds the most probable GPS coordinates of the plot corners from multiple GPS measurements.
#'
#' @details
#' GPS coordinates should be either given in longitude latitude (longlat) or in projected coordinates (projCoord)
#'
#'
#' @param longlat (optional) data frame with the coordinate in longitude latitude (eg. cbind(longitude, latitude)).
#' @param projCoord (optional) data frame with the projected coordinate in X Y
#' @param coordRel data frame with the relative coordinate in the same order than the longlat or projCoord
#' @param rangeX a vector of length 2 giving the range for plot relative X coordinates
#' @param rangeY a vector of length 2 giving the range for plot relative Y coordinates
#' @param maxDist a numeric giving the maximum distance above which GPS measurements should be considered as outliers (by default 15 m)
#' @param drawPlot a logical if you want to display a graphical representation
#' @param rmOutliers a logical if you want to remove the outliers from coordinates calculation
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#'
#' @return If there are no outliers or rmOutliers = TRUE, a list with:
#'    - `cornerCoords`: a data.frame with the coordinates of the corners
#'    - `correctedCoord`: a data.frame with the adjusted coordinates given as input
#'    - `polygon`: a spatial polygon
#'    - `outliers`: index of coordinates lines considered as outliers, if any
#'    - `codeUTM`: the UTM code of the coordinates if the parameter `longlat` is set
#'
#'
#'
#' @export
#'
#' @importFrom data.table between
#' @importFrom sf st_multipoint st_polygon st_sfc
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
#' \donttest{
#' correctCoordGPS(
#'   projCoord = projCoord, coordRel = coordRel,
#'   rangeX = c(0, 100), rangeY = c(0, 100), drawPlot = TRUE
#' )
#' }
#'
correctCoordGPS <- function(longlat = NULL, projCoord = NULL, coordRel, rangeX, rangeY,
                            maxDist = 15, drawPlot = FALSE, rmOutliers = TRUE) {
  
  warning("This function still work but will be removed (defunct) and replaced by checkCoordPlot in the next version. Please see VIGNETTENAME? vignette")
  
  .Deprecated(msg = "'correctCoordGPS()' has been replaced by `check_plot_coord()` function and will be removed in the next version.\nPlease see the vignette `Spatialized trees and forest stand metrics with BIOMASS`")
  
  # To maintain user's original options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
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



  # Transform the geographic coordinates into UTM coordinates
  if (!is.null(longlat)) {
    projCoord <- latlong2UTM(longlat)
    codeUTM <- unique(projCoord[, "codeUTM"])
    projCoord <- projCoord[, c("X", "Y")]
  }

  # Transformation CoordRel to CoordAbs
  res <- procrust(projCoord, coordRel)
  coordAbs <- as.matrix(coordRel) %*% res$rotation
  coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")

  # Calculate the distances between the GNSS measurements and the CoordAbs
  dist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[, 2] - projCoord[, 2])^2)
  outliers <- which(dist > maxDist)

  if (length(outliers)==nrow(projCoord)){
    stop("All coordinates points are considered as outliers at the first stage.\n
         This may be because some coordinates have very large error associated.\n
         Try to remove these very large error or reconsider the maxDist parameter by increasing the distance")
  }
  
  # retransform the coordRel without the outliers
  if (rmOutliers & length(outliers)>0) {
    refineCoord <- TRUE
    while(refineCoord){
      res <- procrust(projCoord[-outliers, ], coordRel[-outliers,])
      coordAbs <- as.matrix(coordRel) %*% res$rotation
      coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")
      newdist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[, 2] - projCoord[, 2])^2)
      if(all(which(newdist > maxDist)==outliers)) refineCoord <- FALSE
      outliers <- which(newdist > maxDist)
    }
  }

  # Create the matrix of corners to return the projected coordinate of the corner of the plot
  cornerCoord <- as.matrix(expand.grid(X = sort(rangeX), Y = sort(rangeY)))
  # switch between the lines 3 and 4 because the corner 3 and 4 are invert in the expand.grid
  cornerCoord <- cornerCoord[c(1, 2, 4, 3), ]

  # Project the corner matrix in the projected coordinate
  cornerCoord <- as.matrix(cornerCoord) %*% res$rotation
  cornerCoord <- sweep(cornerCoord, 2, res$translation, FUN = "+")

  # Create a polygon
  p <- st_multipoint(rbind(cornerCoord, cornerCoord[1, ]))
  ps <- st_polygon(list(p), 1)
  sps <- st_sfc(list(ps))
  

  # draw plot ---------------------------------------------------------------


  if (drawPlot) {
    par(xpd = TRUE, mar = par("mar") + c(0, 0, 0, 7.5))
    plot(if(length(outliers)==0) projCoord else projCoord[-outliers, ],
      col = "grey30", main = "Plot drawing",
      xlim = range(projCoord[, 1], coordAbs[, 1]),
      ylim = range(projCoord[, 2], coordAbs[, 2]),
      asp = 1, xlab = "X", ylab = "Y", axes = FALSE, frame.plot = FALSE
    )

    usr <- par("usr")
    grid <- sapply(par(c("xaxp", "yaxp")), function(x) {
      seq(x[1], x[2], length.out = x[3] + 1)
    }, simplify = FALSE)
    # draw the grid
    segments(x0 = grid$xaxp, y0 = usr[3], y1 = usr[4], col = "grey80", lty = 1)
    segments(y0 = grid$yaxp, x0 = usr[1], x1 = usr[2], col = "grey80", lty = 1)
    # draw the axis
    axis(side = 1, lty = "blank", las = 1)
    axis(side = 2, lty = "blank", las = 1)
    plot(sps, add = TRUE, lwd = 3)
    points(coordAbs, col = "black", pch = 15, cex = 1.3)
    if(length(outliers)>0) points(projCoord[outliers, ], col = "red", pch = 4, cex = 1)

    legend(
      x = usr[2], y = grid$yaxp[length(grid$yaxp) - 1],
      c("GPS measurements", ifelse(rmOutliers, "Outliers (discarded)", "Outliers"), "Corrected coord"),
      col = c("grey30", "red", "black"),
      pch = c(1, 4, 15, 49), bg = "grey90"
    )
    par(xpd = NA, mar = c(5, 4, 4, 2) + 0.1)
  }


  # return ------------------------------------------------------------------



  if (length(outliers) != 0 & !rmOutliers) {
    warning(
      "Be carefull, you may have GNSS measurement outliers. \n",
      "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument)."
    )
  }

  output <- list(
    cornerCoords = data.frame(X = cornerCoord[, 1], Y = cornerCoord[, 2]),
    correctedCoord=data.frame(X = coordAbs[, 1], Y = coordAbs[, 2]),
    polygon = sps, outliers = outliers
  )
  if (!is.null(longlat)) {
    output$codeUTM <- codeUTM
  }
  return(output)
}
