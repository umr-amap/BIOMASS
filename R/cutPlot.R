if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "X", "Y", ".SD", "XRel", "YRel", "XAbs", "YAbs"
  ))
}

#' Divides one or more plots into subplots
#'
#' This function divides a plot (or several plots) in subplots and returns the coordinates of the grid.
#' These coordinates are calculated by a bilinear interpolation with the projected corner coordinates as references.  
#'
#' @param projCoord A data frame containing the projected coordinates of plot corners, with X and Y on the first and second column respectively
#' @param plot A vector indicating the plot codes
#' @param cornerNum A vector with corners numbered from 1 to 4 for each plot, numbering must be in clockwise direction
#' @param gridsize The size of the subplots
#' @param dimX A vector indicating the size of the plot on the X axis, in meters and in the relative coordinates system (if a single value is supplied, it will be replicated for all plots)
#' @param dimY A vector indicating the size of the plot on the Y axis, in meters and in the relative coordinates system (if a single value is supplied, it will be replicated for all plots)
#'
#' @return Returns a data-frame containing as many rows as there are corners corresponding to the subplots, and the following columns :
#'   - `plot`: The plot code
#'   - `subplot`: The automatically generated subplot code
#'   - `XRel`:  The relative coordinates on the X axis (defined by corners 1->4)
#'   - `YRel`:  The relative coordinates on the Y axis (defined by corners 1->2)
#'   - `XAbs`:  The absolute (projected) X coordinates  
#'   - `YAbs`:  The absolute (projected) Y coordinates
#'
#' @export
#' @author Arthur PERE
#' @importFrom data.table data.table := setnames
#' @examples
#'
#' coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000
#' cornerNum <- c(1, 2, 4, 3)
#' plot <- rep("plot1", 4)
#'
#' cut <- cutPlot(coord, plot, cornerNum, gridsize = 100, dimX = 200, dimY = 200)
#'
#' # plot the result
#' plot(coord, main = "example", xlim = c(4900, 5300), ylim = c(4900, 5300), asp = 1)
#' text(coord, labels = cornerNum, pos = 1)
#' points(cut$XAbs, cut$YAbs, pch = "+")
#' legend("bottomright", legend = c("orignal", "cut"), pch = c("o", "+"))
#' 
cutPlot <- function(projCoord, plot, cornerNum, gridsize = 100, dimX = 200, dimY = 200) {
  
  .Deprecated(msg = "'cutPlot()' has been replaced by `divide_plot()` and will be removed in the next version.\nPlease see the vignette `Spatialized trees and forest stand metrics with BIOMASS`")

  # parameter verification --------------------------------------------------
  if (!is.data.frame(projCoord)) {
    projCoord <- data.frame(projCoord)
  }

  if (nrow(projCoord) != length(plot)) {
    stop("Length of plot and the number of row of your UTMcoord data frame are different")
  }
  if (nrow(projCoord) != length(cornerNum)) {
    stop("Length of cornerNum and the number of row of your UTMcoord data frame are different")
  }
  if (length(gridsize) != 1 || !is.numeric(gridsize)) {
    stop("Gridsize must contain 1 numeric value")
  }
  if (!(length(dimX) %in% c(1, length(unique(plot))))) {
    stop("Your dimX vector must be of length 1 or of length equal to length(unique(plot))")
  }
  if (!(length(dimY) %in% c(1, length(unique(plot))))) {
    stop("Your dimY vector must be of length 1 or of length equal to length(unique(plot))")
  }
  if (any(gridsize > dimX) || any(gridsize > dimY)) {
    stop("Your gridsize is larger than the X or Y dimensions")
  }


  # Function ----------------------------------------------------------------

  cornerCoord <- data.table(plot = plot, X = projCoord[, 1], Y = projCoord[, 2], cornerNum = cornerNum)
  setnames(cornerCoord, colnames(cornerCoord), c("plot","X","Y","cornerNum")) #in case the user gives a data.table which preserved column
  cornerCoord <- cornerCoord[order(cornerNum), .SD, by = plot]
  dimRel <- data.table(plot = unique(plot), dimX = dimX, dimY = dimY)

  # Grids the plot in the relative coordinates system and calculates the absolute coordinates of the grid points.
  gridFunction <- function(data, gridsize) {
    
    # Absolute coordinates matrix of the corners
    absCoordMat <- as.matrix(data[, .(X, Y)])
    
    # Relative coordinates matrix of the corners
    plotDimX <- as.numeric(unique(data[,"dimX"]))
    plotDimY <- as.numeric(unique(data[,"dimY"]))
    relCoordMat <- matrix(
      c(0,0,
        0,plotDimY,
        plotDimX,plotDimY,
        plotDimX,0) ,
      byrow = T, ncol=2)
    
    # Grid matrix for the subplots
    gridMat <- as.matrix(expand.grid(
      X = seq(0, max(relCoordMat[, 1]), by = gridsize),
      Y = seq(0, max(relCoordMat[, 2]), by = gridsize)
    ))
    
    # Transformation of relative grid coordinates into absolute coordinates
    absCoord <- bilinear_interpolation(coord = gridMat , from_corner_coord = relCoordMat , to_corner_coord = absCoordMat )

    return(data.table(XRel = gridMat[, 1], YRel = gridMat[, 2], XAbs=absCoord[, 1], YAbs=absCoord[, 2]))
  }

  # Apply gridFunction to all plots
  cornerCoord <- cornerCoord[dimRel, on = "plot"][, gridFunction(.SD, gridsize), by = plot]

  # Number the corners in clockwise direction
  numberingCorner <- function(data) {
    rbindlist(apply(data[XRel < max(XRel) & YRel < max(YRel), -"plot"], 1, function(x) {
      X <- x["XRel"]
      Y <- x["YRel"]

      data[
        (XRel == X & YRel == Y) | (XRel == X + gridsize & YRel == Y) | (XRel == X + gridsize & YRel == Y + gridsize) | (XRel == X & YRel == Y + gridsize),
        .(subplot = paste(plot, X / gridsize, Y / gridsize, sep = "_"),
          XRel, YRel, XAbs, YAbs)][, cornerNum := c(1,4,2,3)] #Clockwise direction, for Counter-clockwise [c(1,2,4,3)]
    }))
  }

  cornerCoord <- cornerCoord[, numberingCorner(.SD), by = plot, .SDcols = colnames(cornerCoord)]

  return(as.data.frame(cornerCoord))
}

