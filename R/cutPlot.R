if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "X", "Y", ".SD", "XRel", "YRel", "XAbs", "YAbs"
  ))
}

#' Divides a plot in subplots
#'
#' This function divides a plot in subplots (with dimX and dimY) and gives the
#' coordinates of the grid in return.
#' This function uses a procrustes analysis to fit the rectangle you gave to the plot you have.
#'
#' @param projCoord A data frame with the projected coordinates with X and Y on the first and second column respectively
#' @param plot Vector with the code of the plot
#' @param corner Vector with the corner numbered from 1 to 4 for each plot, the numbered must be counted clockwise
#' (see the result of the [numberCorner()])
#' @param gridsize The size of the grid
#' @param dimX A vector of the real size for the X axis for the plot (can be given one value it will be replicate for each plot)
#' @param dimY A vector of the real size for the Y axis for the plot (can be given one value it will be replicate for each plot)
#'
#' @return This function return a data frame with :
#'   - `plot`:  The code of the plot you use
#'   - `subplot`:  The code of the subplot automatically generated
#'   - `XRel`:  The relative coordinate for the axis X (following the corner 1->2) for the plot
#'   - `YRel`:  The relative coordinate for the axis Y (following the corner 1->4) for the plot
#'   - `XAbs`:  The absolute coordinate (projected) for the axis X (following the corner 1->2)
#'   - `YAbs`:  The absolute coordinate (projected) for the axis Y (following the corner 1->4)
#'
#' @export
#' @author Arthur PERE
#' @importFrom data.table data.table :=
#' @examples
#'
#' coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000
#' corner <- c(1, 2, 4, 3)
#' plot <- rep("plot1", 4)
#'
#' cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)
#'
#' # plot the result
#' plot(coord, main = "example", xlim = c(4900, 5300), ylim = c(4900, 5300), asp = 1)
#' text(coord, labels = corner, pos = 1)
#' points(cut$XAbs, cut$YAbs, pch = "+")
#' legend("bottomright", legend = c("orignal", "cut"), pch = c("o", "+"))
cutPlot <- function(projCoord, plot, corner, gridsize = 100, dimX = 200, dimY = 200) {

  # parameter verification --------------------------------------------------
  if (!is.data.frame(projCoord)) {
    projCoord <- data.frame(projCoord)
  }

  if (nrow(projCoord) != length(plot)) {
    stop("Length of plot and the number of row of your UTMcoord data frame are different")
  }
  if (nrow(projCoord) != length(corner)) {
    stop("Length of corner and the number of row of your UTMcoord data frame are different")
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


  # function ----------------------------------------------------------------



  Coord <- data.table(plot = plot, X = projCoord[, 1], Y = projCoord[, 2], corner = corner)
  Coord <- Coord[order(corner), .SD, by = plot]
  dimRel <- data.table(plot = unique(plot), dimX = dimX, dimY = dimY)


  # Do the grid in the plot and calcul the coordinate absolute of the points of the grid
  grid <- function(data, gridsize) {
    a <- as.matrix(data[, .(X, Y)])

    # Do the matrix for the procrust problem
    b <- matrix(0, nrow = 4, ncol = 2)
    b[2:3, 1] <- unique(data[, dimX])
    b[3:4, 2] <- unique(data[, dimY])

    res <- procrust(a, b)

    # The grid matrix
    c <- as.matrix(expand.grid(
      X = seq(0, max(b[, 1]), by = gridsize),
      Y = seq(0, max(b[, 2]), by = gridsize)
    ))

    # in absolute coordinate
    coordAbs <- c %*% res$rotation
    coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")

    return(data.table(XRel = c[, 1], YRel = c[, 2], XAbs = coordAbs[, 1], YAbs = coordAbs[, 2]))
  }

  Coord <- Coord[dimRel, on = "plot"][, grid(.SD, gridsize), by = plot]

  cornerCoordinate <- function(data) {
    rbindlist(apply(data[XRel < max(XRel) & YRel < max(YRel), -1], 1, function(x) {
      X <- x["XRel"]
      Y <- x["YRel"]

      data[
        (XRel == X & YRel == Y) |
          (XRel == X + gridsize & YRel == Y) |
          (XRel == X + gridsize & YRel == Y + gridsize) |
          (XRel == X & YRel == Y + gridsize),
        .(subplot = paste(plot, X / gridsize, Y / gridsize, sep = "_"), XRel, YRel, XAbs, YAbs)
      ][c(1, 2, 4, 3), corner := seq(4)]
    }))
  }

  Coord <- Coord[, cornerCoordinate(.SD), by = plot, .SDcols = colnames(Coord)]


  return(as.data.frame(Coord))
}
