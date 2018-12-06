if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "X", "Y", ".SD"
  ))
}

#' Cut plots in pieces
#'
#' This function cut all the plot in smaller rectangle pieces that you can give dimensions (with dimX and dimY), and give the
#' coordinate of the grid in return.
#' This function actually use the procrustes analysis to fit the rectangle you gave to the plot you have.
#'
#' @param projCoord A data frame with the projected coordinate with X and Y on the first and second colonne respectively
#' @param plot Vector with the code of the plot
#' @param corner Vector with the corner numbered from 1 to 4 for each plot, the numbered must be conter clockwise
#' (see the result of the \code{\link{numberCorner}})
#' @param gridsize The size of the grid
#' @param dimX A vector of the real size for the X axis for the plot (can be given one value it will be replicate for each plot)
#' @param dimY A vector of the real size for the Y axis for the plot (can be given one value it will be replicate for each plot)
#'
#' @return This function return a data frame with :
#' \describe{
#'   \item{Plot}{ The code of the plot you use }
#'   \item{XRel}{ The relative coordinate for the axis X (following the corner 1->2) for the plot }
#'   \item{YRel}{ The relative coordinate for the axis Y (following the corner 1->4) for the plot }
#'   \item{XAbs}{ The absolute coordinate (UTM) for the axis X (following the corner 1->2)}
#'   \item{YAbs}{ The absolute coordinate (UTM) for the axis Y (following the corner 1->4)}
#' }
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
  if (nrow(UTMcoord) != length(plot)) {
    stop("Your vector plot and the number of row of your UTMcoord data frame aren't the same")
  }
  if (nrow(UTMcoord) != length(corner)) {
    stop("Your vector corner and the number of row of your UTMcoord data frame aren't the same")
  }
  if (length(gridsize) != 1 || !is.numeric(gridsize)) {
    stop("The parameter 'gridsize' must be one number")
  }
  if (!(length(dimX) %in% c(1, length(unique(plot))))) {
    stop("Your dimX vector must be the length 1 or the length unique(plot)")
  }
  if (!(length(dimY) %in% c(1, length(unique(plot))))) {
    stop("Your dimY vector must be the length 1 or the length unique(plot)")
  }
  if (any(gridsize > dimX) || any(gridsize > dimY)) {
    stop("Your gridsize is superior of your dimension X or Y")
  }


  # function ----------------------------------------------------------------



  Coord <- data.table(Plot = plot, X = UTMcoord[, 1], Y = UTMcoord[, 2], corner = corner)
  dimRel <- data.table(Plot = unique(plot), dimX = dimX, dimY = dimY)


  # Do the grid in the plot and calcul the coordinate absolute of the points of the grid
  grid <- function(data, gridsize) {
    a <- as.matrix(data[order(corner), .(X, Y)])

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

  Coord <- setDF(Coord[dimRel, on = "Plot"][, grid(.SD, gridsize), by = "Plot"])

  return(Coord)
}
