if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "subplot", "plot", "X", "Y"
  ))
}

#' Attribute trees to subplots
#'
#' Fonction to attribute the trees on each subplot, the trees that are at the exterior of the subplot will be marked as NA
#'
#' @param xy The coordinates of the trees for each plot
#' @param plot The label of the plot (same length as the number of rows of `xy`)
#' @param coordAbs Output of the function [cutPlot()]
#'
#' @return A vector with the code of the subplot for each trees, the code will be `plot_X_Y`. `X` and `Y` are the coordinate
#' where the tree is inside the plot in regards to the corresponding subplot.
#' @export
#' @author Arthur PERE
#' @importFrom data.table data.table setDT %between% setnames
#'
#' @examples
#' 
#' # Trees relative coordinates
#' xy <- data.frame(x = runif(200, min = 0, max = 200), y = runif(200, min = 0, max = 200))
#' 
#' 
#' # cut the plot in multiple part
#' coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
#' coord[1:4, ] <- coord[1:4, ] + 5000
#' coord[5:8, ] <- coord[5:8, ] + 6000
#' corner <- rep(c(1, 2, 4, 3), 2)
#' plot <- rep(c("plot1", "plot2"), each = 4)
#' 
#' cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)
#' 
#' 
#' # Assign a plot to 200 trees
#' plot <- rep(c("plot1", "plot2"), 100)
#' 
#' # attribute trees to subplots
#' attributeTree(xy, plot, cut)
attributeTree <- function(xy, plot, coordAbs) {


  # parameters verification -------------------------------------------------
  if (!is.data.frame(xy)) {
    xy <- data.frame(xy)
  }

  if (nrow(xy) != length(plot)) {
    stop("Your plot vector have not the same length with the number of row of xy")
  }
  if (!is.data.frame(coordAbs)) {
    stop("Your parameter 'CoordAbs' is not a data frame")
  }

  Coord <- data.table(xy, plot = plot)
  setnames(Coord, colnames(Coord), c("X", "Y", "plot"))
  setDT(coordAbs)

  # Attribute the trees to the subplot
  invisible(lapply(split(coordAbs, by = "subplot", keep.by = T), function(x) {
    Coord[
      plot == x$plot[1] & X %between% range(x$XRel) & Y %between% range(x$YRel),
      subplot := x$subplot[1]
    ]
  }))

  return(Coord[, subplot])
}
