if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "XRel", "X", "Y", "YRel", "Plot",
    "i.Xmax", "i.Ymax", "Xmax", "Xplot",
    "Ymax", "Yplot", "subplot"
  ))
}

#' Attribute trees to subplots
#'
#' Fonction to attribute the trees on each subplot, the trees that are at the exterior of the subplot will be marked as NA
#'
#' @param xy The coordinates of the trees for each plot
#' @param plot The label of the plot (same length as the number of rows of xy)
#' @param coordAbs Output of the function \code{\link{cutPlot}}
#'
#' @return A vector with the code of the subplot for each trees, the code will be plot_X_Y. X and Y are the coordinate where the tree
#' is inside the plot in regards to the corresponding subplot.
#' @export
#' @author Arthur PERE
#' @importFrom data.table data.table setDT between setnames
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

  if (length(unique(plot)) < length(unique(coordAbs$Plot)) && any(sort(unique(plot)) != sort(unique(coordAbs$Plot)))) {
    warning(
      "The plot(s) ",
      sort(unique(plot))[ sort(unique(plot)) != sort(unique(coordAbs$Plot))],
      " will not be computed but will appear as the label of the plot"
    )
  }

  Coord <- data.table(xy, Plot = plot)
  setnames(Coord, colnames(Coord), c("X", "Y", "Plot"))
  setDT(coordAbs)

  # Attribute the trees to the subplot
  gridsize <- max(coordAbs[, diff(XRel)])
  Coord[, ":="(Xplot = floor(X / gridsize), Yplot = floor(Y / gridsize))]


  # Assign NA to the trees that are out of the limits of the plot
  Coord[coordAbs[, .(Xmax = max(XRel), Ymax = max(YRel)), by = Plot],
    on = "Plot",
    ":="(Xmax = i.Xmax, Ymax = i.Ymax)
  ]
  Coord[X < 0 | X > Xmax, Xplot := NA_integer_]
  Coord[Y < 0 | Y > Ymax, Yplot := NA_integer_]


  # if trees are at the superior limit of the plot
  Coord[, ":="(Xmax = Xmax / gridsize, Ymax = Ymax / gridsize), by = Plot]
  Coord[Xplot == Xmax, Xplot := Xplot - 1]
  Coord[Yplot == Ymax, Yplot := Yplot - 1]

  # assign the subplot and remove it if a tree is out of bounds
  Coord[, subplot := paste(Plot, Xplot, Yplot, sep = "_")]
  Coord[is.na(Xplot) | is.na(Yplot), subplot := NA_character_]
  Coord[is.na(Plot), subplot := NA_character_]

  return(Coord[, subplot])
}
