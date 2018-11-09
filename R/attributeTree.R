#' Attribute the tree to the subplot
#'
#' Fonction to attribute the trees on each subplot, the trees that are at the exterior 
#'
#' @param xy The coordinate of the trees for each plot
#' @param plot The label of the plot (same length as the number of row of xy)
#' @param CoordAbs Output of the function cutPlot
#'
#' @return A vector with the subplot for each trees
#' @export
#' @author Arthur PERE
#' @importFrom data.table data.table setDT between
#'
#' @examples
#' 
#' # trees relative coordinate
#' xy <- data.frame(x = runif(200, min = 0, max = 200), y = runif(200, min = 0, max = 200))
#'
#' 
#' # cut the plot in multiple part
#' coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
#' coord[1:4, ] = coord[1:4, ] + 5000
#' coord[5:8, ] = coord[5:8, ] + 6000
#' corner <- rep( c(1, 2, 4, 3), 2)
#' plot <- rep(c("plot1", "plot2"), each = 4)
#'
#' cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)
#' 
#' 
#' # The attribute the plot
#' plot <- rep(c("plot1","plot2"), 100)
#' 
#' # attribute the trees to subplot
#' attributeTree(xy, plot, cut)
#' 
attributeTree <- function(xy, plot, CoordAbs) {


  # parameters verification -------------------------------------------------

  if (nrow(xy) != length(plot)) {
    stop("Your plot vector haven't the same length than the number of row of xy")
  }
  if (any(sort(unique(plot)) != sort(unique(CoordAbs$Plot)))) {
    warning(
      "The plot(s) ",
      sort(unique(plot))[ sort(unique(plot)) != sort(unique(CoordAbs$Plot))],
      " won't be computed but will appeared as the label of the plot"
    )
  }

  Coord <- data.table(xy, Plot = plot)
  setnames(Coord, colnames(Coord), c("X", "Y", "Plot"))
  setDT(CoordAbs)

  # Attribute the tree to there subplot
  gridsize <- max(CoordAbs[, diff(XRel)])
  Coord[, ":="(Xplot = floor(X / gridsize), Yplot = floor(Y / gridsize))]


  # Mark NA if the tree are out of the limits of the plot
  Coord[CoordAbs[, .(Xmax = max(XRel), Ymax = max(YRel)), by = Plot],
    on = "Plot",
    ":="(Xmax = i.Xmax, Ymax = i.Ymax)
  ]
  Coord[X < 0 | X > Xmax, Xplot := as.integer(NA)]
  Coord[Y < 0 | Y > Ymax, Yplot := as.integer(NA)]


  # if trees are at the superior limit of the plot
  Coord[, ":="(Xmax = Xmax / gridsize, Ymax = Ymax / gridsize)]
  Coord[Xplot == Xmax, Xplot := Xplot - 1]
  Coord[Yplot == Ymax, Yplot := Yplot - 1]

  # assign the subplot and remove it if a tree is out of bounds
  Coord[, subplot := paste(Plot, Xplot, Yplot, sep = "_")]
  Coord[is.na(Xplot) | is.na(Yplot), subplot := as.character(NA)]
  Coord[is.na(Plot), subplot := as.character(NA)]

  return(Coord[, subplot])
}
