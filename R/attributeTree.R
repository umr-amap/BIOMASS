#' Attribute the tree to the subplot
#'
#' @param x The x coordinate of the tree for each plot
#' @param y The y coordinate of the tree for axis
#' @param Plot The label of the plot (same length as x and y)
#' @param CoordAbs Output of the function cutTree
#'
#' @return A vector with the subplot for each trees
#' @export
#' @author Arthur PERE
#' @importFrom data.table data.table setDT between
#'
#' @examples
#'
#'
#'
#'
attributeTree <- function(x, y, Plot, CoordAbs) {


  # parameters verification -------------------------------------------------

  if (length(x) != length(y)) {
    stop("Your x and y vector haven't the same length")
  }
  if (length(x) != length(Plot)) {
    stop("Your Plot vector haven't the same length as the other x and y")
  }
  if (any(sort(unique(Plot)) != sort(unique(CoordAbs$Plot)))) {
    warning(
      "The plot(s) ",
      sort(unique(Plot))[ sort(unique(Plot)) != sort(unique(CoordAbs$Plot))],
      " won't be computed but will appeared as the label of the plot"
    )
  }

  Coord <- data.table(X = x, Y = y, Plot = Plot)
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
