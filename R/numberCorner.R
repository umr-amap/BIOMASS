#' Get the UTM coordinates with the corner of the plot
#'
#' @description
#' To get the UTM coordinates from the latitude and longitude of the corners of a plot.
#' The function also numbered the corner conter clockwise, respecting the origin of the axes for the plot.
#' The corners are numbered from 1 to 4 for each plot, 1 being the origin of the axes
#' Corner numbering is done as followed:
#' \itemize{
#'    \item axis X: the corner 1 to the corner 2
#'    \item axis Y: the corner 1 to the corner 4
#' }
#'
#' @param longlat (optionnal) data frame with the coordinate in longitude latitude (eg. cbind(longitude, latitude)).
#' @param UTMcoord (optionnal) data frame with the UTM coordinate in X Y
#' @param plot A vector of codes (names) of the plots
#' @param origin A logical vector with TRUE corresponding of the origin of the axis of each plot.
#' @param clockWise A logical, do you turn clock wise between the axis X and Y
#'
#' @return A data frame with:
#' \describe{
#'   \item[Plot]{The code of the plot}
#'   \item[X]{The coordinates X in UTM}
#'   \item[Y]{The coordinates Y in UTM}
#'   \item[corner]{The corner numbers}
#' }
#' @export
#' @importFrom data.table data.table :=
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#' @examples
#' coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000
#' plot <- rep("plot1", 4)
#'
#' # if you turn clock wise
#' corner = numberCorner(UTMcoord = coord, plot = plot, origin = c(F, F, T, F), clockWise = T)
#'
#' # Plot the plot
#' plot(coord, asp = 1)
#' text(coord, labels = corner$corner, pos = 1)
#'
#'
#' # if you turn anti clock wise
#' corner = numberCorner(UTMcoord = coord, plot = plot, origin = c(F, F, T, F), clockWise = F)
#'
#' # Plot the plot
#' plot(coord, asp = 1)
#' text(coord, labels = corner$corner, pos = 1)
numberCorner <- function(longlat = NULL, UTMcoord = NULL, plot, origin, clockWise) {


  # Parameters verification -------------------------------------------------

  if (is.null(longlat) && is.null(UTMcoord)) {
    stop("Give me at least one system of coordinate")
  }
  if (!is.null(longlat) && !is.null(UTMcoord)) {
    stop("Please choose between the two coordinate")
  }
  if (!is.null(longlat)) {
    if (nrow(longlat) != length(plot)) {
      stop("Your vector plot isn't the same length of longlat")
    }
    if (nrow(longlat) != length(origin)) {
      stop("Your vector origin isn't the same length of longlat")
    }
  }
  if (!is.null(UTMcoord)) {
    if (nrow(UTMcoord) != length(plot)) {
      stop("Your vector plot isn't the same length of UTMcoord")
    }
    if (nrow(UTMcoord) != length(origin)) {
      stop("Your vector origin isn't the same length of UTMcoord")
    }
  }

  # data table --------------------------------------------------------------

  if (!is.null(longlat)) {
    Coord <- data.table(longlat, Plot = plot)
    setnames(Coord, colnames(Coord), new = c("long", "lat", "Plot"))

    Coord <- Coord[setDT(latlong2UTM(longlat)),
      on = c("long", "lat")
    ]
  }

  if (!is.null(UTMcoord)) {
    Coord <- data.table(UTMcoord, Plot = plot)
    setnames(Coord, colnames(Coord), new = c("X", "Y", "Plot"))
  }

  Coord[, origin := origin]

  # Function ----------------------------------------------------------------



  # Function to assign corner numbers
  cornerFun <- function(x, y, Origin, clockWise) {
    coord <- data.table(X = x, Y = y, n.row = 1:length(x), corner = as.numeric(NA))

    # if the plot the square is turn at 45° in relation to the horizontal
    # rotate the coordinate by 45°
    if (any(rank(coord$X) == 4)) {
      rot <- matrix(c(cos(pi / 4), sin(pi / 4), -sin(pi / 4), cos(pi / 4)), nrow = 2)
      newcoord <- as.matrix(coord[, .(X, Y)]) %*% rot

      coord[, ":="(X = newcoord[, 1], Y = newcoord[, 2]) ]
    }

    # Assign temporary corner numbers without accounting for the origin
    m1 <- coord[ rank(X) <= 2, ]
    tmp1 <- m1[rank(Y), n.row]
    m2 <- coord[ rank(X) > 2, ]
    tmp2 <- m2[rank(Y), n.row]

    if (!clockWise) {
      coord[tmp1, corner := c(1, 4)]
      coord[tmp2, corner := c(2, 3)]
    } else {
      coord[tmp1, corner := c(1, 2)]
      coord[tmp2, corner := c(4, 3)]
    }

    # Shift the corner numbers to have corner 1 on the origin
    Origin <- Origin[order(coord[, corner])]
    shift <- 5 - which(Origin)
    out <- (coord$corner + shift) %% 4
    out[which(out == 0)] <- 4

    return(out)
  }

  Coord[, corner := cornerFun(X, Y, origin, clockWise), by = Plot]

  return(as.data.frame(Coord[, .(Plot, X, Y, corner)]))
}
