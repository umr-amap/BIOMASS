if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "n.row", "corner", "X", "Y", "plot"
  ))
}

#' Get the UTM coordinates with the corner of the plot
#'
#' @description
#' Get the UTM coordinates from the latitude and longitude of the corners of a plot.
#' The function also assign a number to the corners in a clockwise or counterclockwise way, with the number 1 for the XY origin.
#' Corner numbering is done as followed:
#'   - axis X: the corner 1 to the corner 2
#'   - axis Y: the corner 1 to the corner 4
#'
#'
#' @param longlat (optional) data frame with the coordinates in longitude latitude (eg. cbind(longitude, latitude)).
#' @param projCoord (optional) data frame with the projected coordinates in X Y
#' @param plot A vector of codes (names) of the plots
#' @param origin A logical vector with TRUE corresponding of the origin of the axis of each plot.
#' @param clockWise A logical, whether the numbering should be done in a clockwise (TRUE) or counterclockwise (FALSE) way.
#'
#' @return A data frame with:
#'   - `plot`: The code of the plot
#'   - `X`: The coordinates X in UTM
#'   - `Y`: The coordinates Y in UTM
#'   - `corner`: The corner numbers
#'
#' @export
#' @importFrom data.table data.table :=
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#' @examples
#' coord <- data.frame(X = c(0, 200, 0, 200), Y = c(0, 0, 200, 200)) + 5000
#' plot <- rep("plot1", 4)
#' origin <- c(FALSE, FALSE, TRUE, FALSE)
#'
#' # if you turn clock wise
#' corner <- numberCorner(projCoord = coord, plot = plot, origin = origin, clockWise = TRUE)
#'
#' # Plot the plot
#' plot(coord, asp = 1)
#' text(coord, labels = corner$corner, pos = 1)
#'
#'
#' # Using a counterclockwise way
#' corner <- numberCorner(projCoord = coord, plot = plot, origin = origin, clockWise = FALSE)
#'
#' # Plot the plot
#' plot(coord, asp = 1)
#' text(coord, labels = corner$corner, pos = 1)
numberCorner <- function(longlat = NULL, projCoord = NULL, plot, origin, clockWise) {


  # Parameters verification -------------------------------------------------

  if (is.null(longlat) && is.null(projCoord)) {
    stop("Give at least one set of coordinates: longlat or projCoord")
  }
  if (!is.null(longlat) && !is.null(projCoord)) {
    stop("Give only one set of coordinates: longlat or projCoord")
  }
  if (length(plot) != length(origin)) {
    stop("Vectors plot and origin have not the same length")
  }

  if (!is.null(longlat) && !is.data.frame(longlat)) {
    longlat <- as.data.frame(longlat, fix.empty.names = FALSE)
  }
  if (!is.null(projCoord) && !is.data.frame(projCoord)) {
    projCoord <- as.data.frame(projCoord, fix.empty.names = FALSE)
  }

  if (!is.null(longlat) && nrow(longlat) != length(plot)) {
    stop("The length of vectors plot and origin is different from the number of rows of longlat")
  }
  if (!is.null(projCoord) && nrow(projCoord) != length(plot)) {
    stop("The length of vectors plot and origin is different from the number of rows of projCoord")
  }
  tab <- as.numeric(table(plot))
  if (any(as.numeric(table(plot)) != 4)) {
    stop(
      "Lenght of vector plot is not 4, the plot(s):\n\t\t",
      paste(names(table(plot)[ table(plot) != 4 ]), collapse = " "),
      "\nhave:\n\t\t",
      paste(table(plot)[ table(plot) != 4 ], collapse = " "),
      "\ncorner respectively"
    )
  }
  tab <- as.matrix(table(plot, origin))
  if (any(tab[, 1] != 3) || any(tab[, 2] != 1)) {
    stop(
      "Please verify your 'origin' vector, it should contain 1 TRUE and 3 FALSE by plot, those plot(s) are:\n\t\t",
      paste(rownames(tab)[tab[, 1] != 3 | tab[, 2] != 1], collapse = " ")
    )
  }


  # data table --------------------------------------------------------------

  if (!is.null(longlat)) {
    Coord <- data.table(longlat, plot = plot)
    setnames(Coord, colnames(Coord), new = c("long", "lat", "plot"))

    Coord <- Coord[setDT(latlong2UTM(cbind(long, lat))),
      on = c("long", "lat")
    ]
  }

  if (!is.null(projCoord)) {
    Coord <- data.table(projCoord, plot = plot)
    setnames(Coord, colnames(Coord), new = c("X", "Y", "plot"))
  }

  Coord[, origin := origin]

  # Function ----------------------------------------------------------------



  # Function to assign corner numbers
  cornerFun <- function(x, y, Origin, clockWise) {
    coord <- data.table(X = x, Y = y, n.row = 1:length(x), corner = as.numeric(NA))

    # if the plot the square is turn at 45° in relation to the horizontal ######??????
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

  Coord[, corner := cornerFun(X, Y, origin, clockWise), by = plot]

  return(as.data.frame(Coord[, .(plot, X, Y, corner)]))
}
