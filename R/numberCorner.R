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
#'   \item[codeUTM]{The UTM proj4 code}
#' }
#' @export
#' @importFrom data.table data.table :=
#' @author Arthur PERE, Maxime REJOU-MECHAIN
#' @examples
numberCorner <- function(longlat = NULL, UTMcoord = NULL, plot, origin, clockWise) {
  if (is.null(longlat) && is.null(UTMcoord)) {
    stop("Give me at least one system of coordinate")
  }
  if (!is.null(longlat) && !is.null(UTMcoord)) {
    stop("Please choose between the two coordinate")
  }

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


  # Function to assign corner numbers
  cornerFun <- function(x, y, Origin, clockWise) {
    coord <- data.table(X = x, Y = y, n.row = 1:length(x), corner = as.numeric(NA))

    # Assign temporary corner numbers without accounting for the origin
    m1 <- coord[ rank(X) <= 2, ]
    tmp1 <- m1[rank(Y), n.row]
    m2 <- coord[ rank(X) > 2, ]
    tmp2 <- m2[rank(Y), n.row]

    if (clockWise) {
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

  return(as.data.frame(Coord[, .(Plot, X, Y, corner, codeUTM)]))
}
