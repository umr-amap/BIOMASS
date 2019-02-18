if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "plot", "X", "Y", ".BY", "Xproj", "Yproj"
  ))
}

#' Attribute trees to GPS coordinates
#'
#' @param xy The coordinates of the trees for each plot
#' @param plot The label of the plot (same length as the number of rows of `xy` or length of 1)
#' @param dim The dimension of the plot (you can make two dimension in a vector)
#' @param coordAbs (optional) The result of the function [cutPlot()] or [numberCorner()]
#'
#' @return A data frame with two columns :
#'          - `Xproj`: The `X` coordinate in whichever projection you have
#'          - `Yproj`: The `Y` coordinate in whichever projection you have
#' @export
#'
#' @importFrom data.table setDT setnames
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
#' attributeTreeCoord(xy, plot, coordAbs = cut)
attributeTreeCoord <- function(xy, plot, dim, coordAbs) {


  # parameters verification -------------------------------------------------
  setDT(coordAbs)
  setnames(coordAbs, c("XAbs", "YAbs"), c("X", "Y"), skip_absent = T)

  if (!length(plot) %in% c(1, nrow(xy))) {
    stop("The 'plot' vector must have the length equal to 1 or nrow(xy)")
  }

  if (!all(c("plot", "corner", "X", "Y") %in% names(coordAbs))) {
    stop("The column 'plot', 'corner', 'X' (or 'XAbs'), 'Y' (or 'YAbs') are compulsory for the data frame 'coordAbs'")
  }

  if (!all(unique(plot) %in% unique(coordAbs$plot))) {
    stop("Not all the plot in the vector 'plot' are in the data frame coordAbs")
  }


  if (!length(dim) %in% c(1, 2)) {
    stop("Incorect dimension vector must be length of 1 or 2")
  }

  # put the dimension on the X and Y
  if (length(dim) == 1) {
    dimX <- dim
    dimY <- dim
  } else {
    dimX <- dim[1]
    dimY <- dim[2]
  }


  # function ----------------------------------------------------------------

  setDT(xy)
  setnames(xy, names(xy), c("X", "Y"))

  if ("subplot" %in% names(coordAbs)) { # if we have subplot then attribute the trees to all subplot
    xy$plot <- attributeTree(xy, plot, coordAbs)
    coordAbs[, plot := subplot] # and the subplot became the plot
  } else {
    xy$plot <- plot
  }

  xy[, ":="(X = X / dimX, Y = Y / dimY)] # divide all the coordinate by the dimension


  proj <- function(XY, cornCoord) { # project all the coordinate on the projected coordinate
    setDT(XY)
    setnames(XY, names(XY), c("X", "Y"))

    lapply(c("X", "Y"), function(col) {
      XY[, (1 - Y) * (1 - X) * cornCoord[corner == 1, eval(parse(text = col))] +
        X * (1 - Y) * cornCoord[corner == 2, eval(parse(text = col))] +
        Y * X * cornCoord[corner == 3, eval(parse(text = col))] +
        Y * (1 - X) * cornCoord[corner == 4, eval(parse(text = col))]
        ]
    })
  }

  xy[, c("Xproj", "Yproj") := proj(.(X, Y), coordAbs[coordAbs$plot == .BY, c("X", "Y", "corner"), with = F]), by = plot]

  return(as.data.frame(xy[, .(Xproj, Yproj)]))
}
