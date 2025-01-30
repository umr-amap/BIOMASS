if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "plot", "X", "Y", ".BY", "Xproj", "Yproj", "XRel", "YRel"
  ))
}

#' Attribute GPS coordinates to trees
#'
#' @param xy The relative coordinates of the trees within each plot
#' @param plot The label of the plot (same length as the number of rows of `xy` or length of 1)
#' @param dim The dimension of the plot (either one value if the plot is a square or a vector if a rectangle)
#' @param coordAbs The result of the function [cutPlot()] or [numberCorner()]
#'
#' @return A data frame with two columns:
#'          - `Xproj`: The `X` coordinates in the absolute coordinate system
#'          - `Yproj`: The `Y` coordinates in the absolute coordinate system
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
#' Forestplot <- rep(c("plot1", "plot2"), each = 4)
#'
#' Outcut <- cutPlot(coord, Forestplot, corner, gridsize = 100, dimX = 200, dimY = 200)
#'
#'
#' # Assign a plot to 200 trees
#' Forestplot <- rep(c("plot1", "plot2"), 100)
#'
#' # attribute trees to subplots
#' attributeTreeCoord(xy, Forestplot, dim =100,coordAbs = Outcut)
attributeTreeCoord <- function(xy, plot, dim, coordAbs) {

  .Deprecated(msg = "'attributeTreeCoord()' is deprecated and will be removed in the next version. The projected tree coordinates are now retrieved by the `check_plot_coord()` function\nPlease see the vignette `Spatialized trees and forest stand metrics with BIOMASS`")
  
  # parameters verification -------------------------------------------------
  setDT(coordAbs)
  setnames(coordAbs, c("XAbs", "YAbs","cornerNum"), c("X", "Y","corner"), skip_absent = TRUE)

  if (!length(plot) %in% c(1, nrow(xy))) {
    stop("The 'plot' vector must have a length equal to 1 or nrow(xy)")
  }

  if (!all(c("plot", "corner", "X", "Y") %in% names(coordAbs))) {
    stop("The column 'plot', 'corner', 'X' (or 'XAbs'), 'Y' (or 'YAbs') are compulsory for the data frame 'coordAbs'")
  }

  if (!all(unique(plot) %in% unique(coordAbs$plot))) {
    stop("Some plots in the vector 'plot' are absent from the data frame coordAbs")
  }


  if (!length(dim) %in% c(1, 2)) {
    stop("Incorrect dimension vector, must be length of 1 or 2")
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

  xy <- data.table(plot, xy)
  setnames(xy, names(xy), c("plot", "X", "Y"))
  xy[, order := .I]

  if ("subplot" %in% names(coordAbs)) { # if we have subplot
    out <- rbindlist(lapply(
      split(coordAbs, by = "plot", keep.by = TRUE),
      function(subData) {
        res <- procrust(subData[, .(X, Y)], subData[, .(XRel, YRel)])

        subDataTree <- as.matrix(xy[ plot == unique(subData$plot), .(X, Y) ])

        subDataTree <- subDataTree %*% res$rotation
        subDataTree <- sweep(subDataTree, 2, res$translation, FUN = "+")

        return(list(Xproj = subDataTree[, 1], Yproj = subDataTree[, 2], order = xy[ plot == unique(subData$plot), order ]))
      }
    ))
  } else {
    xy[, ":="(X = X / dimX, Y = Y / dimY)] # divide all the coordinates by the dimension

    out <- rbindlist(lapply(split(coordAbs, by = "plot"), function(subData) {
      XY = xy[ plot == unique(subData$plot), .(X, Y, order) ]
      
      out = lapply(c("X", "Y"), function(col) {
        XY[, (1 - Y) * (1 - X) * subData[corner == 1, eval(parse(text = col))] +
             X * (1 - Y) * subData[corner == 2, eval(parse(text = col))] +
             Y * X * subData[corner == 3, eval(parse(text = col))] +
             Y * (1 - X) * subData[corner == 4, eval(parse(text = col))]
           ]
      })
      
      return(list(Xproj = out[[1]], Yproj = out[[2]], order = XY[, order]))
    }))
  }
  return(as.data.frame(out[order(order), .(Xproj, Yproj)]))
}
