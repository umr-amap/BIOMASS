if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "plot", "X", "Y",
    "XRel", "YRel", "XAbs", "YAbs"
  ))
}

#' Attribute trees to GPS coordinates
#'
#' @param xy The coordinates of the trees for each plot
#' @param plot The label of the plot (same length as the number of rows of `xy`)
#' @param coordGPS (optional) The GPS coordinate of the plot
#' @param coordRel (optional) The relative coordinate for the GPS points of the plot
#' @param plotCoord (optional) The name of the plot for the differents GPS points
#' @param coordAbs (optional) The result of the function [cutPlot()]
#'
#' @return A data frame with two columns :
#'          - `X`: The `X` coordinate in whichever projection you have
#'          - `Y`: The `X` coordinate in whichever projection you have
#' @export
#'
#' @importFrom data.table setDT
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
attributeTreeCoord <- function(xy, plot, coordGPS = NULL, coordRel = NULL, plotCoord = NULL, coordAbs = NULL) {
  
  
  # parameters verification -------------------------------------------------
  
  if (is.null(coordAbs)) {
    if (is.null(coordGPS) && is.null(coordRel) && is.null(plotCoord)) {
      stop("One of those arguments is null : coordGPS, coordRel, plotCoord")
    }
    stopifnot(
      ncol(coordGPS) == 2,
      ncol(coordRel) == 2,
      nrow(coordRel) == nrow(coordGPS),
      is.vector(plotCoord) | is.factor(plotCoord),
      length(plotCoord) %in% c(1, nrow(coordGPS))
    )
    
    data <- data.table(plotCoord, coordRel, coordGPS)
  } else {
    data <- coordAbs[, c("plot", "XRel", "YRel", "XAbs", "YAbs")]
    setDT(data)
  }
  
  stopifnot(
    ncol(xy) == 2,
    is.vector(plot) | is.factor(plot),
    length(plot) %in% c(1, nrow(xy))
  )
  
  
  # function ----------------------------------------------------------------
  
  dataTree <- data.table(plot, xy)
  setnames(dataTree, names(dataTree), c("plot", "X", "Y"))
  
  setnames(data, names(data), c("plot", "XRel", "YRel", "XAbs", "YAbs"))
  data <- data[plot %in% unique(dataTree$plot)]
  
  if (!any(dataTree$plot %in% data$plot)) {
    stop("The names in 'plot' and 'plotCoord' are not corresponding")
  }
  
  output <- rbindlist(lapply(
    split(data, by = "plot", keep.by = T),
    function(subData) {
      res <- procrust(subData[, .(XAbs, YAbs)], subData[, .(XRel, YRel)])
      
      subDataTree <- as.matrix(dataTree[ plot == unique(subData$plot), .(X, Y) ])
      
      subDataTree <- subDataTree %*% res$rotation
      subDataTree <- sweep(subDataTree, 2, res$translation, FUN = "+")
      
      return(list(X = subDataTree[, 1], Y = subDataTree[, 2]))
    }
  ))
  
  return(as.data.frame(output))
}