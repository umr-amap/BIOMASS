#' Generalized bilinear interpolation of coordinates
#'
#' @description Apply a generalized bilinear interpolation to convert any coordinates from one original coordinate system to another, using the plot's 4 corner coordinates of both system.
#'
#' @details
#' The plot represented by the 4 coordinates in fromCornerCoord must have 4 right angles, i.e. a rectangular (or square) plot.
#' 
#' @references C. -C. Wei and C. -H. Chen, "Generalized Bilinear Interpolation of Motion Vectors for Quad-Tree Mesh," 2008 International Conference on Intelligent Information Hiding and Multimedia Signal Processing, Harbin, China, 2008, pp. 635-638, doi: 10.1109/IIH-MSP.2008.283.
#' 
#'
#' @param coord a matrix or data.frame : coordinates to be transformed, with X and Y corresponding to the first two columns
#' @param fromCornerCoord a matrix or data.frame : corner coordinates of the plot in the original coordinate system, with X and Y corresponding to the first two columns
#' @param toCornerCoord a matrix or data.frame : corner coordinates of the plot in the coordinate system to be projected, with the same line order as fromCornerCoord and , with X and Y corresponding to the first two columns
#' @param orderedCorner a logical, indicating if fromCornerCoord and toCornerCoord rows are sorted in correct order (clockwise or counter-clockwise)
#'
#' @return a data.frame containing the converted coordinates
#' 
#' @importFrom data.table is.data.table
#' 
#' @export
#' 
#' @keywords  generalized bilinear interpolation
#'
#' @author Arthur Bailly
#'
#' @examples 
#' fromCornerCoord <- expand.grid(X = c(0, 100), Y = c(0, 50))
#' rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
#' toCornerCoord <- as.matrix(fromCornerCoord) %*% rot_mat
#' toCornerCoord <- sweep(toCornerCoord, 2, c(50,100), FUN = "+")
#' coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
#' projCoord = bilinearInterpolation(coord = coord, fromCornerCoord = fromCornerCoord, toCornerCoord = toCornerCoord)
#' # plot(coord, xlim=c(-10,150),ylim=c(-5,200), col="blue") ; points(fromCornerCoord) ; points(projCoord , col="purple") ; points(toCornerCoord, col="red")


bilinearInterpolation = function(coord, fromCornerCoord, toCornerCoord, orderedCorner = F) {
  
  # Parameters verification
  if(nrow(fromCornerCoord)!=4 | nrow(toCornerCoord)!=4 | nrow(fromCornerCoord)!=nrow(fromCornerCoord)) {
    stop("fromCornerCoord and toCornerCoord must have 4 rows representing the 4 corners of the plot")
  }
  if(!(is.data.frame(coord) | is.matrix(coord) | is.data.table(coord))){
    stop("tree coordinates must be a data.frame, a matrix or a data.table")
  }
  if(is.data.table(coord)) coord <- data.frame(coord)
  if(is.data.table(fromCornerCoord) | is.data.table(toCornerCoord)) {
    fromCornerCoord <- data.frame(fromCornerCoord)
    toCornerCoord <- data.frame(toCornerCoord)
  }
  
  # Sorting rows if necessary
  centroid <- colMeans(fromCornerCoord)
  if(!orderedCorner) {
    # Sort fromCornerCoord and toCornerCoord rows in a counter-clockwise direction
    angles <- atan2(fromCornerCoord[, 2] - centroid[2], fromCornerCoord[, 1] - centroid[1])
    fromCornerCoord <- fromCornerCoord[order(angles), ]
    toCornerCoord <- toCornerCoord[order(angles), ]
  }

  # Verification of a rectangular plot for fromCornerCoord
  if(!all(abs(dist(rbind(fromCornerCoord,centroid))[c(4,7,9,10)] - mean(dist(rbind(fromCornerCoord,centroid))[c(4,7,9,10)]))<0.1)) {
    stop("The plot in the relative coordinate system is not a rectangle (or a square). You may consider using trustGPScorners = F")
  }
  
  x_A <- fromCornerCoord[1,1] ; x_B <- fromCornerCoord[2,1] ; x_C <- fromCornerCoord[3,1]  ; x_D <- fromCornerCoord[4,1]
  y_A <- fromCornerCoord[1,2] ; y_B <- fromCornerCoord[2,2] ; y_C <- fromCornerCoord[3,2] ; y_D <- fromCornerCoord[4,2]
  u_A <- toCornerCoord[1,1] ; u_B <- toCornerCoord[2,1];  u_C <- toCornerCoord[3,1];  u_D <- toCornerCoord[4,1] 
  v_A <- toCornerCoord[1,2] ; v_B <- toCornerCoord[2,2] ; v_C <- toCornerCoord[3,2] ; v_D <- toCornerCoord[4,2] 
  
  apply_bilinear_interpolation <- function(x,y) {
    rate_A <- (1-(x-x_A)/(x_C-x_A)) * (1-(y-y_A)/(y_C-y_A))
    rate_B <- (1-(x-x_B)/(x_D-x_B)) * (1-(y-y_B)/(y_D-y_B))
    rate_C <- (1-(x-x_C)/(x_A-x_C)) * (1-(y-y_C)/(y_A-y_C))
    rate_D <- (1-(x-x_D)/(x_B-x_D)) * (1-(y-y_D)/(y_B-y_D))
    return(data.frame(
      X = rate_A*u_A + rate_B*u_B + rate_C*u_C + rate_D*u_D,
      Y = rate_A*v_A + rate_B*v_B + rate_C*v_C + rate_D*v_D)
    )
  }
  
  return(apply_bilinear_interpolation(x=coord[,1],y=coord[,2]))
}
