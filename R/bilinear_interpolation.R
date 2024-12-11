#' Generalized bilinear interpolation of coordinates
#'
#' @description Apply a generalized bilinear interpolation to convert any coordinates from one original coordinate system to another, using the plot's 4 corner coordinates of both system.
#'
#' @details
#' The plot represented by the 4 coordinates in from_corner_coord must have 4 right angles, i.e. a rectangular (or square) plot.
#' 
#' When ordered_corner = FALSE, the function automatically reassigns corners in a counter-clockwise order.
#' 
#' @references C. -C. Wei and C. -H. Chen, "Generalized Bilinear Interpolation of Motion Vectors for Quad-Tree Mesh," 2008 International Conference on Intelligent Information Hiding and Multimedia Signal Processing, Harbin, China, 2008, pp. 635-638, doi: 10.1109/IIH-MSP.2008.283.
#' 
#'
#' @param coord a matrix or data.frame : coordinates to be transformed, with X and Y corresponding to the first two columns
#' @param from_corner_coord a matrix or data.frame : corner coordinates of the rectangular plot in the original coordinate system, with X and Y corresponding to the first two columns
#' @param to_corner_coord a matrix or data.frame : corner coordinates of the plot in the coordinate system to be projected, with the same line order as from_corner_coord and , with X and Y corresponding to the first two columns
#' @param ordered_corner a logical, if TRUE : indicating that from_corner_coord and to_corner_coord rows are sorted in correct order (clockwise or counter-clockwise)
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
#' from_corner_coord <- expand.grid(X = c(0, 100), Y = c(0, 50))
#' rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
#' to_corner_coord <- as.matrix(from_corner_coord) %*% rot_mat
#' to_corner_coord <- sweep(to_corner_coord, 2, c(50,100), FUN = "+")
#' coord <- expand.grid(X = seq(0,100,10), Y = seq(0,50,5))
#' projCoord = bilinear_interpolation(coord = coord,
#'                                    from_corner_coord = from_corner_coord,
#'                                    to_corner_coord = to_corner_coord)

bilinear_interpolation = function(coord, from_corner_coord, to_corner_coord, ordered_corner = F) {
  
  # Parameters verification
  if(nrow(from_corner_coord)!=4 | nrow(to_corner_coord)!=4 | nrow(from_corner_coord)!=nrow(from_corner_coord)) {
    stop("from_corner_coord and to_corner_coord must have 4 rows representing the 4 corners of the plot")
  }
  if(!(is.data.frame(coord) | is.matrix(coord) | is.data.table(coord))){
    stop("tree coordinates must be a data.frame, a matrix or a data.table")
  }
  if(is.data.table(coord)) coord <- data.frame(coord)
  if(is.data.table(from_corner_coord) | is.data.table(to_corner_coord)) {
    from_corner_coord <- data.frame(from_corner_coord)
    to_corner_coord <- data.frame(to_corner_coord)
  }
  
  # to_corner_coord colnames attribution
  if(is.null(colnames(to_corner_coord))) {
    to_corner_coord <- to_corner_coord[,1:2]
    colnames(to_corner_coord) <- c("x_interp","y_interp")
  }
  
  # Sorting rows if necessary
  centroid <- colMeans(from_corner_coord[,1:2])
  if(!ordered_corner) {
    # Sort from_corner_coord and to_corner_coord rows in a counter-clockwise direction
    angles <- atan2(from_corner_coord[, 2] - centroid[2], from_corner_coord[, 1] - centroid[1])
    from_corner_coord <- from_corner_coord[order(angles), ]
    to_corner_coord <- to_corner_coord[order(angles), ]
  }

  # Verification of a rectangular plot for from_corner_coord
  if(!all(abs(stats::dist(rbind(from_corner_coord[,1:2],centroid))[c(4,7,9,10)] - mean(stats::dist(rbind(from_corner_coord[,1:2],centroid))[c(4,7,9,10)]))<0.01)) {
    stop("The plot in the relative coordinate system is not a rectangle (or a square). You may consider using trustGPScorners = F")
  }
  
  x_A <- from_corner_coord[1,1] ; x_B <- from_corner_coord[2,1] ; x_C <- from_corner_coord[3,1]  ; x_D <- from_corner_coord[4,1]
  y_A <- from_corner_coord[1,2] ; y_B <- from_corner_coord[2,2] ; y_C <- from_corner_coord[3,2] ; y_D <- from_corner_coord[4,2]
  u_A <- to_corner_coord[1,1] ; u_B <- to_corner_coord[2,1];  u_C <- to_corner_coord[3,1];  u_D <- to_corner_coord[4,1] 
  v_A <- to_corner_coord[1,2] ; v_B <- to_corner_coord[2,2] ; v_C <- to_corner_coord[3,2] ; v_D <- to_corner_coord[4,2] 
  
  apply_bilinear_interpolation <- function(x,y,to_corner_coord_colnames) {
    rate_A <- (1-(x-x_A)/(x_C-x_A)) * (1-(y-y_A)/(y_C-y_A))
    rate_B <- (1-(x-x_B)/(x_D-x_B)) * (1-(y-y_B)/(y_D-y_B))
    rate_C <- (1-(x-x_C)/(x_A-x_C)) * (1-(y-y_C)/(y_A-y_C))
    rate_D <- (1-(x-x_D)/(x_B-x_D)) * (1-(y-y_D)/(y_B-y_D))
    interp_df <- data.frame(
      rate_A*u_A + rate_B*u_B + rate_C*u_C + rate_D*u_D,
      rate_A*v_A + rate_B*v_B + rate_C*v_C + rate_D*v_D
    )
    setnames(interp_df, new = to_corner_coord_colnames)
    interp_df
  }
  
  return(apply_bilinear_interpolation(x=coord[,1],y=coord[,2],to_corner_coord_colnames=colnames(to_corner_coord)[1:2]))
}
