#' Bilinear interpolation of X and Y absolute coordinates
#'
#' Apply a bilinear interpolation to convert relative coordinates into absolute coordinates, using the 4 corners absolute coordinates of the plot.
#'
#' @param relCoord a matrix : the relative coordinates to transform
#' @param cornerCoord a data.table : the absolute corners coordinates and its numbering following a clockwise direction (names resp. "X","Y","cornerNum")
#' @param dimX a vector indicating the size of the plot on the X axis, in meters and in the relative coordinates system
#' @param dimY a vector indicating the size of the plot on the Y axis, in meters and in the relative coordinates system
#'
#' @return a data.table containing the absolute coordinates of desired points
#' 
#' @importFrom data.table data.table :=
#' 
#' @keywords Internal bilinear interpolation
#'
#' @author Arthur Bailly
#'
bilinearInterpolation = function(relCoord, cornerCoord, dimX, dimY) {
  
  # See https://en.wikipedia.org/wiki/Bilinear_interpolation#Alternative_matrix_form for details
  
  denominator <- (dimX-0)*(dimY-0)
  
  multMat <- matrix(c(dimX*dimY , -dimX*0 , -0*dimY , 0*0,
                      -dimY,0,dimY,-0,
                      -dimX,dimX,0,-0,
                      1,-1,-1,1) , ncol = 4)
  
  # Apply the bilinear interpolation formula to a single coordinate point (x;y)
  bilinearInterpolationFunction = function(x,y) {
    apply(cornerCoord[order(cornerNum),.(X,Y)][c(1,2,4,3),] , 2 , function(i) 1/denominator * i %*% multMat %*% matrix(c(1,x,y,x*y)) )
  }
  # Apply the bilinearInterpolationFunction to all points of relCoord
  absCoord = apply(relCoord , 1 , function(dat) {
    bilinearInterpolationFunction(x=dat[1] , y=dat[2])
  })
  
  return(data.table(XAbs=absCoord[1,] , YAbs=absCoord[2,]))
  
}