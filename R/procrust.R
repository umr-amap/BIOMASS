#' Procrust analysis
#'
#' Do a procrust analysis. X is the target matrix, Y is the matrix we want to fit to the target.
#' This function returns a translation vector and a rotation matrix
#' After the procrust problem you __must__ do the rotation before the translation.
#' __Warning :__ The order of the between the lines of the matrix X and Y must be respected. #####??????
#' __The order of the value on both matrix is important__
#'
#' @param X the target matrix
#' @param Y the matrix we want to fit to the target
#'
#' @return A list with the translation vector and the matrix of rotation
#' @keywords Internal procrust analysis
#'
#' @author Arthur PERE
#'
procrust <- function(X, Y) {
  xmean <- colMeans(X)
  ymean <- colMeans(Y)

  X <- scale(X, scale = FALSE)
  Y <- scale(Y, scale = FALSE)

  XY <- crossprod(X, Y)
  sol <- svd(XY)
  A <- sol$v %*% t(sol$u)

  b <- xmean - ymean %*% A

  return(list(rotation = A, translation = b))
}
