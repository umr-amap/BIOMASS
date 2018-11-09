#' Procrust analysis
#'
#' Do the procrust analysis X is the target matrix, Y is the matrix we want to fit to the target.
#' This procrust analysis give just a translation vector and a rotation matrix
#' After the procrust problem you \textbf{must} do the rotation before the translation.
#' \textbf{Warning :} The order of the between the lines of the matrix X and Y must be respected.
#'
#'
#' \textbf{The order of the value on both matrix is important}
#'
#' @param X the target matrix
#' @param Y the matrix we want to fit to the target
#'
#' @return A list with the translation vector and the matrix of rotation
#' @keywords Internal procrust analysis
#'
#' @author Arthur PERE
#'
#' @examples
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
