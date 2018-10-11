#' Modeling height-diameter relationship with loglog function
#'
#' These functions model the relationship between tree height (H) and diameter (D).
#' \bold{loglogFunction} 
#' Compute three types of log model (log, log2 and log3) to model H from D.
#' The model can be:
#' \itemize{
#'    \item log 1: \eqn{log(H) = a+ b*log(D)} (equivalent to a power model)
#'    \item log 2: \eqn{log(H) = a+ b*log(D) + c*log(D)^2}
#'    \item log 3: \eqn{log(H) = a+ b*log(D) + c*log(D)^2 + d*log(D)^3}
#' }
#'
#'
#' @param data Input data with the column H (Heigth) and D (Diameter)
#' @param method either "log1", "log2", or "log3"
#'
#' @return This function return \code{\link{lm}} output.
#' @references
#' Michaelis, L., & Menten, M. L. (1913). \emph{Die kinetik der invertinwirkung}. Biochem. z, 49(333-369), 352.
#' Weibull, W. (1951). \emph{Wide applicability}. Journal of applied mechanics, 103.
#' Baskerville, G. L. (1972). \emph{Use of logarithmic regression in the estimation of plant biomass}. 
#' Canadian Journal of Forest Research, 2(1), 49-53.
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY
#'

loglogFunction <- function(data, method)
{
  ### Compute the loglog model of the H-D relationship
  
  logH <- log(data$H)
  logD <- log(data$D)
  
  if(method == "log1")
    modSelected <- lm(logH ~ logD)
  if(method == "log2")
    modSelected <- lm(logH ~ logD + I(logD^2))
  if(method == "log3")
    modSelected <- lm(logH ~ logD + I(logD^2) + I(logD^3))
  
  return(modSelected)
}
