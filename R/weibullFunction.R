#' Modeling height-diameter relationship with Weibull function
#'
#' This function model the relationship between tree height (H) and diameter (D).
#' Construct a three parameter Weibull model of the form: \deqn{H = a*(1-exp(-(D/b)^c))} (a, b, c are the model parameters to be estimated)
#'
#' @param data Dataset with the informations of height (H) and diameter (D)
#' @param weight (optional) Vector indicating observation weights in the model.
#'
#' @return This function give an output similar to the one given by \code{\link{lm}}, obtained for \code{weibullFunction} from \code{\link[minpack.lm]{nlsLM}}).
#' @references 
#' Weibull, W. (1951). \emph{Wide applicability}. Journal of applied mechanics, 103.
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY
#' @seealso \code{\link{modelHD}}, \code{\link[lmfor]{HDmodels}}
#' 
#' @importFrom minpack.lm nlsLM nls.lm.control

weibullFunction <- function(data, weight = NULL) {
  ### Compute the weibull model of the H-D relationship
  
  H <- data$H
  D <- data$D
  
  Hmax <- quantile(H, probs = 0.90, na.rm = TRUE)
  init <- list(a = as.double(Hmax), b = 24.9, c = 0.8)
  
  count <- 1
  maxIter <- 50
  converge <- FALSE
  
  if(is.null(weight)) {
    while(converge == FALSE && count <= 10) {
      tt <- tryCatch(nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                           start = init, 
                           data = data, 
                           control = nls.lm.control(maxiter = maxIter)),
                     error = function(e) e, 
                     warning = function(w) w)
      
      if(is(tt, "warning")) {
        count <- count + 1
        maxIter <- maxIter + 50
      } else{
        converge <- TRUE
      }
    }
    
    model <- nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                   start = init, 
                   data = data, 
                   control = nls.lm.control(maxiter = maxIter))
  } else {
    while(converge == FALSE && count <= 10) {
      tt <- tryCatch(nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                           start = init, 
                           data = data, 
                           weights = weight,
                           control = nls.lm.control(maxiter = maxIter)),
                     error = function(e) e, 
                     warning = function(w) w)
      
      if(is(tt, "warning")){
        count <- count + 1
        maxIter <- maxIter + 50
      } else {
        converge <- TRUE
      }
    }
    
    model <- nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                   start = init, 
                   data = data, 
                   weights = weight,
                   control = nls.lm.control(maxiter = maxIter))
  }
  return(model)
}