#' Modeling height-diameter relationship with Michaelis-Menten function
#'
#' Construct a Michaelis Menten model of the form: \deqn{H = (A * D) / (B + D)} (A and B are the model parameters to be estimated)
#'
#' @param data Dataset with the informations of height (H) and diameter (D)
#' @param weight (optional) Vector indicating observation weights in the model.
#'
#' @return This function give an output similar to the one given by \code{\link{lm}}, obtained for \code{michaelisFunction} from \code{\link[minpack.lm]{nlsLM}}).
#' @references
#' Michaelis, L., & Menten, M. L. (1913). \emph{Die kinetik der invertinwirkung}. Biochem. z, 49(333-369), 352.
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY
#' 
#' @importFrom minpack.lm nls.lm.control nlsLM
#' 
michaelisFunction <- function(data, weight = NULL)
{
  ### Compute the michaelis model of the H-D relationship
  
  H <- data$H
  D <- data$D
  
  count <- 1
  maxIter <- 50
  converge <- FALSE
  
  if(is.null(weight))
  {
    while(converge == FALSE && count <= 10)
    {
      tt <- tryCatch(nlsLM(H ~ SSmicmen(D, A, B), 
                           control = nls.lm.control(maxiter = maxIter)),
                     error = function(e) e, 
                     warning = function(w) w)
      
      if(is(tt, "warning"))
      {
        count <- count + 1
        maxIter <- maxIter + 50
      }
      else
        converge <- TRUE
    }
    model <- nlsLM(H ~ SSmicmen(D, A, B), 
                   control = nls.lm.control(maxiter = maxIter))
  }
  else
  {
    while(converge == FALSE && count <= 10)
    {
      tt <- tryCatch(nlsLM(H ~ SSmicmen(D, A, B), 
                           weights = weight,
                           control = nls.lm.control(maxiter = maxIter)),
                     error = function(e) e, 
                     warning = function(w) w)
      
      if(is(tt, "warning"))
      {
        count <- count + 1
        maxIter <- maxIter + 50
      }
      else
        converge <- TRUE
    }
    model <- nlsLM(H ~ SSmicmen(D, A, B), 
                   weights = weight,
                   control = nls.lm.control(maxiter = maxIter))
  }
  return(model)
}
