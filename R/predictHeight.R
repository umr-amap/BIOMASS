#' Predicting tree height
#' 
#' The function predicts height from diameter based on a fitted model. 
#'
#' @param D Vector of diameter (in cm).
#' @param model A height-diameter model output by the function \code{link{modelHD}}
#' @param err If \code{TRUE}, An error is taken randomly from a normal distribution with a mean of 
#' zero and a standard deviation equals to the residual standard error of the model (RSE). We only use 
#' this argument for the Monte Carlo approach (see \code{\link{AGBmonteCarlo}}), otherwise it should be 
#' let as \code{FALSE}, the default case.
#'
#' @details In the case where the error is \code{FALSE} and the model is a loglog model, we use the 
#' Baskerville correction, a bias correction factor used to get unbiased backtransformation values.
#'
#' @return Returns a vector of total tree height (in m).
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY
#' @seealso \code{\link[minpack.lm]{nlsLM}}
#'
#' @examples
#' # Load a database
#' data(NouraguesHD)
#' 
#' # Create a model
#' model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
#' # Predict the height
#' H <- predictHeight(NouraguesHD$D, model)
#' 
#' @keywords Internal
#' 
#' 
predictHeight <- function(D, model, err = FALSE)
{
  ### From the diameter and the model, compute (with or without error) the heigth
  method <- model$method  
  logmod <- any(grepl("log",method))
  coeff <- model$coefficients
  
  if (!err){
    if (logmod){
      e <- 0.5*model$RSElog^2 # Baskerville correction
    } else { 
      e <- 0 # No error
    }
  } else {
    if (logmod){
      e <- rnorm(length(D), 0, model$RSElog) # Log-log error
    } else {
      e <- rnorm(length(D), 0, model$RSE) # Michaelis or Weibull error
    }
  }
  
  Hpredict <- switch(method, 
                     "log1" = exp(coeff[1] + e + coeff[2]*log(D)), 
                     "log2" = exp(coeff[1] + e + coeff[2]*log(D) + coeff[3]*log(D)^2), 
                     "log3" = exp(coeff[1] + e + coeff[2]*log(D) + coeff[3]*log(D)^2 + coeff[4]*log(D)^3), 
                     "weibull" = coeff[1]*(1 - exp(-(D/coeff[2])^coeff[3])) + e, 
                     "michaelis" = SSmicmen(D, coeff[1], coeff[2]) + e) 
  
  # If H predicted values are negative due to random error assignment
  Hpredict[Hpredict <= 0] <- 0.1
  
  return(Hpredict)
}