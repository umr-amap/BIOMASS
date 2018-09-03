predictHeight <- function(D, model, err = FALSE)
{
  ### From the diameter and the model, compute (with or without error) the heigth
  method <- model$method  
  logmod <- any(grepl("log",method))
  coeff <- model$coefficients

  if(err == FALSE & logmod) 
    e <- 0.5*model$RSElog^2 # Baskerville correction
  if(err == FALSE & !logmod) 
    e <- 0 # No error
  if(err == TRUE & logmod) 
    e <- rnorm(length(D), 0, model$RSElog) # Log-log error
  if(err == TRUE & !logmod) 
    e <- rnorm(length(D), 0, model$RSE) # Michaelis or Weibull error
  
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




