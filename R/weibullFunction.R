weibullFunction <- function(data, weight = NULL)
{
  ### Compute the weibull model of the H-D relationship
  
  H <- data$H
  D <- data$D
  
  Hmax <- quantile(H, probs = 0.90, na.rm = TRUE)
  init <- list(a = as.double(Hmax), b = 24.9, c = 0.8)
  
  count <- 1
  maxIter <- 50
  converge <- FALSE
  
  if(is.null(weight))
  {
    while(converge == FALSE && count <= 10)
    {
      tt <- tryCatch(minpack.lm::nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                                       start = init, 
                                       data = data, 
                                       control = minpack.lm::nls.lm.control(maxiter = maxIter)),
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
    
    model <- minpack.lm::nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                               start = init, 
                               data = data, 
                               control = minpack.lm::nls.lm.control(maxiter = maxIter))
  }
  else
  {
    while(converge == FALSE && count <= 10)
    {
      tt <- tryCatch(minpack.lm::nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                                       start = init, 
                                       data = data, 
                                       weights = weight,
                                       control = minpack.lm::nls.lm.control(maxiter = maxIter)),
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
    
    model <- minpack.lm::nlsLM(H ~ a*(1-exp(-(D/b)^c)), 
                               start = init, 
                               data = data, 
                               weights = weight,
                               control = minpack.lm::nls.lm.control(maxiter = maxIter))
  }
  return(model)
}
