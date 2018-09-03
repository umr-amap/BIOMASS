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
      tt <- tryCatch(minpack.lm::nlsLM(H ~ SSmicmen(D, A, B), 
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
    model <- minpack.lm::nlsLM(H ~ SSmicmen(D, A, B), 
                               control = minpack.lm::nls.lm.control(maxiter = maxIter))
  }
  else
  {
    while(converge == FALSE && count <= 10)
    {
      tt <- tryCatch(minpack.lm::nlsLM(H ~ SSmicmen(D, A, B), 
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
    model <- minpack.lm::nlsLM(H ~ SSmicmen(D, A, B), 
                               weights = weight,
                               control = minpack.lm::nls.lm.control(maxiter = maxIter))
  }
  return(model)
}
