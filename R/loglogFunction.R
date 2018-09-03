loglogFunction <- function(data, method)
{
  ### Compute the loglog model of the H-D relationship
  
  logH <- log(data$H)
  logD <- log(data$D)
  H <- data$H
  D <- data$D
  
  if(method == "log1")
    modSelected <- lm(logH ~ logD)
  if(method == "log2")
    modSelected <- lm(logH ~ logD + I(logD^2))
  if(method == "log3")
    modSelected <- lm(logH ~ logD + I(logD^2) + I(logD^3))
  
  return(modSelected)
}
