printFitData <- function(H, Hpredict, mod)
{
  res <- H - Hpredict # residuals
  RSE <- sqrt(sum(res^2)/summary(mod)$df[2]) # Residual standard error
  bias <- (mean(Hpredict) - mean(H))/mean(H)
  
  if(any(grepl("log",mod$call)))
    cat(paste("----- RSE = ", round(RSE, 4), " (RSElog = ", round(summary(mod)$sigma, 4), ") \n",sep=""))
  else
    cat(paste("----- RSE = ", round(RSE, 4)," \n"))
  cat(paste("----- Average bias = ", round(bias, digits = 4), "\n"))
  cat("\n")
}
