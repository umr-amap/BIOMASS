#' Printing model's information
#' 
#' This function prints a set of statistics (RSE, Average Bias) to assess the quality of the fit. 
#'
#' @param H Vector of the observed heights
#' @param Hpredict Vector of the predicted heights
#' @param mod A model obtained by functions of type \code{\link{lm}}. 
#'
#' @author Ariane TANGUY, Maxime REJOU-MECHAIN
#'
#' @examples
#' # Load a database
#' data(NouraguesHD)
#' 
#' # Create a model
#' model <- lm(log(NouraguesHD$H)~log(NouraguesHD$D))
#' 
#' plot(log(na.omit(NouraguesHD$H)), model$fitted.values)
#' printFitData(H = NouraguesHD$H, Hpredict = model$fitted.values, mod = model)
#' 
#' @keywords internal
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
