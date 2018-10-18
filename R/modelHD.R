#' Fitting height-diameter model
#'
#' This function fits and compares (optional) height-diameter models.
#'
#' @param D Vector with diameter measurements (in cm). NA values are accepted but a 
#' minimum of 10 valid entries (i.e. having a corresponding height in H) is required.
#' @param H Vector with total height measurements (in m). NA values are accepted but a minimum of 10 valid entries (i.e. having a corresponding diameter in D) is required.
#' @param method Method used to fit the relationship. 
#' To be chosen between: 
#'   \itemize{
#'     \item log1, log2, log3
#'     \itemize{
#'       \item log 1: \eqn{(log(H) = a+ b*log(D))} (equivalent to a power model)
#'       \item log 2: \eqn{(log(H) = a+ b*log(D) + c*log(D)^2)}
#'       \item log 3: \eqn{(log(H) = a+ b*log(D) + c*log(D)^2 + d*log(D)^3)}
#'     }
#'     \item weibull: \eqn{H = a*(1-exp(-(D/b)^c))}
#'     \item michaelis: \eqn{H = (A * D)/(B + D)}
#'   }
#' If \code{NULL}, all the methods will be compared.
#' @param useWeight If weight is \code{TRUE}, model weights will be \eqn{(D^2)*H} (i.e. weights are proportional to tree volume, so that larger trees have a stronger influence during the construction of the model).
#' @param drawGraph If \code{TRUE}, a graphic will illustrate the relationship between H and D. 
#'
#' @details All the back transformations in loglog are done using the Baskerville correction (\eqn{0.5 * RSE^2}, where RSE is the Residual Standard Error). 
#'
#'
#' @return Returns a list with if the parameter model is not null: 
#' \describe{
#' \item{input}{list of the data used to construct the model (list(H, D))}
#' \item{model}{outputs of the model (same outputs as given by \code{\link{lm}}, \code{\link{nls}})}
#' \item{RSE}{Residual Standard Error of the model}
#' \item{RSElog}{Residual Standard Error of the log model (\code{NULL} if other model)}
#' \item{residuals}{Residuals of the model}
#' \item{coefficients}{Coefficients of the model}
#' \item{R.squared}{\eqn{R^2} of the model}
#' \item{formula}{Formula of the model}
#' \item{method}{Name of the method used to construct the model}
#' \item{predicted}{Predicted height values}
#' }
#' 
#' If the parameter model is null, the function return a graph with all the methods for 
#' comparison, the function also return a data.frame with:
#' \describe{
#' \item{method}{The method that had been used to construct the graph}
#' \item{color}{The color of the curve in the graph}
#' \item{RSE}{Residual Standard Error of the model}
#' \item{RSElog}{Residual Standard Error of the log model (\code{NULL} if other model)}
#' \item{Average_bias}{The average bias for the model}
#' }
#' 
#' 
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY, Arthur PERE
#' @seealso \code{\link{retrieveH}}, \code{\link{predictHeight}}
#' 
#' @export
#'
#' @examples
#' 
#' # Load a data set
#' data(NouraguesHD)
#' 
#' # To model the height from a dataset
#' \dontrun{HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, drawGraph = TRUE)}
#' 
#' # If the method needed is known
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "weibull", drawGraph = TRUE)
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log1", drawGraph = TRUE)
#' 
#' # Using weights
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "weibull", useWeight = TRUE,
#'                    drawGraph = TRUE)
#'                    
#'                    
#' @importFrom graphics legend lines par plot
#' @importFrom stats SSmicmen lm median na.omit quantile rnorm sd
#' @importFrom utils data download.file unzip write.table
#' @importFrom methods is
#' @importFrom data.table data.table

modelHD <- function(D, H, method = NULL, useWeight = FALSE, drawGraph = FALSE)
{   
  
  # parameters verification -------------------------------------------------
  
  # Check if there is enough data to compute an accurate model
  nbNonNA <- sum(!is.na(H))
  if(nbNonNA < 15)
    stop("The data has not enough height data (less than 15 non NA)")
  
  if (length(H) != length(D))
    stop("Your vector D and H don't have the same length")
  
  if ( !is.null(method) && ! (tolower(method) %in% c("log1", "log2", "log3", "weibull", "michaelis")) )
    stop("Chose your method among those ones : log1, log2, log3, weibull, michaelis")
  
  if(!is.logical(useWeight))
    stop("UseWeight argument must be a boolean")
  
  if(!is.logical(drawGraph))
    stop("drawGraph argument must be a boolean")

  
  
  
  # Data processing ---------------------------------------------------------
  
  Hdata <- data.table(H = H, D = D)
  Hdata <- na.omit(Hdata) # Remove NA values
  weight <- NULL
  
  # Vector of diameter used only for visualisation purpose
  D_Plot <- seq(from = Hdata[, floor(min(D))], to = Hdata[, ceiling(max(D))], 0.5)
  
  # If the measures need to be weighted
  if(useWeight == TRUE)
    weight <- Hdata[, D^2 * H] # weight is proportional to tree volume
  
  

  # If we gave the function a method ----------------------------------------
  
  if(!is.null(method))
  {
    RSElog <- NULL
    method = tolower(method)
    
    ################## Log-log model
    if(grepl("log", method))
    {
      # Fit the log model      
      modSelected <- loglogFunction(Hdata, method)
      RSElog <- summary(modSelected)$sigma
      
      # Baskerville correction 1972
      RSElog_2 = 0.5*RSElog^2
      Hpredict = exp( predict(modSelected) + RSElog_2 )
    
    }  
    
    ################## Weibull 3 parameters
    if(method == "weibull")
    {
      modSelected <- weibullFunction(Hdata, weight)
      Hpredict <- predict(modSelected)
    }
    
    ################## Michaelis-Menten function
    if(method == "michaelis")
    {   
      modSelected <- michaelisFunction(Hdata, weight) 
      Hpredict <- predict(modSelected)
    }
    
    if(drawGraph == TRUE)
    { 
      if ( grepl("log", method) ){
        logD = data.frame(logD = log(D_Plot))
        Hpredict_plot = exp( predict(modSelected, newdata = logD) + RSElog_2 )
      } else {
        D_ = data.frame(D = D_Plot)
        Hpredict_plot <- predict(modSelected, newdata = data.frame(D = D_Plot))
      }
        
      par(mar = c(5, 5, 3, 3))
      plot(Hdata$D, Hdata$H, pch = 20, cex = 0.5, col = "grey50", log = "xy", las = 1,
           xlab = "D (cm)", ylab = "H (m)", cex.lab = 1.8, cex.axis = 1.5,
           main = paste("Selected model : ", method), cex.main = 2) 
      lines(D_Plot, Hpredict_plot, lwd = 2, col = "blue")
      legend('bottomright', c("Data", "Model selected"), lty = c(3,1), lwd = c(3,3), 
             col = c("grey","blue"), cex = 1.5)
    }    
  } 
  else
  {
    # Compare Models ----------------------------------------------------------
    
    logD_plot = data.frame(logD = log(D_Plot))
    D_plot_ = data.frame(D = D_Plot)
    Plot = data.frame(D = D_Plot, log1 = NA, log2 = NA, log3 = NA, weibull = NA, michaelis = NA)
    
    # Let's compare all the models and plot all the graphs !
    mod_log1 <- loglogFunction(Hdata, method = "log1")
    RSElog = summary(mod_log1)$sigma
    Plot$log1 <- exp( predict(mod_log1, newdata = logD_plot) + 0.5*RSElog^2 )
    
    mod_log2 <- loglogFunction(Hdata, method = "log2")
    RSElog = summary(mod_log2)$sigma
    Plot$log2 <- exp( predict(mod_log2, newdata = logD_plot) + 0.5*RSElog^2 )
    
    mod_log3 <- loglogFunction(Hdata, method = "log3")
    RSElog = summary(mod_log3)$sigma
    Plot$log3 <- exp( predict(mod_log3, newdata = logD_plot) + 0.5*RSElog^2 )
    
    mod_wei <- weibullFunction(Hdata, weight)
    Plot$weibull <- predict(mod_wei, newdata = D_plot_)
    
    mod_mich <- michaelisFunction(Hdata, weight)
    Plot$michaelis <- predict(mod_mich, newdata = D_plot_)
    
    # Plot everything
    par(mar = c(5, 5, 3, 3))
    plot(Hdata$D, Hdata$H, pch = 20, cex = 0.5, col = "grey50", log = "xy", las = 1,
         xlab = "D (cm)", ylab = "H (m)", cex.lab = 1.8, cex.axis = 1.5,
         main = "Model comparison",cex.main = 2)
    color = c("blue", "green", "red", "orange", "purple")
    for (i in 2:ncol(Plot))
      lines(Plot[,1], Plot[,i], lwd = 2, col = color[i-1])

    legend('bottomright', c("Log 1", "Log 2", "Log 3", "Weibull", "Michaelis"),
           lty = c(1, 1, 1, 1, 1), lwd = c(2, 2, 2, 2, 2), cex = 1.5,
           col = c("blue", "green", "red", "orange", "purple"))
    
    
    result = data.table(method = c("log1", "log2", "log3", "weibull", "michaelis"),
                       color = c("blue", "green", "red", "orange", "purple"),
                       RSE = as.numeric(NA),
                       RSElog = as.numeric(NA),
                       Average_bias = as.numeric(NA))
    
    printFitData <- function(H, mod, model)
    {
      if(grepl("log",model)){
        RSElog = summary(mod)$sigma
        Hpredict <- exp( predict(mod) + 0.5*RSElog^2 )
      }
      else{
        RSElog = as.numeric(NA)
        Hpredict = predict(mod)
      }
      
      res <- H - Hpredict # residuals
      
      result = list()
      result$RSE = sqrt(sum(res^2)/summary(mod)$df[2]) # Residual standard error
      result$Average_bias = (mean(Hpredict) - mean(H))/mean(H)
      result$RSElog = RSElog
      
      return(result)
    }
    
    mod_ = list(mod_log1, mod_log2, mod_log3, mod_wei, mod_mich)
    result[, c("RSE", "Average_bias", "RSElog") := printFitData(Hdata$H, mod_[[.GRP]], method), by= method]

    message("\n If you want to use a particular model, use the parameter 'method' in this function.")
    result = setDF(result)
    return(result)
  }
  
  ################## Return the model chosen
  
  # Results (RSE, model coefficient, residuals, R?)
  
  names(Hpredict) = NULL
  
  Residuals = Hdata$H - Hpredict
  RSE <- sqrt(sum(Residuals^2, na.rm = TRUE)/summary(modSelected)$df[2])
  
  output <- list(
    input = list(H = Hdata$H, D = Hdata$D),
    model = modSelected,
    residuals = Residuals,
    coefficients = summary(modSelected)$coefficients,
    R.squared = summary(modSelected)$r.squared,
    formula = summary(modSelected)$call,
    method = method,
    predicted = Hpredict,
    RSE = RSE)
  
  if(grepl("log", method))
    output$RSElog = RSElog
  
  return(output)
}
