modelHD <- function(D, H, method = NULL, useWeight = FALSE, drawGraph = FALSE)
{   
  ### Model a relationship between the height and the diameter of the trees
  
  # Check if there is enough data to compute an accurate model
  nbNonNA <- sum(!is.na(H))
  if(nbNonNA < 15)
    stop(paste("The data has not enough height data (less than 15 non NA)"))
  
  Hdata <- data.frame(H, D)
  names(Hdata) <- c("H", "D")    
  Hdata <- na.omit(Hdata) # Remove NA values
  weight <- NULL
  
  # Vector of diameter used only for visualisation purpose
  D_Plot <- seq(from = floor(min(Hdata$D)), to = ceiling(max(Hdata$D)), 0.5)
  
  # If the measures need to be weighted
  if(useWeight == TRUE)
    weight <- (Hdata$D^2)*Hdata$H # weight is proportional to tree volume
  
  if(!is.null(method))
  {
    RSElog <- NULL
    
    ################## Log-log model
    if(grepl("log", method))
    {
      # Fit the log model      
      modSelected <- loglogFunction(Hdata, method)
      RSElog <- summary(modSelected)$sigma
      coeff <- summary(modSelected)$coefficients
      
      # Baskerville correction 1972          
      if(method == "log1")
      {
        Hpredict_plot <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(D_Plot))
        Hpredict <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(Hdata$D))
      }
      if(method == "log2")
      {
        Hpredict_plot <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(D_Plot) + coeff[3]*log(D_Plot)^2) 
        Hpredict <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(Hdata$D) + coeff[3]*log(Hdata$D)^2)
      }
      if(method == "log3")
      {
        Hpredict_plot <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(D_Plot) + coeff[3]*log(D_Plot)^2 + coeff[4]*log(D_Plot)^3) 
        Hpredict <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(Hdata$D) + coeff[3]*log(Hdata$D)^2 + coeff[4]*log(Hdata$D)^3) 
      }
    }  
    
    ################## Weibull 3 parameters
    if(method == "weibull")
    {
      modSelected <- weibullFunction(Hdata, weight) 
      coeff <- summary(modSelected)$coefficients
      a <- coeff[1]
      b <- coeff[2]
      c <- coeff[3]
      Hpredict_plot <- a*(1-exp(-(D_Plot/b)^c))
      Hpredict <- a*(1-exp(-(Hdata$D/b)^c))
    }
    
    ################## Michaelis-Menten function
    if(method == "michaelis")
    {   
      modSelected <- michaelisFunction(Hdata, weight) 
      coeff <- summary(modSelected)$coefficients
      A <- coeff[1]
      B <- coeff[2]
      Hpredict_plot <- SSmicmen(D_Plot, A, B)
      Hpredict <- SSmicmen(Hdata$D, A, B)
    }
    
    if(drawGraph == TRUE)
    {      
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
    ################## Compare Models
    
    # Let's compare all the models and plot all the graphs !
    mod_log1 <- loglogFunction(Hdata, method = "log1")
    RSElog <- summary(mod_log1)$sigma
    coeff <- summary(mod_log1)$coefficients
    Hpredict_log1_plot <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(D_Plot)) 
    Hpredict_log1 <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(Hdata$D)) 
    
    mod_log2 <- loglogFunction(Hdata, method = "log2")
    RSElog <- summary(mod_log2)$sigma
    coeff <- summary(mod_log2)$coefficients
    Hpredict_log2_plot <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(D_Plot) + coeff[3]*log(D_Plot)^2) 
    Hpredict_log2 <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(Hdata$D) + coeff[3]*log(Hdata$D)^2) 
    
    mod_log3 <- loglogFunction(Hdata, method = "log3")
    RSElog <- summary(mod_log3)$sigma
    coeff <- summary(mod_log3)$coefficients
    Hpredict_log3_plot <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(D_Plot) + coeff[3]*log(D_Plot)^2 + coeff[4]*log(D_Plot)^3)
    Hpredict_log3 <- exp(coeff[1] + 0.5*RSElog^2 + coeff[2]*log(Hdata$D) + coeff[3]*log(Hdata$D)^2 + coeff[4]*log(Hdata$D)^3)
    
    mod_wei <- weibullFunction(Hdata, weight)
    coeff <- summary(mod_wei)$coefficients
    a <- coeff[1]
    b <- coeff[2]
    c <- coeff[3]
    Hpredict_wei_plot <- a*(1-exp(-(D_Plot/b)^c))
    Hpredict_wei <- a*(1-exp(-(Hdata$D/b)^c))
    
    mod_mich <- michaelisFunction(Hdata, weight)
    coeff <- summary(mod_mich)$coefficients
    A <- coeff[1]
    B <- coeff[2]
    Hpredict_mich_plot <- SSmicmen(D_Plot, A, B)
    Hpredict_mich <- SSmicmen(Hdata$D, A, B)
    
    # Plot everything
    par(mar = c(5, 5, 3, 3))
    plot(Hdata$D, Hdata$H, pch = 20, cex = 0.5, col = "grey50", log = "xy", las = 1,
         xlab = "D (cm)", ylab = "H (m)", cex.lab = 1.8, cex.axis = 1.5,
         main = "Model comparison",cex.main = 2)
    lines(D_Plot, Hpredict_log1_plot, lwd = 2, col = "blue")
    lines(D_Plot, Hpredict_log2_plot, lwd = 2, col = "green")
    lines(D_Plot, Hpredict_log3_plot, lwd = 2, col = "red")   
    lines(D_Plot, Hpredict_wei_plot, lwd = 2, col = "orange")
    lines(D_Plot, Hpredict_mich_plot,lwd = 2, col = "purple")
    
    legend('bottomright', c("Log 1", "Log 2", "Log 3", "Weibull", "Michaelis"),
           lty = c(1, 1, 1, 1, 1), lwd = c(2, 2, 2, 2, 2), cex = 1.5,
           col = c("blue", "green", "red", "orange", "purple"))
    
    cat("Which model would you like to select to model your data ? \n \n")
    
    cat("1 : Log 1 (blue) \n")
    printFitData(Hdata$H, Hpredict_log1, mod_log1)
    
    cat("2 : Log 2 (green) \n")
    printFitData(Hdata$H, Hpredict_log2, mod_log2)
    
    cat("3 : Log 3 (red) \n")
    printFitData(Hdata$H, Hpredict_log3, mod_log3)
    
    cat("4 : Weibull (orange) \n")
    printFitData(Hdata$H, Hpredict_wei, mod_wei)
    
    cat("5 : Michaelis - Menten (purple) \n")
    printFitData(Hdata$H, Hpredict_mich, mod_mich)
    
    ans <- scan(what = integer(), nmax = 1, quiet = TRUE)
    
    modSelected <- switch(ans, 
                          "1" = mod_log1, 
                          "2" = mod_log2, 
                          "3" = mod_log3, 
                          "4" = mod_wei, 
                          "5" = mod_mich) 
    
    Hpredict <- switch(ans, 
                       "1" = Hpredict_log1, 
                       "2" = Hpredict_log2, 
                       "3" = Hpredict_log3, 
                       "4" = Hpredict_wei, 
                       "5" = Hpredict_mich) 
    
    method <- switch(ans, 
                     "1" = "log1", 
                     "2" = "log2", 
                     "3" = "log3", 
                     "4" = "weibull", 
                     "5" = "michaelis") 
  }
  
  ################## Return the model chosen
  
  # Results (RSE, model coefficient, residuals, R?)
  
  coeff <- summary(modSelected)$coefficients
  Residuals <- Hdata$H - Hpredict 
  rSquared <- summary(modSelected)$r.squared
  formula <- summary(modSelected)$call
  RSE <- sqrt(sum(Residuals^2, na.rm = TRUE)/summary(modSelected)$df[2])
  
  output <- list(
    input = list(H = Hdata$H, D = Hdata$D),
    model = modSelected,
    residuals = Residuals,
    coefficients = coeff,
    R.squared = rSquared,
    formula = formula,
    method = method,
    predicted = Hpredict,
    RSE = RSE)
  
  if(grepl("log", method))
    output$RSElog = RSElog
  
  return(output)
}
