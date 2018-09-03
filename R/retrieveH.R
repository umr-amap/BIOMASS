retrieveH <- function(D, model = NULL, coord = NULL, region = NULL)
{  
  if(is.null(model) & is.null(region) & is.null(coord))
    stop("Either model, region, coord should be given")
  
  # First case : with a model fitted with HDfunction
  if(!is.null(model))
  {
    H <- predictHeight(D, model)
    RSE <- model$RSE
  }
  else
  {
    # Second case : with the coordinates of your site, find the E index and estimate the H following Chave et al. 2014 Global Change Biology
    if(!is.null(coord))
    {
      if(is.null(dim(coord))) 
        coord <- as.matrix(t(coord))
      
      if(nrow(coord) == 1)
        coord <- cbind(rep(coord[1], length(D)), rep(coord[2], length(D)))
      
      if(nrow(coord) != length(D))
        stop("coord should be either 
- a vector (e.g. c(longitude, latitude)) or 
- a matrix/dataframe with two columns (longitude and latitude) 
             having the same number of rows as the number of trees (length(D))")
      
      logD <- log(D)
      E <- computeE(coord) # E = environmental index in Chave et al. 2014
      
      # eq 6a Chave et al. 2014
      logH <- 0.893 - E + 0.760 * logD - 0.0340 * I(logD^2)
      RSE <- 0.243
      H <- exp(logH + 0.5*RSE^2)
    }
    else
    {
      # Third case : with the region, use the weibull parameters from Feldpaush et al. 2012 Biogeosciences
      if(length(region) != 1 & length(region) != length(D)) 
          stop("The number of region does not match the number of trees") 

      feldCoef <- NULL
	  data(feldCoef, envir = environment())
      a <- feldCoef[region, "a"]
      b <- feldCoef[region, "b"]
      c <- feldCoef[region, "c"]
      RSE <- feldCoef[region, "RSE"]
      # eq 5 in Feldpaush et al. 2012
      H <- a * (1 - exp(-b * D^c))
    }
  }
  output <- list(H = H, RSE = RSE)  
  return(output)
}
