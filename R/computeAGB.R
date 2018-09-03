computeAGB <- function(D, WD, H = NULL, coord = NULL, Dlim = NULL)
{  
  ### Compute the Above Ground Biomass in Mg/ha
  if(length(D) != length(WD))
    stop("D and WD have different lenghts")
  
  # If there is height data for all the trees
  if(!is.null(H))
  {
    if(length(D) != length(H))
      stop("H and WD have different length")
    
    if(any(is.na(H)) & !any(is.na(D)))
      warning("NA values are generated for AGB values because of missing information on tree height, 
you may construct a height-diameter model to overcome that issue (see ?HDFunction and ?retrieveH)")
    if(any(is.na(D)))
      warning("NA values in D")
    
    AGB <- (0.0673 * (WD * H * D^2)^0.976) / 1000 # Eq 4 from Chave et al. 2014 Global change biology
  } 
  else
  {
    # If there is no heigth, but the coordinates : 
    if(!is.null(coord))
    {
      if(is.null(dim(coord))) 
        coord <- as.matrix(t(coord))
      if(nrow(coord) == 1)
        coord <- cbind(rep(coord[1], length(D)), rep(coord[2], length(D)))
      if(nrow(coord) != length(D))
        stop("coord should be either
             - a vector (e.g. c(longitude, latitude))
             - a matrix with two columns (longitude and latitude) 
             having the same number of rows as the number of trees (length(D))")
      
      E <- computeE(coord) # environmental index in Chave et al. 2014
      AGB <- exp(-2.023977- 0.89563505*E + 0.92023559*log(WD) + 2.79495823*log(D) - 0.04606298*(log(D)^2)) / 1000 # Modified Eq 7 from Chave et al. 2014 Global change biology
    }
    else
      stop("You need to provide either H or coord")
  }
  if(!is.null(Dlim)) AGB[D<Dlim] <- 0 
  return(AGB)
}
