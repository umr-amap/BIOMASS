AGBmonteCarlo <- function(D, WD = NULL, errWD = NULL, H = NULL, errH = NULL, 
                          HDmodel = NULL, coord = NULL, Dpropag = NULL, n = 1000, Carbon = FALSE, Dlim = NULL)
{  
  if(n > 1000 | n < 50) 
    stop("n cannot be smaller than 50 or larger than 1000")
  
  ### function truncated random gausien law
  myrtruncnorm <- function(n,lower = -1, upper = 1,mean=0,sd=1) {
    qnorm(runif(n,pnorm(lower,mean=mean,sd=sd),pnorm(upper,mean=mean,sd=sd)),mean=mean,sd=sd)
  }
  
  len = length(D)
  
  ### Propagate error with Markov Chain Monte Carlo approach
  
  # --------------------- D ---------------------
  
  if(!is.null(Dpropag))
  {
    if(length(Dpropag) == 1 && Dpropag == "chave2004")
    {
      # Propagation of the measurement error on D: based on Chave et al. 2004 (p.412) Phil. Trans. R. Soc. Lond. B. 
      fivePercent <- round( len * 5 / 100 )
      chaveError <- function(x, len)
      { 
        ## Assigning large errors on 5% of the trees
        largeErrSample <- sample(length(x), fivePercent)
        
        D_sd = 0.0062 * x + 0.0904 # Assigning small errors on the remaining 95% trees
        D_sd[largeErrSample] = 4.64
        
        x <- myrtruncnorm(n = len, mean = x, sd = D_sd, lower = 0.1, upper = 500)
        return(x)
      }
      D_simu = replicate(n, chaveError(D, len))
      
    }
    else
    {
      if(!is.numeric(Dpropag) | !length(Dpropag)%in%c(1,len)) 
        stop("Dpropag should be set to one of these options:
             - \"chave2004\"
             - a single sd value that will be applied to all trees
             - a vector of sd values of the same length as D")
      D_simu = replicate(n, myrtruncnorm(len, mean = D, sd = Dpropag, lower = 0.1, upper = 500))
    }
  }else{ D_simu <- replicate(n, D) }
  
  # --------------------- WD ---------------------
  
  if(!is.null(WD) & !is.null(errWD)){
    
    if(length(errWD) != length(WD))
      stop("Your wood density vector (WD) and the vector of the associated errors (errWD) don't have the same length")
    
    #### Below 0.08 and 1.39 are the minimum and the Maximum WD value from the global wood density database respectively
    len = length(WD)
    WD_simu <- replicate(n, myrtruncnorm(n = len, mean = WD, sd = errWD, lower = 0.08, upper = 1.39))
  }
  else
    stop("The WD and errWD arguments must be not NULL")
  
  
  # --------------------- H ---------------------
  
  if(is.null(HDmodel) & is.null(coord) & is.null(H))
    stop("Input missing, you need to provide one of the following arguments:
             - H
             - HDmodel
             - coord")
  
  # if there is data for H
  if(!is.null(HDmodel) | !is.null(H))
  {    
    if(!is.null(HDmodel))
      # Propagation of the error thanks to the local model of H
      H_simu <- apply(D_simu, 2, function(x) predictHeight(x, model = HDmodel, err = TRUE))
    else
    {
      if(is.null(errH))
        stop("Cannot propagate height errors without information on associated errors (errH is null), if you do not want to propagate H errors please set errH to 0")
      # Propagation of the error using the errH value(s)
      upper = max(H)+15
      len = length(H)
      H_simu <- replicate(n, myrtruncnorm(len, mean = H, sd = errH, lower = 1.3, upper = upper)) 
    }
    
    # --------------------- AGB
    
    param_4 <- NULL
    data(param_4, envir = environment()) # posterior parameters from MCMC algorithm
    selec <- sample(1:nrow(param_4), n)
    RSE <- param_4[selec,"sd"]
    
    # Construct a matrix where each column contains random errors taken from N(0,RSEi) with i varying between 1 and n
    matRSE <- mapply(function(y){rnorm(sd = y, n = length(D))}, y = RSE)
    
    # Posterior model parameters
    Ealpha <- param_4[selec,"intercept"]
    Ebeta <- param_4[selec,"logagbt"]
    
    # Propagation of the error using simulated parameters
    Comp <- t( log(WD_simu * H_simu * D_simu^2) ) * Ebeta + Ealpha
    Comp <- t(Comp) + matRSE
    
    # Backtransformation
    AGB_simu <- exp(Comp)/1000
  }
  
  # --------------------- Coordinates ---------------------
  
  # If there is no data for H, but site coordinates
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
    
    # Equ 7
    # Log(agb) = -1.803 - 0.976 (0.178TS - 0.938CWD - 6.61PS) + 0.976log(WD) + 2.673log(D) -0.0299log(D2)
    param_7 <- NULL
    data(param_7, envir = environment()) # posterior parameters from MCMC algorithm
    selec <- sample(1:nrow(param_7), n)
    
    bioclimParams <- getBioclimParam(coord) # get bioclim variables corresponding to the coordinates
    
    # Posterior model parameters 
    RSE <- param_7[selec,"sd"] # vector of simulated RSE values
    
    # Recalculating n E values based on posterior parameters associated with the bioclimatic variables
    Tmp <- replicate(n, bioclimParams$tempSeas)
    CWD <- replicate(n, bioclimParams$CWD)
    PS <- replicate(n, bioclimParams$precSeas)
    
    Esim <- t(Tmp) * param_7[selec, "temp"] + t(CWD) * param_7[selec, "cwd"] + t(PS) * param_7[selec, "prec"]
    
    # Applying AGB formula over simulated matrices and vectors
    AGB_simu <- t( t(log(WD_simu)) * param_7[selec, "logwsg"] +  
                     t(log(D_simu)) * param_7[selec, "logdbh"] + 
                     t(log(D_simu)^2) * param_7[selec, "logdbh2"] + 
                     Esim * -param_7[selec, "E"] + 
                     param_7[selec, "intercept"] )
    
    # Construct a matrix where each column contains random errors taken from N(0,RSEi) with i varying between 1 and n
    matRSE <- mapply(function(y){rnorm(sd = y, n = length(D))}, y = RSE)
    AGB_simu <- AGB_simu + matRSE
    AGB_simu <- exp(AGB_simu)/1000
  }  
  
  if(!is.null(Dlim)) AGB_simu[D<Dlim,] <- 0  
  
  if(Carbon == FALSE){
    sum_AGB_simu = colSums(AGB_simu, na.rm = T)
    res <- list(meanAGB = mean(sum_AGB_simu), 
                medAGB = median(sum_AGB_simu), 
                sdAGB = sd(sum_AGB_simu), 
                credibilityAGB = quantile(sum_AGB_simu, probs = c(0.025,0.975)), 
                AGB_simu = AGB_simu)
  }else{
    AGC_simu <- AGB_simu*rnorm(mean = 47.13, sd = 2.06,n = n*length(D))/100 # Biomass to carbon ratio calculated from Thomas and Martin 2012 forests data stored in DRYAD (tropical angiosperm stems carbon content)
    sum_AGC_simu = colSums(AGC_simu, na.rm = T)
    res <- list(meanAGC = mean(sum_AGC_simu), 
                medAGC = median(sum_AGC_simu), 
                sdAGC = sd(sum_AGC_simu), 
                credibilityAGC = quantile(sum_AGC_simu, probs = c(0.025,0.975)), 
                AGC_simu = AGC_simu)
  }
  return(res)
}
