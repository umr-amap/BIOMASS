#' Propagating above-ground biomass (AGB) or carbon (AGC) errors to the stand level
#' 
#' Propagation of the errors throughought the steps needed to compute AGB or AGC.
#' 
#' @param D Vector of tree diameters (in cm)
#' @param WD Vector of wood density estimates (in g/cm3)
#' @param errWD Vector of error associated to the wood density estimates (should be of the same size as \code{WD})
#' @param H (option 1) Vector of tree heights (in m). If set, \code{errH} must be set too.
#' @param errH (if \code{H}) Residual standard error (RSE) of a model or vector of errors (sd values) associated to tree height 
#' values (in the latter case the vector should be of the same length as \code{H}).
#' @param HDmodel (option 2) Model used to estimate tree height from tree diameter (output from \code{\link{modelHD}}, see example).
#' @param coord (option 3) Coordinates of the site(s), either a vector giving a single site (e.g. c(longitude, latitude)) 
#' or a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)). The coordinates are used to predict 
#' height-diameter allometry with bioclimatic variables.
#' @param Dpropag This variable can take three kind of values, indicating how to propagate the errors on diameter measurements : 
#' a single numerical value or a vector of the same size as \code{D}, both representing the standard deviation associated 
#' with the diameter measurements or \code{"chave2004"} (an important error on 5 percent of the measures, a smaller error on 
#' 95 percent of the trees).
#' @param n Number of iteration. Cannot be smaller than 50 or larger than 1000. By default \code{n = 1000}
#' @param Carbon (logical) Wether or not the propagation should be done up to the carbon value (FALSE by default).
#' @param Dlim (optional) Minimum diameter (in cm) for which aboveground biomass should be calculated (all diameter below 
#' \code{Dlim} will have a 0 value in the output).
#' 
#' @details See Rejou-Mechain et al. (in prep) for all details on the error propagation procedure.
#' 
#' @return Returns a list  with (if Carbon is FALSE): 
#' \describe{
#' \item{meanAGB}{Mean stand AGB value following the error propagation}
#' \item{medAGB}{Median stand AGB value following the error propagation}
#' \item{sdAGB}{Standard deviation of the stand AGB value following the error propagation}
#' \item{credibilityAGB}{Credibility interval at 95\% of the stand AGB value following the error propagation}
#' \item{AGB_simu}{Matrix with the AGB of the trees (rows) times the n iterations (columns)}
#' }
# 
#' Returns a list  with (if Carbon is TRUE): 
#' \describe{
#'   \item{meanAGC}{Mean stand AGC value following the error propagation}
#' \item{medAGC}{Median stand AGC value following the error propagation}
#' \item{sdAGC}{Standard deviation of the stand AGC value following the error propagation}
#' \item{credibilityAGC}{Credibility interval at 95\% of the stand AGC value following the error propagation}
#' \item{AGC_simu}{Matrix with the AGC of the trees (rows) times the n iterations (columns)}
#' }
#' 
#' @references Chave, J. et al. (2004). \emph{Error propagation and scaling for tropical forest biomass estimates}. 
#' Philosophical Transactions of the Royal Society B: Biological Sciences, 359(1443), 409-420.
#' @references Rejou-Mechain et al. (in prep). \emph{BIOMASS: An R Package for estimating above-ground biomass and its 
#' uncertainty in tropical forests}.
#' 
#' @author Bruno HERAULT, Camille PIPONIOT,  Ariane TANGUY, Maxime REJOU-MECHAIN
#' 
#' @examples 
#' # Load a database
#' data(NouraguesHD)
#' data(KarnatakaForest)
#' 
#' # Modelling height-diameter relationship
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
# 
#' # Retrieving wood density values
#' KarnatakaWD <- getWoodDensity(KarnatakaForest$genus, KarnatakaForest$species, 
#'                               stand = KarnatakaForest$plotId)
#' 
#' # Propagating errors with a standard error in wood density in one plot
#' filt <- KarnatakaForest$plotId=="BSP20"
#' resultMC <- AGBmonteCarlo(D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt], 
#'                           errWD = KarnatakaWD$sdWD[filt], HDmodel = HDmodel)
#' str(resultMC)
#' 
#' # If only the coordinates are available
#' lat <- KarnatakaForest$lat[filt]
#' long <- KarnatakaForest$long[filt]
#' coord <- cbind(long, lat)
#' \dontrun{
#'   resultMC <- AGBmonteCarlo(D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt], 
#'                             errWD = KarnatakaWD$sdWD[filt], coord = coord)
#'   str(resultMC)}
#' 
#' # Propagating errors with a standard error in wood density in all plots at once
#' KarnatakaForest$meanWD = KarnatakaWD$meanWD
#' KarnatakaForest$sdWD = KarnatakaWD$sdWD
#' 
#' \dontrun{
#'   resultMC <- by(KarnatakaForest, KarnatakaForest$plotId, 
#'                  function(x) AGBmonteCarlo(D = x$D, WD = x$meanWD, errWD = x$sdWD, 
#'                                            HDmodel = HDmodel, Dpropag = "chave2004"))
#'   meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
#'   credperplot <- sapply(resultMC, "[", 4)}
#'   
#'@keywords monte carlo
#'@export

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
        largeErrSample <- sample(len, fivePercent)
        
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
  
  if( !is.null(HDmodel) & !is.null(coord) | !is.null(HDmodel) & !is.null(H) | !is.null(coord) & !is.null(H))
    stop("Too many input, choose one input among those arguments:
              - H and Herr
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
      H_simu <- replicate(n, myrtruncnorm(len, mean = H, sd = errH, lower = 1.3, upper = upper)) 
    }
    
    # --------------------- AGB
    
    param_4 <- NULL
    data(param_4, envir = environment()) # posterior parameters from MCMC algorithm
    selec <- sample(1:nrow(param_4), n)
    RSE <- param_4[selec,"sd"]
    
    # Construct a matrix where each column contains random errors taken from N(0,RSEi) with i varying between 1 and n
    matRSE <- mapply(function(y){rnorm(sd = y, n = len)}, y = RSE)
    
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
      coord <- cbind(rep(coord[1], len), rep(coord[2], len))
    if(nrow(coord) != len)
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
    matRSE <- mapply(function(y){rnorm(sd = y, n = len)}, y = RSE)
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
    # Biomass to carbon ratio calculated from Thomas and Martin 2012 forests data stored in DRYAD (tropical angiosperm stems carbon content)
    AGC_simu <- AGB_simu*rnorm(mean = 47.13, sd = 2.06,n = n*len)/100 
    sum_AGC_simu = colSums(AGC_simu, na.rm = T)
    res <- list(meanAGC = mean(sum_AGC_simu), 
                medAGC = median(sum_AGC_simu), 
                sdAGC = sd(sum_AGC_simu), 
                credibilityAGC = quantile(sum_AGC_simu, probs = c(0.025,0.975)), 
                AGC_simu = AGC_simu)
  }
  return(res)
}
