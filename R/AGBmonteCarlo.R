#' Propagating above ground biomass (AGB) or carbon (AGC) errors to the stand level
#'
#' @description Propagation of the errors throughout the steps needed to compute AGB or AGC, supporting user AGB allometry.
#'
#' @param D Vector of tree diameters (in cm)
#' @param WD Vector of wood density estimates (in g/cm3)
#' @param errWD Vector of error associated to the wood density estimates (should be of the same size as `WD`)
#' @param H (option 1) Vector of tree heights (in m). If set, `errH` must be set too.
#' @param errH (if `H`) Residual standard error (RSE) of a model or vector of errors (sd values) associated to tree height
#' values (in the latter case the vector should be of the same length as `H`).
#' @param HDmodel (option 2) Model used to estimate tree height from tree diameter (output from [modelHD()], see example).
#' @param coord (option 3, only used with `"chave2014"` allometry) Coordinates of the site(s), either a vector giving a single site (e.g. c(longitude, latitude))
#' or a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)). The coordinates are used to predict
#' height-diameter allometry with bioclimatic variables.
#' @param Dpropag This variable can take three kind of values, indicating how to propagate the errors on diameter measurements:
#' a single numerical value or a vector of the same size as `D`, both representing the standard deviation associated
#' with the diameter measurements or `"chave2004"` (an important error on 5 percent of the measures, a smaller error on
#' 95 percent of the trees).
#' @param n Number of iterations. Cannot be smaller than 50 or larger than 1000. By default `n = 1000`
#' @param Carbon (logical) Whether or not the propagation should be done up to the carbon value (FALSE by default).
#' @param Dlim (optional) Minimum diameter (in cm) for which above ground biomass should be calculated (all diameter below
#' `Dlim` will have a 0 value in the output).
#' @param fitted_allom Can be three types of values, indicating the AGB allometry that will be used to compute tree AGB: 
#' `"chave2014"` (pantropical Chave's allometry, default value), or a brms.fit object of a fitted allometry, or a list of 3 objects 
#' (a formula, a data.frame of fitted parameters values, a character value specifying if the response is log scaled). 
#' See Details below for supported user allometries.
#' @param var_in_data (Needed when `"fitted_allom"` is a brms.fit object) A vector of characters among : 
#' `"AGB"`, `"logAGB"`, `"V"`, `"logV"`, `"H"`, `"logH"`, `"D"`, `"logD"`, `"WD"` standing for variables (including response & predictors) 
#' used in fitting procedure, corresponding to `"fitted_allom$data"`, variables order MUST match variables order of `"colnames(fitted_allom$data)"`.
#' @param volume_allom (logical) Whether the response of the allometry model is AGB or volume (V). By default `volume_allom = FALSE`
#' @param return_volume (logical) Whether the returned output should be AGB or volume (V). By default `return_volume = FALSE`
#'
#' @details See Rejou-Mechain et al. (2017) for all details on the error propagation procedure.
#' @details Supported types of allometry, and how to properly specify the allometry:
#'  - brms.fit object: response must be (log)AGB or (log)V (volume); predictors must be among (log)height H (logH), (log)diameter D (logD), wood density WD; 
#'   group level effect not supported;
#'  - formula & parameters: list(formula = NULL, parameters = NULL, link = NULL)
#'   formula = ~ param1 * H ..., can be any function of variables H, D, WD; parameters = data.frame(param1 = , param2 = , ..., sigma =); link = NULL or "log";
#'   variables in the formula must be among `"H"`, `"D"`, `"WD"`; column names of parameters must match parameters names in formula;
#'   a Gaussian error model is assumed on mean response, with `"sigma"` as standard deviation.
#'
#' @return Returns a list  with (if Carbon is FALSE):
#'   - `meanAGB`: Mean stand AGB value following the error propagation
#'   - `medAGB`: Median stand AGB value following the error propagation
#'   - `sdAGB`: Standard deviation of the stand AGB value following the error propagation
#'   - `credibilityAGB`: Credibility interval at 95\% of the stand AGB value following the error propagation
#'   - `AGB_simu`: Matrix with the AGB of the trees (rows) times the n iterations (columns)
#'   - `AGB_simu_per_stem`: Dataframe with mean, median, sd, 2.5 and 97.5\% quantiles of AGB value 
#'   following the error propagation at individual tree level.
#'
#' @author Dominique LAMONICA, Maxime REJOU-MECHAIN, Bruno HERAULT, Camille PIPONIOT, Ariane TANGUY, Arthur PERE
#'
#' @examples
#' # Load a database
#' data(NouraguesHD)
#' data(NouraguesTrees)
#' 
#' # Modelling height-diameter relationship
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2", bayesian = FALSE)
#' 
#' # Retrieving wood density values
#' \donttest{
#' NouraguesWD <- getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species,
#'   stand = NouraguesTrees$Plot
#' )
#' }
#' 
#' # Propagating errors with a standard error for Wood density
#' \donttest{
#' resultMC <- AGBmonteCarlo(
#'   D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
#'   errWD = NouraguesWD$sdWD, HDmodel = HDmodel
#' )
#' }
#' 
#' # If only the coordinates are available
#' coord <- c(-52.683213,4.083024 )
#' \donttest{
#' resultMC <- AGBmonteCarlo(
#'   D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
#'   errWD = NouraguesWD$sdWD, coord = coord
#' )
#' }
#' 
#' # Propagating errors with a standard error in wood density in all plots at once
#' \donttest{
#' NouraguesTrees$meanWD <- NouraguesWD$meanWD
#' NouraguesTrees$sdWD <- NouraguesWD$sdWD
#' resultMC <- by(
#'   NouraguesTrees, NouraguesTrees$Plot,
#'   function(x) AGBmonteCarlo(
#'       D = x$D, WD = x$meanWD, errWD = x$sdWD,
#'       HDmodel = HDmodel, Dpropag = "chave2004"
#'     )
#' )
#' meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
#' credperplot <- sapply(resultMC, "[", 4)
#' }
#' closeAllConnections()
#' 
#' @keywords Monte Carlo
#' @importFrom stats pnorm qnorm runif
#' @export

AGBmonteCarlo <- function(D, WD = NULL, errWD = NULL, H = NULL, errH = NULL,
                          HDmodel = NULL, coord = NULL, Dpropag = NULL, n = 1000,
                          Carbon = FALSE, Dlim = NULL,
                          fitted_allom = "chave2014",
                          var_in_data = NULL,
                          volume_allom = FALSE, return_volume = FALSE
                         ) {
  
  
  if(all(fitted_allom == "chave2014")){
    
    # set volume_allom & return_volume to FALSE when chave's allometry is used
    volume_allom <- FALSE
    return_volume <- FALSE
    
    #call to AGBmonteCarlo_chave2014
    AGB_sim <- AGBmonteCarlo_chave2014(
    WD = WD, errWD = errWD, H = H, errH = errH, HDmodel = HDmodel, coord = coord, 
    D = D, Dpropag = Dpropag, n =n, Dlim = Dlim)
    
  }else{
    
    if (volume_allom == FALSE & return_volume == T){
      stop("You need to provide a volume allometry to get volume estimates")
    }
    
    # parameters verification -------------------------------------------------
    ## checks that are shared by formula or fit object
    len <- length(D)
 
    if (n > 1000 | n < 50) {
      stop("n cannot be smaller than 50 or larger than 1000")
    }
    
    if (!is.null(Dpropag)) {
      if ((is.numeric(Dpropag) && !(length(Dpropag) %in% c(1, len)) || (!is.numeric(Dpropag) && tolower(Dpropag) != "chave2004"))) {
        stop('Dpropag should be set to one of these options:
             - "chave2004"
             - a single sd value that will be applied to all trees
             - a vector of sd values of the same length as D')
      }
    }
    
    if (is.null(WD) & volume_allom == TRUE) {
      stop("When the model predicts volume, WD is needed")
    }
    
    if (!is.null(WD)){
    if (len != length(WD) || len != length(errWD)) {
      stop("One of vector WD or errWD does not have the same length as D")
    }
    }
    

    if(class(fitted_allom) == "brmsfit"){

        # id response variable in brmsfit
      var_names_allom <- colnames(fitted_allom$data)
      resp_name_allom <- all.vars(fitted_allom$formula$formula)[1]
      
     if(length(var_names_allom) != var_in_data){
       stop("The length of variable names do not match the number of columns found in brms.fit object data")
     }
      
     if(var_in_data[which(var_names_allom == resp_name_allom)] %in% c("AGB", "logAGB", "V", "logV") == FALSE){
       stop("Response/predicted variable is not among AGB, logAGB, V, logV")
     }
      
      predictor_names <- var_in_data[which(var_names_allom != resp_name_allom)]
  
      # check that data for prediction are in args
    if(all(predictor_names %in% c("D", "H", "WD", "logD", "logH")) == FALSE){
      stop("Predictors are not among D, H, WD, logD, logH")
    }
     
      predictor_names_allom <- var_names_allom[var_names_allom != resp_name_allom]
      names(predictor_names) <- predictor_names_allom
    }
    
    if(length(fitted_allom) == 2){
      
      # check that formula is composed of variables among D, H, WD and parameters from the provided list
      if(all(all.vars(fitted_allom$formula) %in% c("D", "H", "WD", names(fitted_allom$parameters))) == FALSE){
        stop("Parameters in formula not provided, or variable not among D, H, WD.")
      }
      
      # check that there is a residual parameter sigma
      if (!("sigma" %in% names(fitted_allom$parameters))){
        stop("A residual error parameter named sigma should be provided.")
      }
      
      # check for parameter sample size
      if(n<= nrow(fitted_allom$parameters)){
      params <- fitted_allom$parameters[1:n,]
      }else{ #resample to get enough parameter combinations
        params <- rbind(fitted_allom$parameters, 
                        fitted_allom$parameters[sample(1:nrow(fitted_allom$parameters), n - nrow(fitted_allom$parameters), replace = T),]
                        )
      }
      
    }
  
  ### Propagate error with Markov Chain Monte Carlo approach
  
  # function truncated random gausian law -----------------------------------
  myrtruncnorm <- function(n, lower = -1, upper = 1, mean = 0, sd = 1) {
    qnorm(p = runif(n = n, min = pnorm(lower, mean = mean, sd = sd), max = pnorm(upper, mean = mean, sd = sd)), mean = mean, sd = sd)
  }
  
  
  # --------------------- D ---------------------
  
  if (!is.null(Dpropag)) {
    if (length(Dpropag) == 1 && tolower(Dpropag) == "chave2004") {
      # Propagation of the measurement error on D: based on Chave et al. 2004 (p.412) Phil. Trans. R. Soc. Lond. B.
      fivePercent <- round(len * 5 / 100)
      chaveError <- function(x, len) {
        ## Assigning large errors on 5% of the trees
        largeErrSample <- sample(len, fivePercent)
        
        D_sd <- 0.0062 * x + 0.0904 # Assigning small errors on the remaining 95% trees
        D_sd[largeErrSample] <- 4.64
        
        x <- myrtruncnorm(n = len, mean = x, sd = D_sd, lower = 0.1, upper = 500)
        return(x)
      }
      D_simu <- suppressWarnings(replicate(n, chaveError(D, len)))
    }
    else {
      D_simu <- suppressWarnings(replicate(n, myrtruncnorm(len, mean = D, sd = Dpropag, lower = 0.1, upper = 500)))
    }
  } else {
    D_simu <- replicate(n, D)
  }
  
  
  # --------------------- WD ---------------------
  
  #### Below 0.08 and 1.39 are the minimum and the Maximum WD value from the global wood density database respectively
 if(!is.null(WD)){
   WD_simu <- suppressWarnings(replicate(n, myrtruncnorm(n = len, mean = WD, sd = errWD, lower = 0.08, upper = 1.39)))
 }else{WD_simu <- NA}

    # --------------------- H ---------------------

    # if there is data for H
  if (!is.null(HDmodel) | !is.null(H)) {
    if (!is.null(HDmodel)) {
      # Propagation of the error thanks to the H-D local model
      
      # If HDmodel is a list of stand-specific hd-models, get back the plotID vector
      if (!identical(names(HDmodel)[1:4] , c("input", "model", "residuals", "method"))) {
        plotID <- do.call(c , lapply(names(HDmodel), function(x) {
          rep(x , length(HDmodel[[x]]$input$D))
        }))
      } else plotID <- NULL
      
      H_simu <- predictHeight(D_simu, HDmodel, err = TRUE, plot = plotID)
      
    } else {
      # Propagation of the error using the errH value(s)
      upper <- max(H, na.rm = TRUE) + 15
      H_simu <- suppressWarnings(replicate(n, myrtruncnorm(len, mean = H, sd = errH, lower = 1.3, upper = upper)))
    }
    
  }else{H_simu <- NA} #H evaluation

  
  
  # --------------------- AGB prediction ---------------------
 
   if (length(fitted_allom) == 2){
  
     compute_pred <- function(formula, data, params)
     {
       call = rlang::f_rhs(formula)
       data = as.list(data)
       envir = c(data, params)
       eval(call, envir = envir)
     }
     
     # names D_simu, H_simu in data vs D, H in the formula
     predicted_response <- compute_pred(formula = fitted_allom$formula, 
                             data = list(D = D_simu, H = H_simu, WD = WD_simu), 
                  params = fitted_allom$parameters)
     #trees in row, replicates columns -> ok same as AGBmC_chave2014
     # residual error
     AGB_simu <- t(apply(MARGIN = 1, X = predicted_response,
                       FUN = rnorm, n = nrow(params), sd = fitted_allom$parameters$sigma
     ))
     
     if (fitted_allom$link == "log"){
       AGB_simu <- exp(AGB_simu)
     }
     
  }
  
  if (class(fitted_allom) == "brmsfit"){
    
    
    newdata_for_pred <- data.table(D = as.vector(t(D_simu)),
                                   H = as.vector(t(H_simu)), 
                                   WD = as.vector(t(WD_simu)),
                                   logD = log(as.vector(t(D_simu))),
                                   logH = log(as.vector(t(H_simu)))
                                   )
    # ordered as replicates nested in trees
   
    # replace colnames to match brms.fit data names
    colnames(newdata_for_pred) <- names(predictor_names)[match(colnames(newdata_for_pred), predictor_names)]
   
    nb_draws <- 20
    #predict response with newdata, treeID preservation to be checked
     predicted_response_brms <- predict(object = fitted_allom, newdata = newdata_for_pred, summary = F, ndraws = nb_draws)
     # nb_draws rows, columns are len times (tree with n replicates)
     
     # randomly select a row for each tree x replicate
     row_index_to_keep <- sample(x = 1:nb_draws, size = nrow(newdata_for_pred), replace = TRUE)
     AGB_simu <- matrix(predicted_response_brms[cbind(row_index_to_keep, 1:ncol(predicted_response_brms))],
                                  ncol = n, nrow = len, byrow = TRUE)
     
    #3) if fitted_allom$formula$family$link == "log" back transform predicted
    
     if(fitted_allom$formula$family$link == "log"|"logAGB" %in% var_in_data|"logV" %in% var_in_data){
       AGB_simu <- exp(AGB_simu)
       }
  }
 
  
  }
  
  
  #----------- AGB_simu management
  
  if (volume_allom == TRUE & return_volume == FALSE){ # lets multiply predicted variable by WD_simu 
    
     AGB_simu <- AGB_simu * WD_simu
  }
  
  
  if (!is.null(Dlim)) AGB_simu[D < Dlim, ] <- 0 # ok keep
  AGB_simu[ which(is.infinite(AGB_simu)) ] <- NA # ok keep
  
  
  if (volume_allom == TRUE & return_volume == TRUE){
 
    sum_V_simu <- colSums(AGB_simu, na.rm = TRUE) #stand level
    
    res <- list(
      meanV = mean(sum_V_simu),
      medV = median(sum_V_simu),
      sdV = sd(sum_V_simu),
      credibilityV = quantile(sum_V_simu, probs = c(0.025, 0.975)),
      
      V_simu = V_simu, # ok keep
      
      V_simu_per_stem = data.frame( # stem level
        meanV = apply(X = V_simu, MARGIN = 1, FUN = mean, na.rm = T),
        medV = apply(X = V_simu, MARGIN = 1, FUN = median, na.rm = T),
        sdV = apply(X = V_simu, MARGIN = 1, FUN = sd, na.rm = T),
        lower_CI_V = apply(X = V_simu, MARGIN = 1, FUN = quantile, probs = 0.025, na.rm = T),
        upper_CI_V = apply(X = V_simu, MARGIN = 1, FUN = quantile, probs = 0.975, na.rm = T)
      )
      
    )
    
   
     }else{
      
  AGB_simu <- AGB_simu / 1000
 

  if (Carbon == FALSE) {
   
    sum_AGB_simu <- colSums(AGB_simu, na.rm = TRUE) #stand level
    
    res <- list(
       meanAGB = mean(sum_AGB_simu),
       medAGB = median(sum_AGB_simu),
       sdAGB = sd(sum_AGB_simu),
       credibilityAGB = quantile(sum_AGB_simu, probs = c(0.025, 0.975)),
       
       AGB_simu = AGB_simu, # ok keep
       
       AGB_simu_per_stem = data.frame( # stem level
         meanAGB = apply(X = AGB_simu, MARGIN = 1, FUN = mean, na.rm = T),
         medAGB = apply(X = AGB_simu, MARGIN = 1, FUN = median, na.rm = T),
         sdAGB = apply(X = AGB_simu, MARGIN = 1, FUN = sd, na.rm = T),
         lower_CI_AGB = apply(X = AGB_simu, MARGIN = 1, FUN = quantile, probs = 0.025, na.rm = T),
         upper_CI_AGB = apply(X = AGB_simu, MARGIN = 1, FUN = quantile, probs = 0.975, na.rm = T)
       )
  
    )
  } else {
    # Biomass to carbon ratio calculated from Thomas and Martin 2012 forests data stored in DRYAD (tropical
    # angiosperm stems carbon content)
    AGC_simu <- AGB_simu * rnorm(mean = 47.13, sd = 2.06, n = n * len) / 100
    
    sum_AGC_simu <- colSums(AGC_simu, na.rm = TRUE)
    res <- list(
      meanAGC = mean(sum_AGC_simu),
      medAGC = median(sum_AGC_simu),
      sdAGC = sd(sum_AGC_simu),
      credibilityAGC = quantile(sum_AGC_simu, probs = c(0.025, 0.975)),
      
      AGC_simu = AGC_simu,
      
      AGC_simu_per_stem = data.frame( # stem level
        meanAGC = apply(X = AGC_simu, MARGIN = 1, FUN = mean, na.rm = T),
        medAGC = apply(X = AGC_simu, MARGIN = 1, FUN = median, na.rm = T),
        sdAGC = apply(X = AGC_simu, MARGIN = 1, FUN = sd, na.rm = T),
        lower_CI_AGC = apply(X = AGC_simu, MARGIN = 1, FUN = quantile, probs = 0.025, na.rm = T),
        upper_CI_AGC = apply(X = AGC_simu, MARGIN = 1, FUN = quantile, probs = 0.975, na.rm = T)
      )
      
    )
  }
  
     }
  return(res)
}
