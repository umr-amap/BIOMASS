rm(list=ls())
library(BIOMASS)
library(profvis)
library(microbenchmark)

data("KarnatakaForest")
data(NouraguesHD)

selecPlot<-KarnatakaForest$plotId%in%c("BSP2","BSP12","BSP14","BSP26","BSP28","BSP30","BSP34","BSP44","BSP63","BSP65")
KarnatakaForestsub<-droplevels(KarnatakaForest[selecPlot,])


Taxo<-correctTaxo(genus=KarnatakaForest$genus,species=KarnatakaForest$species)
KarnatakaForest$genusCorr<-Taxo$genusCorrected
KarnatakaForest$speciesCorr<-Taxo$speciesCorrected

APG<-getTaxonomy(KarnatakaForest$genusCorr, findOrder =T)
KarnatakaForest$familyAPG<-APG$family
KarnatakaForest$orderAPG<-APG$order

LocalWoodDensity<-data.frame(genus=c("Ziziphus","Terminalia","Garcinia"),
                             species=c("oenopolia","bellirica","indica"),
                             wd=c(0.65,0.72,0.65))

dataWD<-getWoodDensity(genus=KarnatakaForest$genusCorr,
                       species=KarnatakaForest$speciesCorr,
                       family=KarnatakaForest$familyAPG,
                       stand=KarnatakaForest$plotID,
                       addWoodDensityData=LocalWoodDensity)

HDmodel<-modelHD(D=NouraguesHD$D,
                 H=NouraguesHD$H,
                 method="log2",
                 useWeight =TRUE)


KarnatakaForest$sdWD=dataWD$sdWD
KarnatakaForest$WD=dataWD$meanWD



#coord = cbind(KarnatakaForest$long, KarnatakaForest$lat)







AGBmonteCarlo1 = function(D, WD = NULL, errWD = NULL, H = NULL, errH = NULL, HDmodel = NULL, 
                          coord = NULL, Dpropag = NULL, n = 1000, Carbon = FALSE, Dlim = NULL){
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
      fivePercent <- round( length(D) * 5 / 100 )
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
      if(!is.numeric(Dpropag) | !length(Dpropag)%in%c(1,length(D))) 
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
    intercept <- param_7[selec, "intercept"] # vector of simulated intercept
    coeffWD <- param_7[selec, "logwsg"] # vector of simulated parameter associated to ln(WD)
    coefflnD <- param_7[selec, "logdbh"] # vector of simulated parameter associated to ln(D) 
    coefflnD2 <- param_7[selec, "logdbh2"] # vector of simulated parameter associated to ln(D)^2
    coeffE <- -param_7[selec, "E"] # vector of simulated parameter associated to E
    coeffTmp <- param_7[selec, "temp"] # vector of of simulated parameter associated to tempsea coeff
    coeffCWD <- param_7[selec, "cwd"] # vector of of simulated parameter associated to CWD coeff
    coeffPS <- param_7[selec, "prec"] # vector of of simulated parameter associated to precSeas coeff
    RSE <- param_7[selec,"sd"] # vector of simulated RSE values
    
    # Recalculating n E values based on posterior parameters associated with the bioclimatic variables
    Tmp <- replicate(n, bioclimParams$tempSeas)
    CWD <- replicate(n, bioclimParams$CWD)
    PS <- replicate(n, bioclimParams$precSeas)
    
    Esim <- sweep(Tmp, MARGIN = 2, coeffTmp, "*") + sweep(CWD, MARGIN = 2, coeffCWD, "*") +
      sweep(PS, MARGIN = 2, coeffPS, "*")
    
    # Applying AGB formula over simulated matrices and vectors
    AGB_simu <- sweep(sweep(log(WD_simu), MARGIN = 2, coeffWD, "*") + 
                        sweep(log(D_simu), MARGIN = 2, coefflnD, "*") + 
                        sweep(log(D_simu)^2, MARGIN = 2, coefflnD2, "*")+
                        sweep(Esim, MARGIN = 2, coeffE, "*"),
                      MARGIN = 2, intercept, '+')
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






res = microbenchmark("origin"=AGBmonteCarlo(D=KarnatakaForest$D,WD=KarnatakaForest$WD,
                                            errWD = KarnatakaForest$sdWD,HDmodel=HDmodel,Dpropag ="chave2004"),
                     "modified"=AGBmonteCarlo1(D=KarnatakaForest$D,WD=KarnatakaForest$WD,
                                               errWD = KarnatakaForest$sdWD,HDmodel=HDmodel,Dpropag ="chave2004"), times = 10)





######## time and memory in function of dataset size

n = 60000
time = matrix(nrow=length(seq(10, n, by = 100)), ncol = 2, dimnames = list(NULL, c("original", "modified")))
k = 1
b = seq(10, n, by = 100)
for( i in b){
  gc(reset = T)
  time[k,1] = system.time(AGBmonteCarlo(D=KarnatakaForest$D[1:i],WD=KarnatakaForest$WD[1:i],
                         errWD = KarnatakaForest$sdWD[1:i],HDmodel=HDmodel,Dpropag ="chave2004"))[1]
  gc(reset = T)
  time[k,2] = system.time(AGBmonteCarlo1(D=KarnatakaForest$D[1:i],WD=KarnatakaForest$WD[1:i],
                         errWD = KarnatakaForest$sdWD[1:i],HDmodel=HDmodel,Dpropag ="chave2004"))[1]
  k = k + 1
  print(paste(i, k))
}

png(filename = "~/Bureau/document arthur/biomass/test/function/speed_AGB.png")
plot(b, time[,1], type = "l", col = "blue", xlab = "dataset size (lines)", ylab = "user time")
lines(b, time[,2], col = "red")
legend("bottomright", legend = c("original", "modified"), col = c("blue", "red"), lty = 1)
dev.off()

write.csv(cbind("dataset size" = b, time), file = "~/Bureau/document arthur/biomass/test/function/speed_time.csv")

n = 60000
mem = matrix(nrow=length(seq(10, n, by = 100)), ncol = 2, dimnames = list(NULL, c("original", "modified")))
k = 1
b = seq(10, n, by = 100)
for( i in b){
  start = gc(reset = T)
  AGBmonteCarlo(D=KarnatakaForest$D[1:i],WD=KarnatakaForest$WD[1:i],
                errWD = KarnatakaForest$sdWD[1:i],HDmodel=HDmodel,Dpropag ="chave2004")
  end = gc() - start
  mem[k, 1] = colSums(end)[6]

  start = gc(reset = T)
  AGBmonteCarlo1(D=KarnatakaForest$D[1:i],WD=KarnatakaForest$WD[1:i],
                errWD = KarnatakaForest$sdWD[1:i],HDmodel=HDmodel,Dpropag ="chave2004")
  end = gc() - start
  mem[k, 2] = colSums(end)[6]  

  k = k + 1
  print(paste(i, k))
}

png(filename = "~/Bureau/document arthur/biomass/test/function/memory_AGB.png")
plot(b, mem[,1], type = "l", col = "blue", xlab = "dataset size (lines)", ylab = "Memory (Mb)")
lines(b, mem[,2], col = "red")
legend("bottomright", legend = c("original", "modified"), col = c("blue", "red"), lty = 1)
dev.off()

write.csv(cbind("dataset size" = b, mem), file = "~/Bureau/document arthur/biomass/test/function/memory.csv")




#### function time vs dataset size

b2 = b^2
lm_time1 = lm(time[,1] ~ b + b2)
plot(b, time[,1], cex = 0.1, col = "blue", xlab = "dataset size (lines)", ylab = "user time")
lines(b, lm_time1$coefficients[1] + lm_time1$coefficients[2] * b + lm_time1$coefficients[3] * b2, col = "blue")

lm_time2 = lm(time[,2] ~ b)
points(b, time[,2], cex = 0.1, col = "red")
abline(lm_time2, col = "red")
#























########## function coord
coord = function(n, coord, WD_simu, D_simu, selec){
  param_7 <- NULL
  data(param_7, envir = environment()) # posterior parameters from MCMC algorithm
  
  bioclimParams <- getBioclimParam(coord) # get bioclim variables corresponding to the coordinates
  
  # Posterior model parameters 
  intercept <- param_7[selec, "intercept"] # vector of simulated intercept
  coeffWD <- param_7[selec, "logwsg"] # vector of simulated parameter associated to ln(WD)
  coefflnD <- param_7[selec, "logdbh"] # vector of simulated parameter associated to ln(D) 
  coefflnD2 <- param_7[selec, "logdbh2"] # vector of simulated parameter associated to ln(D)^2
  coeffE <- -param_7[selec, "E"] # vector of simulated parameter associated to E
  coeffTmp <- param_7[selec, "temp"] # vector of of simulated parameter associated to tempsea coeff
  coeffCWD <- param_7[selec, "cwd"] # vector of of simulated parameter associated to CWD coeff
  coeffPS <- param_7[selec, "prec"] # vector of of simulated parameter associated to precSeas coeff
  RSE <- param_7[selec,"sd"] # vector of simulated RSE values
  
  # Recalculating n E values based on posterior parameters associated with the bioclimatic variables
  Tmp <- replicate(n, bioclimParams$tempSeas)
  CWD <- replicate(n, bioclimParams$CWD)
  PS <- replicate(n, bioclimParams$precSeas)
  
  Esim <- sweep(Tmp, MARGIN = 2, coeffTmp, "*") + sweep(CWD, MARGIN = 2, coeffCWD, "*") +
    sweep(PS, MARGIN = 2, coeffPS, "*")
  
  # Applying AGB formula over simulated matrices and vectors
  AGB_simu <- sweep(sweep(log(WD_simu), MARGIN = 2, coeffWD, "*") + 
                      sweep(log(D_simu), MARGIN = 2, coefflnD, "*") + 
                      sweep(log(D_simu)^2, MARGIN = 2, coefflnD2, "*")+
                      sweep(Esim, MARGIN = 2, coeffE, "*"),
                    MARGIN = 2, intercept, '+')
  return(AGB_simu)
}

coord1 = function(n, coord, WD_simu, D_simu, selec){
  param_7 <- NULL
  data(param_7, envir = environment()) # posterior parameters from MCMC algorithm
    
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
                   t(log(D_simu)^2) * param_7[selec, "logdbh2"] + Esim * -param_7[selec, "E"] + 
                   param_7[selec, "intercept"] )
  return(AGB_simu)
  
}


myrtruncnorm <- function(n,lower = -1, upper = 1,mean=0,sd=1) {
  qnorm(runif(n,pnorm(lower,mean=mean,sd=sd),pnorm(upper,mean=mean,sd=sd)),mean=mean,sd=sd)
}


n = 1000
a = 10000
coord12 = cbind( KarnatakaForest$long[1:a], KarnatakaForest$lat[1:a] )
D_simu = replicate(n, myrtruncnorm(a, mean= D[1:a], lower = 0.1, upper = 500))
WD_simu = replicate(n, myrtruncnorm(a, mean = WD[1:a], lower = 0.08, upper = 1.39))
selec <- sample(1:1001, n) 

all( coord(n, coord12, WD_simu, D_simu, selec) == coord1(n, coord12, WD_simu, D_simu, selec) , na.rm = T)







#### Try of a new function coord

coord2 = function(n, coord, WD_simu, D_simu, selec){
  param_7 <- NULL
  data(param_7, envir = environment()) # posterior parameters from MCMC algorithm
  
  bioclimParams <- getBioclimParam(coord) # get bioclim variables corresponding to the coordinates
  
  simulation = function(x){
    Esim = bioclimParams$tempSeas * param_7[selec[x], "temp"] + 
      bioclimParams$CWD * param_7[selec[x], "cwd"] + 
      bioclimParams$precSeas * param_7[selec[x], "prec"]
    AGB_simu = log(WD_simu[,x]) * param_7[selec[x], "logwsg"] +
      log(D_simu[,x]) * param_7[selec[x], "logdbh"] +
      log(D_simu[,x])^2 * param_7[selec[x], "logdbh2"] +
      Esim * -param_7[selec[x], "E"] +
      param_7[selec[x], "intercept"]
    return(AGB_simu)
  }
  
  RSE <- param_7[selec,"sd"] # vector of simulated RSE values
  

  return(sapply(1:n, simulation))
  
}
all( coord(n, coord12, WD_simu, D_simu, selec) == coord2(n, coord12, WD_simu, D_simu, selec))


res = microbenchmark("original"= coord(n, coord12, WD_simu, D_simu, selec),
                     "modified1" = coord1(n, coord12, WD_simu, D_simu, selec),
                     "modified2" = coord1(n, coord12, WD_simu, D_simu, selec))



##### function coordonates spped and memory

n = 1000
coord12 = cbind( KarnatakaForest$long, KarnatakaForest$lat )
D_simu = replicate(n, myrtruncnorm(length(D), mean= D, lower = 0.1, upper = 500))
WD_simu = replicate(n, myrtruncnorm(length(WD), mean = WD, lower = 0.08, upper = 1.39))
selec <- sample(1:1001, n)


n = 61000
time = matrix(nrow=length(seq(10, n, by = 100)), ncol = 3, dimnames = list(NULL, c("original", "modified1", "modified2")))
k = 1
size = seq(10, n, by = 100)
for( i in size){
  gc(reset = T)
  time[k,1] = system.time(coord(1000, coord12[1:i, ], WD_simu[1:i, ], D_simu[1:i, ], selec))[1]
  gc(reset = T)
  time[k,2] = system.time(coord1(1000, coord12[1:i, ], WD_simu[1:i, ], D_simu[1:i, ], selec))[1]
  gc(reset = T)
  time[k,3] = system.time(coord2(1000, coord12[1:i, ], WD_simu[1:i, ], D_simu[1:i, ], selec))[1]
  
  k = k + 1
  print(paste(i, k))
}

png(filename = "~/Bureau/document arthur/biomass/test/function/speed_coord.png")
plot(size, time[,1], col = "blue", xlab = "dataset size (lines)", ylab = "user time", cex = 0.1)
points(size, time[,2], col = "red", cex = 0.1)
points(size, time[,3], col = "green", cex = 0.1)
legend("bottomright", legend = c("original", "modified1", "modified2"), col = c("blue", "red", "green"), lty = 1)
dev.off()

write.csv(cbind("dataset size" = size, time), file = "~/Bureau/document arthur/biomass/test/function/speed_coord.csv", row.names = F)

n = 61000
mem = matrix(nrow=length(seq(10, n, by = 100)), ncol = 3, dimnames = list(NULL, c("original", "modified1", "modified2")))
k = 1
size = seq(10, n, by = 100)
for( i in size ){
  start = gc(reset = T)
  coord(1000, coord12[1:i, ], WD_simu[1:i, ], D_simu[1:i, ], selec)
  end = gc() - start
  mem[k, 1] = colSums(end)[6]
  
  start = gc(reset = T)
  coord1(1000, coord12[1:i, ], WD_simu[1:i, ], D_simu[1:i, ], selec)
  end = gc() - start
  mem[k, 2] = colSums(end)[6]
  
  start = gc(reset = T)
  coord3(1000, coord12[1:i, ], WD_simu[1:i, ], D_simu[1:i, ], selec)
  end = gc() - start
  mem[k, 3] = colSums(end)[6]
  
  k = k + 1
  print(paste(i, k))
}

png(filename = "~/Bureau/document arthur/biomass/test/function/memory_coord.png")
plot(size, mem[,1], col = "blue", xlab = "dataset size (lines)", ylab = "Memory (Mb)", cex = 0.1)
points(size, mem[,2], col = "red", cex = 0.1)
points(size, mem[,3], col = "green", cex = 0.1)
legend("bottomright", legend = c("original", "modified1", "modified2"), col = c("blue", "red", "green"), lty = 1)
dev.off()

write.csv(cbind("dataset size" = size, mem), file = "~/Bureau/document arthur/biomass/test/function/memory_coord.csv", row.names = F)



