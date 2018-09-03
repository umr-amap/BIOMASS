\name{BIOMASS-package}
\alias{BIOMASS-package}
\alias{BIOMASS}
\docType{package}
\title{
\packageTitle{BIOMASS}
}
\description{
\packageDescription{BIOMASS}
}
\author{
\packageAuthor{BIOMASS}

Maintainer: \packageMaintainer{BIOMASS}
}
\references{
Rejou-Mechain M., Tanguy A., Piponiot C., Chave J., Herault B. (2016). BIOMASS : An R Package for estimating above-ground
biomass and its uncertainty in tropical forests. R package version 1.0.
}
\keyword{ package }

\examples{
\dontrun{
library(BIOMASS)

data(KarnatakaForest)
str(KarnatakaForest)

# Dataset containing height and diameter measurements from two 1-ha plots
# established in the lowland rainforest of French Guiana, at the Nouragues 
# Ecological Research Station
data(NouraguesHD)
str(NouraguesHD)

#############################################################################
# WOOD DENSITY

# 1-RETRIEVE AND CORRECT TAXONOMY

# Checking typos in taxonomy
Taxo <- correctTaxo(genus = KarnatakaForest$genus, species = KarnatakaForest$species)
KarnatakaForest$genusCorr <- Taxo$genusCorrected
KarnatakaForest$speciesCorr <- Taxo$speciesCorrected

# Retrieving APG III Families and Orders from Genus names
APG <- getTaxonomy(KarnatakaForest$genusCorr, findOrder = T)
KarnatakaForest$familyAPG <- APG$family
KarnatakaForest$orderAPG <- APG$order

# 2-RETRIEVE WOOD DENSITY
dataWD <- getWoodDensity(genus = KarnatakaForest$genusCorr,
                         species = KarnatakaForest$speciesCorr,
                         stand = KarnatakaForest$plotID)

#############################################################################
# TREE HEIGHT

# Compare different local H-D models
HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, 
                   drawGraph = TRUE, useWeight = TRUE)

# Compute the local H-D model with the lowest RSE
HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H,
                   method = "log2", useWeight = TRUE)

# Compute models specific to given stands
HDmodelPerPlot <- by(NouraguesHD, NouraguesHD$plotId,
                     function(x) modelHD(D = x$D, H = x$H, method = "weibull", useWeight = T),
                     simplify = FALSE)                     
RSEmodels <- sapply(HDmodelPerPlot, function(x) x$RSE)
Coeffmodels <- lapply(HDmodelPerPlot, function(x) x$coefficients)

# Retrieve height data from a local HD model
dataHlocal <- retrieveH(D = KarnatakaForest$D, model = HDmodel)

# Retrieve height data from a Feldpaush et al. (2012) averaged model
dataHfeld <- retrieveH(D = KarnatakaForest$D, region = "SEAsia")

# Retrieve height data from Chave et al. (2012) equation 6
dataHchave <- retrieveH(D = KarnatakaForest$D, 
                        coord = cbind(KarnatakaForest$long, KarnatakaForest$lat))

#############################################################################
# AGB CALCULATION

KarnatakaForest$WD = dataWD$meanWD
KarnatakaForest$H = dataHlocal$H
KarnatakaForest$Hfeld = dataHfeld$H

# Compute AGB(Mg) per tree
AGBtree <- computeAGB(D = KarnatakaForest$D, WD = KarnatakaForest$WD, 
                      H = KarnatakaForest$H)

# Compute AGB(Mg) per plot
AGBPlotList <- by(KarnatakaForest, KarnatakaForest$plotId,
                  function(x) computeAGB(D = x$D, WD = x$WD, H = x$H),
                  simplify = F)
AGBplot <- sapply(AGBPlotList, sum) 

# Compute AGB(Mg) per tree without height information (Eq. 7 from Chave et al. (2014))
AGBPlotListChave <- by(KarnatakaForest, KarnatakaForest$plotId,
                       function(x) computeAGB(D = x$D, WD = x$WD, coord = cbind(x$long, x$lat)),
                       simplify = F)
AGBplotChave <- sapply(AGBPlotListChave, sum) 

# Compute AGB(Mg) per tree with Feldpausch et al. (2012) regional H-D model
AGBPlotListFeld <- by(KarnatakaForest, KarnatakaForest$plotId,
                      function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                      simplify = F)
AGBplotFeld <- sapply(AGBPlotListFeld, sum) 

#############################################################################
# PROPAGATING ERRORS

KarnatakaForest$sdWD = dataWD$sdWD
KarnatakaForest$HfeldRSE = dataHfeld$RSE

# Per plot using the local HD model constructed above (modelHD)
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x) AGBmonteCarlo(D = x$D, WD = x$WD, H = x$H, errWD = x$sdWD,
                                         HDmodel = HDmodel, Dpropag = "chave2004"),
               simplify = F)
meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
credperplot <- sapply(resultMC, "[", 4)

# Per plot using the Feldpaush regional HD averaged model
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x) AGBmonteCarlo(D = x$D, WD = x$WD, errWD = x$sdWD, H = x$Hfeld,
                                         errH = x$HfeldRSE, Dpropag = "chave2004"),
               simplify = F)
meanAGBperplotFeld <- unlist(sapply(resultMC, "[", 1))
credperplotFeld <- sapply(resultMC, "[", 4)

# Per plot using Chave et al. (2014) Equation 7
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x)AGBmonteCarlo(D = x$D, WD = x$WD, errWD = x$sdWD,
                                        coord = cbind(x$long, x$lat),
                                        Dpropag = "chave2004"),
               simplify = F)
meanAGBperplotChave <- unlist(sapply(resultMC, "[", 1))
credperplotChave <- sapply(resultMC, "[", 4)
}
}