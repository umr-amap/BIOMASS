#' @references
#' Réjou-Méchain M., Tanguy A., Piponiot C., Chave J., Hérault B. (2017). BIOMASS :
#' An R Package for estimating above-ground biomass and its uncertainty in tropical forests.
#' Methods in Ecology and Evolution, 8(9), 1163-1167.
#'
#' @author \packageAuthor{BIOMASS}
#' @author Maintainer: \packageMaintainer{BIOMASS}
#'
#'
#' @examples
#' \dontrun{
#' library(BIOMASS)
#' 
#' # Dataset containing plot inventory data from Karnataka, India (Ramesh et al. 2010)
#' data(KarnatakaForest)
#' str(KarnatakaForest)
#' 
#' # Dataset containing height and diameter measurements from two 1-ha plots
#' # established in the lowland rainforest of French Guiana, at the Nouragues
#' # Ecological Research Station
#' data(NouraguesHD)
#' str(NouraguesHD)
#' 
#' #############################################################################
#' # WOOD DENSITY
#' 
#' # 1-RETRIEVE AND CORRECT TAXONOMY
#' 
#' # Checking typos in taxonomy
#' Taxo <- correctTaxo(genus = KarnatakaForest$genus, species = KarnatakaForest$species)
#' KarnatakaForest$genusCorr <- Taxo$genusCorrected
#' KarnatakaForest$speciesCorr <- Taxo$speciesCorrected
#' 
#' # Retrieving APG III Families and Orders from Genus names
#' APG <- getTaxonomy(KarnatakaForest$genusCorr, findOrder = T)
#' KarnatakaForest$familyAPG <- APG$family
#' KarnatakaForest$orderAPG <- APG$order
#' 
#' # 2-RETRIEVE WOOD DENSITY
#' dataWD <- getWoodDensity(
#'   genus = KarnatakaForest$genusCorr,
#'   species = KarnatakaForest$speciesCorr,
#'   stand = KarnatakaForest$plotID
#' )
#' 
#' #############################################################################
#' # TREE HEIGHT
#' 
#' # Compare different local H-D models
#' modelHD(
#'   D = NouraguesHD$D, H = NouraguesHD$H,
#'   drawGraph = TRUE, useWeight = TRUE
#' )
#' 
#' # Compute the local H-D model with the lowest RSE
#' HDmodel <- modelHD(
#'   D = NouraguesHD$D, H = NouraguesHD$H,
#'   method = "log2", useWeight = TRUE
#' )
#' 
#' # Compute plot-specific H-D models
#' HDmodelPerPlot <- modelHD(NouraguesHD$D, NouraguesHD$H,
#'   method = "weibull",
#'   useWeight = T, plot = NouraguesHD$plotId
#' )
#' 
#' RSEmodels <- sapply(HDmodelPerPlot, function(x) x$RSE)
#' Coeffmodels <- lapply(HDmodelPerPlot, function(x) x$coefficients)
#' 
#' # Retrieve height data from a local HD model
#' dataHlocal <- retrieveH(D = KarnatakaForest$D, model = HDmodel)
#' 
#' # Retrieve height data from a Feldpaush et al. (2012) averaged model
#' dataHfeld <- retrieveH(D = KarnatakaForest$D, region = "SEAsia")
#' 
#' # Retrieve height data from Chave et al. (2012) equation 6
#' dataHchave <- retrieveH(
#'   D = KarnatakaForest$D,
#'   coord = cbind(KarnatakaForest$long, KarnatakaForest$lat)
#' )
#' 
#' #############################################################################
#' # AGB CALCULATION
#' 
#' KarnatakaForest$WD <- dataWD$meanWD
#' KarnatakaForest$H <- dataHlocal$H
#' KarnatakaForest$Hfeld <- dataHfeld$H
#' 
#' # Compute AGB(Mg) per tree
#' AGBtree <- computeAGB(
#'   D = KarnatakaForest$D, WD = KarnatakaForest$WD,
#'   H = KarnatakaForest$H
#' )
#' 
#' # Compute AGB(Mg) per plot
#' AGBPlotList <- by(KarnatakaForest, KarnatakaForest$plotId,
#'   function(x) computeAGB(D = x$D, WD = x$WD, H = x$H),
#'   simplify = F
#' )
#' AGBplot <- sapply(AGBPlotList, sum)
#' 
#' # Compute AGB(Mg) per tree without height information (Eq. 7 from Chave et al. (2014))
#' AGBPlotListChave <- by(KarnatakaForest, KarnatakaForest$plotId,
#'   function(x) computeAGB(D = x$D, WD = x$WD, coord = cbind(x$long, x$lat)),
#'   simplify = F
#' )
#' AGBplotChave <- sapply(AGBPlotListChave, sum)
#' 
#' # Compute AGB(Mg) per tree with Feldpausch et al. (2012) regional H-D model
#' AGBPlotListFeld <- by(KarnatakaForest, KarnatakaForest$plotId,
#'   function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
#'   simplify = F
#' )
#' AGBplotFeld <- sapply(AGBPlotListFeld, sum)
#' 
#' #############################################################################
#' # PROPAGATING ERRORS
#' 
#' KarnatakaForest$sdWD <- dataWD$sdWD
#' KarnatakaForest$HfeldRSE <- dataHfeld$RSE
#' 
#' # Per plot using the local HD model constructed above (modelHD)
#' resultMC <- AGBmonteCarlo(
#'   D = KarnatakaForest$D, WD = KarnatakaForest$WD, errWD = KarnatakaForest$sdWD,
#'   HDmodel = HDmodel, Dpropag = "chave2004"
#' )
#' resMC <- summaryByPlot(resultMC$AGB_simu, KarnatakaForest$plotId)
#' 
#' # Per plot using the Feldpaush regional HD averaged model
#' resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
#'   function(x) AGBmonteCarlo(
#'       D = x$D, WD = x$WD, errWD = x$sdWD, H = x$Hfeld,
#'       errH = x$HfeldRSE, Dpropag = "chave2004"
#'     ),
#'   simplify = F
#' )
#' meanAGBperplotFeld <- unlist(sapply(resultMC, "[", 1))
#' credperplotFeld <- sapply(resultMC, "[", 4)
#' 
#' # Per plot using Chave et al. (2014) Equation 7
#' resultMC <- AGBmonteCarlo(
#'   D = KarnatakaForest$D, WD = KarnatakaForest$WD, errWD = KarnatakaForest$sdWD,
#'   coord = KarnatakaForest[, c("long", "lat")],
#'   Dpropag = "chave2004"
#' )
#' resMC <- summaryByPlot(resultMC$AGB_simu, KarnatakaForest$plotId)
#' }
#' @keywords internal
"_PACKAGE"
