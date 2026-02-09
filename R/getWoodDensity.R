#' Estimating wood density and associated uncertainties
#'
#' @description
#' The function estimates the wood density (WD) and the associated standard deviation of the trees from their taxonomy or from their congeners using the global wood density database V2 (Fischer et al. 2026) or any additional dataset if the sd is also provided. The WD can either be attributed to an individual at a species, genus, family or stand level.
#'
#' @param genus Vector of genus names.
#' @param species Vector of species names.
#' @param family (optional) Vector of families. If set, the missing wood densities at the genus
#' level will be attributed at family level if available.
#' @param stand (optional) Vector with the corresponding stands of your data.
#' If set, the missing wood densities at the genus level will be attributed at stand level.
#' If not, the value attributed will be the mean of the whole tree dataset.
#' @param addWoodDensityData A dataframe containing additional wood density data to be
#'  combined with the global wood density database (see Details).
#' @param verbose A logical, give some statistic with the database
#'
#' @details
#' The function assigns wood density estimates (WD) and uncertainty (sigma) at species, genus or family level to each taxon, using the results of Bayesian hierarchical modelling on the Global Wood Density Database V2, with the following brms formula:
#' WD ~ 1 + (1 | family / genus / species) + (1 | source_short)
#' sigma ~ 1 + ind + (1 | species)
#' The uncertainties related to the genus and family are then estimated by simulating WD values for all the species.
#' 
#' If a taxon is unidentified or absent from the database, the estimated WD and uncertainty of the stand (if set) is given.
#'
#' When supplying addWoodDensityData, the dataframe should be organized as follow:
#' - four (or five) columns: "genus","species","meanWD","sdWD" (the fifth column "family" is optional)
#' - one row per species (not per individual measurement)
#' The taxa present in addWoodDensityData will replace the GWDD V2 estimates.
#'
#' @return Returns a dataframe containing the following information:
#'   - `family`: Family
#'   - `genus`: Genus
#'   - `species`: Species
#'   - `meanWD` (g/cm^3): Mean wood density estimates
#'   - `sdWD` (g/cm^3): Standard deviation estimates of the wood density
#'   - `levelWD`: Level at which wood density has been calculated. Can be species, genus, family,
#' dataset (mean of the entire dataset) or, if stand is set, the name of the stand (mean of the current stand)
#'
#' @export
#'
#' @author Arthur BAILLY, Maxime REJOU-MECHAIN, Fabian FISCHER, Dominique LAMONICA
#'
#' @references
#' Fischer, F. J., et al. (2026). Beyond species means - the intraspecific contribution to global wood density variation. New Phytol. https://doi.org/10.1111/nph.70860
#' Fischer, F. J., et al. (2026). Global Wood Density Database v.2 (GWDD v.2) (Data set). Zenodo. https://doi.org/10.5281/zenodo.18262736
#' 
#' @examples
#' # Load a data set
#' data(NouraguesTrees)
#'
#' # Compute the Wood Density up to the genus level and give the mean wood density of the dataset
#' \donttest{
#' WD <- getWoodDensity(
#'   genus = NouraguesTrees$Genus,
#'   species = NouraguesTrees$Species
#' )
#' }
#' 
#' # Compute the Wood Density up to the genus level and then give the mean wood density per stand
#' \donttest{
#' WD <- getWoodDensity(
#'   genus = NouraguesTrees$Genus,
#'   species = NouraguesTrees$Species,
#'   stand = NouraguesTrees$plotId
#' )
#' }
#' 
#' @seealso [wsg_estimates]
#' @keywords Wood density
#' @importFrom data.table data.table := setDF setnames setcolorder
#'
getWoodDensity <- function(genus, species, family = NULL, stand = NULL,
                           addWoodDensityData = NULL, verbose = TRUE) {


  # Parameters verification ----------------------------------------------------

  if (length(genus) != length(species)) {
    stop("Your data (genus and species) do not have the same length")
  }

  if (!is.null(family)) {
    if( length(genus) != length(family) ) {
      stop("Your family vector and your genus/species vectors do not have the same length")
    }
    if (any(colSums(table(family, genus) > 0, na.rm = TRUE) >= 2)) {
      stop("Some genera are in two or more families")
    }
  }

  if (!is.null(stand) && (length(genus) != length(stand))) {
    stop("Your stand vector and your genus/species vectors do not have the same length")
  }

  if (!is.null(addWoodDensityData)) {
    if (!(all(names(addWoodDensityData) %in% c("genus", "species", "meanWD","sdWD", "family")) && length(names(addWoodDensityData)) %in% c(4, 5))) {
      stop('The additional wood density database should be organized in a dataframe with four (or five) columns:
           "genus","species","meanWD","sdWD", and the column "family" is optional')
    }
  }

  
  # Data processing ------------------------------------------------------------

  # Load global wood density estimates and uncertainties
  wdData <- data.table(BIOMASS::wsg_estimates)

  # Adding addWoodDensityData to wdData
  if (!is.null(addWoodDensityData)) {
    # Formatting addWoodDensityData:
    addWoodDensityData <- as.data.table(addWoodDensityData)
    addWoodDensityData[,species := paste(genus, species)]
    addWoodDensityData <- addWoodDensityData[!is.na(meanWD), ]
    addWoodDensityData[!is.na(species), add_level_tax := "species"]
    addWoodDensityData[is.na(species) & !is.na(genus), add_level_tax := "genus"]
    if("family" %in% names(addWoodDensityData)) {
      addWoodDensityData[is.na(species) & is.na(genus) & !is.na(family), add_level_tax := "family"]
    }
    
    wdData <- merge(wdData, addWoodDensityData, by = c("family", "genus", "species"), all = TRUE)
    wdData[!is.na(meanWD), wsg := meanWD]
    wdData[!is.na(sdWD), sd := sdWD]
    wdData[!is.na(add_level_tax), level_tax := add_level_tax]
    wdData[, c("meanWD","sdWD") := NULL]
  }

  # Creating an input data-table
  inputData <- data.table(genus = as.character(genus), species = as.character(species))
  inputData[, binomial := paste(genus, species)] # a species name can be found in multiple genus, so we will match the binomial names instead of species names
  
  # Assign family if provided 
  if (!is.null(family)) {
    inputData[, family := as.character(family)]
  }

  # Assign stand if provided 
  if (!is.null(stand)) {
    inputData[, stand := as.character(stand)]
  }

  taxa <- unique(inputData, by = c("binomial"))
  if (verbose) {
    message("Your taxonomic table contains ", nrow(taxa), " taxa")
  }

  # # If there is no genus or species level
  # inputData[, ":="(meanWD = NA_real_, nInd = NA_integer_, sdWD = NA_real_, levelWD = NA_character_)]
  # 
  # if (nrow(meanWdData) == 0) {
  #   stop("Our database have not any of your family, genus and species")
  # }

  # if (!((!is.null(family) && nrow(merge(inputData, meanWdData[, .N, by = .(family)], c("family"))) != 0) ||
  #   nrow(merge(inputData, meanWdData[, .N, by = .(family, genus)], c("family", "genus"))) != 0 ||
  #   nrow(merge(inputData, meanWdData[, .N, by = .(family, genus, species)], c("family", "genus", "species"))) != 0)) {
  #   stop("There is no exact match among the family, genus and species, please check that 'genus' and 'species' doesn't contain any special character or provide 'addWoodDensity' dataset.")
  # }

  
  # Merging WdData with inputData ----------------------------------------------

  # At species level: 
  inputData <- merge(x = inputData, y = wdData[, c("species","wsg","sd","level_tax")], all.x = TRUE, by.x = "binomial", by.y = "species")
  
  # At genus level: 
  inputData[ is.na(level_tax) & !is.na(genus),
             c("wsg", "sd", "level_tax") := wdData[is.na(species),][
               .SD,
               on = .(genus),
               .(wsg, sd, level_tax)]
  ]
  
  # At family level: 
  if(!is.null(family)) {
    inputData[ is.na(level_tax) & !is.na(family),
               c("wsg", "sd", "level_tax") := wdData[is.na(genus),][
                 .SD,
                 on = .(family),
                 .(wsg, sd, level_tax)]
    ]
  }
  
  # Warning when family is not provided and when all taxa are not matched
  if(is.null(family) && sum(is.na(inputData$wsg)) != 0) {
      warning( paste( sum(is.na(inputData$wsg)), "taxa don't match the Global Wood Density Database V2. You may provide 'family' to match wood density estimates at family level." ) )
  }
  
  
  
  # Compute wsg and sd at stand level ------------------------------------------
  
  # utilitary function : paste y values inside x when one x value is NA
  coalesce <- function(x, y) {
    if (length(y) == 1) {
      y <- rep(y, length(x))
    }
    where <- is.na(x)
    x[where] <- y[where]
    x
  }
  
  # retrieve mean at stand level if provided
  if (!is.null(stand)) {
    # Calculate mean(WD) and sd(WD) at stand level
    meanST <- inputData[!is.na(wsg),
      by = stand,
      .(
        meanWDst = mean(wsg),
        sdWDst = sd(wsg)
      )
    ]
    inputData[is.na(wsg), level_tax := stand]
    inputData[meanST,
      on = "stand", by = .EACHI,
      `:=`(
        wsg = coalesce(wsg, meanWDst),
        sd = coalesce(sd, sdWDst)
      )
    ]
  }

  # mean of whole dataset for remaining NA
  meanDS <- inputData[
    !is.na(wsg),
    .(
      meanWDds = mean(wsg),
      sdWDds = sd(wsg)
    )
  ]
  inputData[
    is.na(wsg),
    `:=`(
      wsg = meanDS$meanWDds,
      sd = meanDS$sdWDds,
      level_tax = "dataset"
    )
  ]
  
  # Formatting output ----------------------------------------------------------
  
  setnames(x = inputData, old = c("wsg","sd","level_tax"), new = c("meanWD","sdWD","levelWD"))
  inputData[, binomial := NULL]
  
  if(!is.null(family)) { # family in first column if prodived
    setcolorder(inputData, "family", before=1)
  }
  
  return(setDF(inputData))
}
