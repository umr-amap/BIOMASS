#' Estimating wood density and associated uncertainties
#'
#' @description
#' The function estimates the wood density (WD) and the associated sd of the trees from their taxonomy or from their congeners using the global wood density database V2 (Fischer et al. 2025) or any additional dataset if the sd is also provided. The WD can either be attributed to an individual at a species, genus, family or stand level.
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
#' - four (or five) columns: "genus","species","WD","sdWD" (the fifth column "family" is optional)
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
#' @author Arthur BAILLY, Maxime REJOU-MECHAIN
#'
#' @references
#' Fischer, F. J., et al. 2025 _A global map of wood density_ https://doi.org/10.1101/2025.08.25.671920
#' Fischer, F. J., et al. 2025 _Beyond species means - the intraspecific contribution to global wood density variation_ https://doi.org/10.1101/2025.08.25.671896
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
#' @importFrom data.table data.table := setDF setDT setkey chmatch %chin% merge
#'
getWoodDensity <- function(genus, species, family = NULL, stand = NULL,
                           addWoodDensityData = NULL, verbose = TRUE) {


  # Parameters verification ----------------------------------------------------

  if (length(genus) != length(species)) {
    stop("Your data (genus and species) do not have the same length")
  }

  if (!is.null(family) && (length(genus) != length(family))) {
    stop("Your family vector and your genus/species vectors do not have the same length")

    if (any(colSums(table(family, genus) > 0, na.rm = TRUE) >= 2)) {
      stop("Some genera are in two or more families")
    }
  }

  if (!is.null(stand) && (length(genus) != length(stand))) {
    stop("Your stand vector and your genus/species vectors do not have the same length")
  }

  if (!is.null(addWoodDensityData)) {
    if (!(all(names(addWoodDensityData) %in% c("genus", "species", "wd","sd", "family")) && length(names(addWoodDensityData)) %in% c(4, 5))) {
      stop('The additional wood density database should be organized in a dataframe with four (or five) columns:
           "genus","species","wd","sd", and the column "family" is optional')
    }
  }

  
  # Data processing ------------------------------------------------------------

  # Load global wood density estimates and uncertainties (created from data-raw/format_wsg_estimates_and_sd.R)
  wdData <- data.table(BIOMASS::wsg_estimates)

  # Adding addWoodDensityData to wdData
  if (!is.null(addWoodDensityData)) {
    setDT(addWoodDensityData)
    addWoodDensityData[,species := paste(genus, species)]
    # Retrieve family : I think we don't need to do it 
    # if (!("family" %in% names(addWoodDensityData))) {
    #   addWoodDensityData <- merge(x = addWoodDensityData, y = wdData[level_tax=="genus", c("family","genus")], by = "genus", all.x = TRUE)
    # }
    addWoodDensityData <- addWoodDensityData[!is.na(WD), ]
    wdData <- merge(wdData, addWoodDensityData, by = c("family", "genus", "species"), all = TRUE)
    wdData[!is.na(WD), wsg := WD]
    wdData[!is.na(sdWD), sd := sdWD]
  }

  # Do we print the following information ??? subWdData contained individual measurements so if we want to print it, we have to retrieve the number of individual measurements in GWDD_v2... BUT GWDD_v2 contains aggregate measurements !
  # if (verbose) {
  #   message("The reference dataset contains ", nrow(subWdData), " wood density values")
  # }

  # Creating an input dataframe
  inputData <- data.table(genus = as.character(genus), species = as.character(species))

  # Adding the family if not provided
  if (!is.null(family)) {
    inputData[, family := as.character(family)]
  } else {
    if (!exists("genusFamily", inherits = FALSE)) {
      genusFamily <- data.table(BIOMASS::genusFamily)
    }
    inputData[genusFamily, on = "genus", family := i.family]
  }

  if (!is.null(stand)) {
    inputData[, stand := as.character(stand)]
  }

  taxa <- unique(inputData, by = c("family", "genus", "species"))
  if (verbose) {
    message("Your taxonomic table contains ", nrow(taxa), " taxa")
  }

  # utilitary function : paste y values inside x when one x value is NA
  coalesce <- function(x, y) {
    if (length(y) == 1) {
      y <- rep(y, length(x))
    }
    where <- is.na(x)
    x[where] <- y[where]
    x
  }

  # Select only the relevant data
  meanWdData <- subWdData[(family %in% taxa$family | genus %in% taxa$genus | species %in% taxa$species), ]

  if (nrow(meanWdData) == 0) {
    stop("Our database have not any of your family, genus and species")
  }


  # If there is no genus or species level
  inputData[, ":="(meanWD = NA_real_, nInd = NA_integer_, sdWD = NA_real_, levelWD = NA_character_)]


  if (!((!is.null(family) && nrow(merge(inputData, meanWdData[, .N, by = .(family)], c("family"))) != 0) ||
    nrow(merge(inputData, meanWdData[, .N, by = .(family, genus)], c("family", "genus"))) != 0 ||
    nrow(merge(inputData, meanWdData[, .N, by = .(family, genus, species)], c("family", "genus", "species"))) != 0)) {
    stop("There is no exact match among the family, genus and species, try with 'addWoodDensity'
         or inform the 'family' or increase the 'region'")
  }

  # Extracting data ---------------------------------------------------------

  # compute mean at species level
  sdSP <- sd_10[taxo == "species", sd]
  meanSP <- meanWdData[,
    by = c("family", "genus", "species"),
    .(
      meanWDsp = mean(wd),
      nIndsp = .N,
      sdWDsp = sdSP
    )
  ]
  inputData[meanSP,
    on = c("family", "genus", "species"), by = .EACHI,
    `:=`(
      meanWD = meanWDsp,
      nInd = nIndsp,
      sdWD = sdWDsp,
      levelWD = "species"
    )
  ]

  # mean at genus level
  sdGN <- sd_10[taxo == "genus", sd]
  meanGN <- meanSP[,
    by = c("family", "genus"),
    .(
      meanWDgn = mean(meanWDsp),
      nIndgn = .N,
      sdWDgn = sdGN
    )
  ]
  inputData[meanGN,
    on = c("family", "genus"), by = .EACHI,
    `:=`(
      meanWD = coalesce(meanWD, meanWDgn),
      nInd = coalesce(nInd, nIndgn),
      sdWD = coalesce(sdWD, sdWDgn),
      levelWD = coalesce(levelWD, "genus")
    )
  ]

  # mean at family level if provided
  if (!is.null(family)) {
    sdFM <- sd_10[taxo == "family", sd]
    meanFM <- meanGN[,
      by = family,
      .(
        meanWDfm = mean(meanWDgn),
        nIndfm = .N,
        sdWDfm = sdFM
      )
    ]
    inputData[meanFM,
      on = "family", by = .EACHI,
      `:=`(
        meanWD = coalesce(meanWD, meanWDfm),
        nInd = coalesce(nInd, nIndfm),
        sdWD = coalesce(sdWD, sdWDfm),
        levelWD = coalesce(levelWD, "family")
      )
    ]
  }

  # mean at stand level if provided
  if (!is.null(stand)) {
    meanST <- inputData[!is.na(meanWD),
      by = stand,
      .(
        meanWDst = mean(meanWD),
        nIndst = .N,
        sdWDst = sd(meanWD)
      )
    ]
    inputData[is.na(meanWD), levelWD := stand]
    inputData[meanST,
      on = "stand", by = .EACHI,
      `:=`(
        meanWD = coalesce(meanWD, meanWDst),
        nInd = coalesce(nInd, nIndst),
        sdWD = coalesce(sdWD, sdWDst)
      )
    ]
  }

  # mean of whole dataset for remaining NA
  meanDS <- inputData[
    !is.na(meanWD),
    .(
      meanWDds = mean(meanWD),
      nIndds = .N,
      sdWDds = sd(meanWD)
    )
  ]
  inputData[
    is.na(meanWD),
    `:=`(
      meanWD = meanDS$meanWDds,
      nInd = meanDS$nIndds,
      sdWD = meanDS$sdWDds,
      levelWD = "dataset"
    )
  ]

  # Deal with NA or zero values in sdWD (adopt the most conservative approach assigning the sd over the full wdData dataset)
  #(very specific cases where no or only one species co-occur with unidentified individuals in the plot)
  inputData[is.na(sdWD) | sdWD==0, sdWD:=sd_tot]
  
  # Convert to a dataframe
  result <- setDF(inputData[, .(family, genus, species, meanWD, sdWD, levelWD, nInd)])
  
  return(result)
}
