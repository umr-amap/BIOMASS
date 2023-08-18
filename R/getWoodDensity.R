if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "regionId", "i.family", "wd", "wd.x", "wd.y", "taxo", ".EACHI",
    "meanWDsp", "nIndsp", "sdWDsp", "meanWD", "meanWDgn", "nInd", "nIndgn",
    "sdWD", "sdWDgn", "levelWD", "meanWDfm", "nIndfm", "sdWDfm",
    "meanWDst", "nIndst", "sdWDst"
  ))
}


#' Estimating wood density
#'
#' The function estimates the wood density (WD) of the trees from their taxonomy or from their
#' congeners using the global wood density database (Chave et al. 2009, Zanne et al. 2009) or
#' any additional dataset. The WD can either be attributed to an individual at a species, genus,
#' family or stand level.
#'
#' @param genus Vector of genus names
#' @param species Vector of species names
#' @param stand (optional) Vector with the corresponding stands of your data.
#' If set, the missing wood densities at the genus level will be attributed at stand level.
#' If not, the value attributed will be the mean of the whole tree dataset.
#' @param family (optional) Vector of families. If set, the missing wood densities at the genus
#' level will be attributed at family level if available.
#' @param region Region (or vector of region) of interest of your sample. By default, Region is
#' set to 'World', but you can restrict the WD estimates to a single region :
#'   - `AfricaExtraTrop`: Africa (extra tropical)
#'   - `AfricaTrop`: Africa (tropical)
#'   - `Australia`: Australia
#'   - `AustraliaTrop`: Australia (tropical)
#'   - `CentralAmericaTrop`: Central America (tropical)
#'   - `China`: China
#'   - `Europe`: Europe
#'   - `India`: India
#'   - `Madagascar`: Madagascar
#'   - `Mexico`: Mexico
#'   - `NorthAmerica`: North America
#'   - `Oceania`: Oceania
#'   - `SouthEastAsia`: South-East Asia
#'   - `SouthEastAsiaTrop`: South-East Asia (tropical)
#'   - `SouthAmericaExtraTrop`: South America (extra tropical)
#'   - `SouthAmericaTrop`: South America (tropical)
#'   - `World`: World
#'
#' @param addWoodDensityData A dataframe containing additional wood density data to be
#'  combined with the global wood density database. The dataframe should be organized
#'  in a dataframe with three (or four) columns: "genus","species","wd", the fourth
#'  column "family" is optional.
#' @param verbose A logical, give some statistic with the database
#'
#' @details
#' The function assigns to each taxon a species- or genus- level average if at least
#'  one wood density value at the genus level is available for that taxon in the reference database.
#'  If not, the mean wood density of the family (if set) or of the stand (if set) is given.
#'
#' The function also provides an estimate of the error associated with the wood density estimate
#'  (i.e. a standard deviation): a mean standard deviation value is given to the tree at the
#'  appropriate taxonomic level using the [sd_10] dataset.
#'
#'
#' @return Returns a dataframe containing the following information:
#'   - `family`: (if set) Family
#'   - `genus`: Genus
#'   - `species`: Species
#'   - `meanWD` (g/cm^3): Mean wood density
#'   - `sdWD` (g/cm^3): Standard deviation of the wood density that can be used in error propagation
#' (see [sd_10] and [AGBmonteCarlo()])
#'   - `levelWD`: Level at which wood density has been calculated. Can be species, genus, family,
#' dataset (mean of the entire dataset) or, if stand is set, the name of the stand (mean of the current stand)
#'   - `nInd`: Number of individuals taken into account to compute the mean wood density
#'
#' @export
#'
#' @author Maxime REJOU-MECHAIN, Arthur PERE, Ariane TANGUY
#'
#' @references
#' Chave, J., et al. _Towards a worldwide wood economics spectrum_. Ecology letters 12.4 (2009): 351-366.
#' Zanne, A. E., et al. _Global wood density database_. Dryad. Identifier: http://hdl. handle. net/10255/dryad 235 (2009).
#'
#'
#' @examples
#' # Load a data set
#' data(KarnatakaForest)
#'
#' # Compute the Wood Density up to the genus level and give the mean wood density of the dataset
#' \donttest{
#' WD <- getWoodDensity(
#'   genus = KarnatakaForest$genus,
#'   species = KarnatakaForest$species
#' )
#' }
#' 
#' # Compute the Wood Density up to the genus level and then give the mean wood density per stand
#' \donttest{
#' WD <- getWoodDensity(
#'   genus = KarnatakaForest$genus,
#'   species = KarnatakaForest$species,
#'   stand = KarnatakaForest$plotId
#' )
#' }
#' 
#' # Compute the Wood Density up to the family level and then give the mean wood density per stand
#' \donttest{
#' WD <- getWoodDensity(
#'   family = KarnatakaForest$family,
#'   genus = KarnatakaForest$genus,
#'   species = KarnatakaForest$species,
#'   stand = KarnatakaForest$plotId
#' )
#' str(WD)
#' }
#' @seealso [wdData], [sd_10]
#' @keywords Wood density
#' @importFrom data.table data.table := setDF setDT setkey copy chmatch %chin%
#'
getWoodDensity <- function(genus, species, stand = NULL, family = NULL, region = "World",
                           addWoodDensityData = NULL, verbose = TRUE) {


  # Parameters verification -------------------------------------------------

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
    if (!(all(names(addWoodDensityData) %in% c("genus", "species", "wd", "family")) && length(names(addWoodDensityData)) %in% c(3, 4))) {
      stop('The additional wood density database should be organized in a dataframe with three (or four) columns:
           "genus","species","wd", and the column "family" is optional')
    }
  }




  # Data processing ---------------------------------------------------------

  # Load global wood density database downloaded from http://datadryad.org/handle/10255/dryad.235
  wdData <- setDT(copy(BIOMASS::wdData))

  # Load the mean standard deviation observed at the species, Genus or Family level
  # in the Dryad dataset when at least 10 individuals are considered
  sd_10 <- setDT(copy(BIOMASS::sd_10))
  sd_tot <- sd(wdData$wd)

  Region <- tolower(region)
  if ((Region != "world") && any(is.na(chmatch(Region, tolower(wdData$regionId))))) {
    stop("One of the region you entered is not recognized in the global wood density database")
  }

  subWdData <- wdData
  if (!("world" %in% Region)) {
    subWdData <- wdData[tolower(regionId) %chin% Region]
  }

  if (nrow(subWdData) < 1000 && is.null(addWoodDensityData)) {
    warning(
      "DRYAD data only stored ", nrow(subWdData), " wood density values in your region of interest. ",
      'You could provide additional wood densities (parameter addWoodDensityData) or widen your region (region="World")'
    )
  }

  if (!is.null(addWoodDensityData)) {
    setDT(addWoodDensityData)
    if (!("family" %in% names(addWoodDensityData))) {
      genusFamily <- setDT(copy(BIOMASS::genusFamily))
      addWoodDensityData[genusFamily, on = "genus", family := i.family]
    }
    addWoodDensityData <- addWoodDensityData[!is.na(wd), ]
    subWdData <- merge(subWdData, addWoodDensityData, by = c("family", "genus", "species"), all = TRUE)
    subWdData[!is.na(regionId), wd := wd.x][is.na(regionId), wd := wd.y][, ":="(wd.x = NULL, wd.y = NULL)]
  }

  if (verbose) {
    message("The reference dataset contains ", nrow(subWdData), " wood density values")
  }

  # Creating an input dataframe
  inputData <- data.table(genus = as.character(genus), species = as.character(species))


  if (!is.null(family)) {
    inputData[, family := as.character(family)]
  } else {
    if (!exists("genusFamily", inherits = FALSE)) {
      genusFamily <- setDT(copy(BIOMASS::genusFamily))
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
