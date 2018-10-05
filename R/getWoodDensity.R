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
#' @param region Region of interest of your sample. By default, Region is set to 'World', but you 
#' can restrict the WD estimates to a single region : 
#' \itemize{
#'   \item \code{AfricaExtraTrop}: Africa (extra tropical)
#'   \item \code{AfricaTrop}: Africa (tropical)
#'   \item \code{Australia}: Australia
#'   \item \code{AustraliaTrop}: Australia (tropical)
#'   \item \code{CentralAmericaTrop}: Central America (tropical)
#'   \item \code{China}: China
#'   \item \code{Europe}: Europe
#'   \item \code{India}: India
#'   \item \code{Madagascar}: Madagascar
#'   \item \code{Mexico}: Mexico
#'   \item \code{NorthAmerica}: North America
#'   \item \code{Oceania}: Oceania
#'   \item \code{SouthEastAsia}: South-East Asia
#'   \item \code{SouthEastAsiaTrop}: South-East Asia (tropical)
#'   \item \code{SouthAmericaExtraTrop}: South America (extra tropical)
#'   \item \code{SouthAmericaTrop}: South America (tropical)
#'   \item \code{World}: World
#' }
#' @param addWoodDensityData A dataframe containing additional wood density data to be
#'  combined with the global wood density database. The dataframe should be organized 
#'  in a dataframe with three columns: "genus","species","wd" (column order and names 
#'  should be respected).
#' @param verbose (optional) a binary set on TRUE that will display the test how much
#' the reference dataset have data and how much the dataset provided have data
#'  
#' @details 
#' The function assigns to each taxon a species- or genus- level average if at least
#'  one wood density value at the genus level is available for that taxon in the reference database.
#'  If not, the mean wood density of the family (if set) or of the stand (if set) is given.
#' 
#' The function also provides an estimate of the error associated with the wood density estimate
#'  (i.e. a standard deviation): a mean standard deviation value is given to the tree at the 
#'  appropriate taxonomic level using the sd_10 dataset (see \code{\link{sd_10}}).
#'
#'
#' @return Returns a dataframe containing the following information: 
#' \item{family}{(if set) Family}
#' \item{genus}{Genus}
#' \item{species}{Species}
#' \item{meanWD}{Mean wood density}
#' \item{sdWD}{Standard deviation of the wood density that can be used in error propagation 
#' (see \code{\link{sd_10}} and \code{\link{AGBmonteCarlo}})}
#' \item{levelWD}{Level at which wood density has been calculated. Can be species, genus, family, 
#' Dataset (mean of the entire dataset) or, if stand is set, the name of the stand (mean of the current stand)}
#' \item{nInd}{Number of individuals taken into account to compute the mean wood density}
#' @export
#'
#' @examples
#' # Load a data set
#' data(KarnatakaForest)
#' 
#' # Compute the Wood Density up to the genus level and give the mean wood density of the dataset
#' WD <- getWoodDensity(genus = KarnatakaForest$genus, 
#'                      species = KarnatakaForest$species)
#' 
#' # Compute the Wood Density up to the genus level and then give the mean wood density per stand
#' WD <- getWoodDensity(genus = KarnatakaForest$genus, 
#'                      species = KarnatakaForest$species, 
#'                      stand = KarnatakaForest$plotId)
#' 
#' # Compute the Wood Density up to the family level and then give the mean wood density per stand
#' WD <- getWoodDensity(family = KarnatakaForest$family, 
#'                      genus = KarnatakaForest$genus, 
#'                      species = KarnatakaForest$species, 
#'                      stand = KarnatakaForest$plotId)
#' str(WD)
#' 
#' @seealso \code{\link{wdData}}, \code{\link{sd_10}}
#' @keywords Wood density
#' @importFrom data.table data.table := setDF setDT setkey
#' 
getWoodDensity <- function(genus, species, stand = NULL, family = NULL, region = "World", 
                           addWoodDensityData = NULL, verbose = TRUE)
{  
  ### For each genus and species, assign a wood density
  
  if(length(genus) != length(species))
    stop("Your data (genus and species) don't have the same lenght")
  
  # Load global wood density database downloaded from http://datadryad.org/handle/10255/dryad.235
  wdData <- NULL
  data(wdData, envir = environment()) 
  # Load the mean standard deviation observed at the species, Genus or Family level in the Dryad dataset when at least 10 individuals are considered
  sd_10 <- NULL
  data(sd_10, envir = environment()) 
  
  # Set the database on data.table
  setDT(wdData, key = c('family', 'genus', 'species'))
  setDT(sd_10)
  wdData[, region := NULL]
  wdData[, referenceNumber := NULL]
  
  subWdData <- wdData
  if(region != "World") 
    subWdData <- wdData[regionId == region,] 
  
  if(nrow(subWdData) == 0) 
    stop("The region you entered is not recognized in the global wood density database")
  
  if(nrow(subWdData)<1000 & is.null(addWoodDensityData)) 
  {
    cat("DRYAD data only stored", nrow(subWdData), "wood density values in your region of interest. 
              Would you want to try with the 'World' option ? (y/n)")
    ans <- scan(what = character(), nmax = 1, quiet = TRUE)
    
    # Set the world option instead
    if(ans == "y")
      subWdData <- wdData 
  }
  
  if(!is.null(addWoodDensityData))
  {
    if(any(names(addWoodDensityData) != c("genus","species","wd")))
      stop("The additional wood density database should be organized in a dataframe with three columns: 
           \"genus\",\"species\",\"wd\" (column order and names should be respected)")
    setDT(addWoodDensityData)
    addWoodDensityData[, family := getTaxonomy(genus)$family]
    addWoodDensityData <- addWoodDensityData[!is.na(wd),]
    subWdData = merge(subWdData, addWoodDensityData, by = c('family', "genus", "species"), all = T)
    subWdData[!is.na(regionId), wd := wd.x][is.na(regionId), wd := wd.y][, ':='(wd.x = NULL, wd.y = NULL)]
  }
  setkey(subWdData, family, genus, species)
  
  if (verbose)
    cat("The reference dataset contains",nrow(subWdData), "wood density values \n")
  
  # Creating an input dataframe
  inputData <- data.table(id = 1:length(genus), genus = as.character(genus), species = as.character(species), key = "id")
  
  if(!is.null(family))
  {
    if(length(genus) == length(family))
      inputData[, family := as.character(family)]
    else
      stop("Your family vector and your genus/species vectors don't have the same length")
  } else {
    inputData[, family := getTaxonomy(genus)$family]
  }
  
  taxa = unique( inputData[, .(genus, species, family)] )
  if (verbose)
    cat("Your taxonomic table contains", nrow(taxa), "taxa \n")
  
  if(!is.null(stand))
  {
    if(length(genus) == length(stand))
      inputData[, stand := as.character(stand)]
    else
      stop("Your stand vector and your genus/species vectors don't have the same length")
  }
  
  
  setkey(inputData, family, genus, species)
  
  # Select only the relevant data
  meanWdData <- subWdData[(ifelse(!is.null(family), family %in% taxa$family, F) | genus %in% taxa$genus | species %in% taxa$species), ]
  
  # Compute the mean at species, genus (and family) level
  # -- Species
  allMean = meanWdData[, .(meanWDsp = mean(wd), nIndSp = .N), by=.(family, genus, species)]
  outData = merge(inputData, allMean, all.x = T, by = c("family","genus","species"))
  
  # -- Genus
  allMean = allMean[, .(meanWDgen = mean(meanWDsp), nIndGen = .N), by=.(family, genus)]
  outData = merge(outData, allMean, all.x = T, by = c("family","genus"))
  
  
  #### Create the final WD estimates
  ###################################
  # At the species level
  outData[, ':='(meanWD = meanWDsp, 
                 nInd = nIndSp, 
                 levelWD = "species", 
                 sdWD = sd_10[taxo == "species", sd])]
  
  ###################################
  # At the genus level
  outData[ is.na(meanWD), 
           ':='(meanWD = meanWDgen, 
                nInd = nIndGen, 
                levelWD = "genus", 
                sdWD = sd_10[taxo == "genus", sd])]
  
  # If the family is provided, compute the wood density at the family level
  if(!is.null(family))
  {
    allMean = allMean[, .(meanWDfam = mean(meanWDgen), nIndFam = .N), by=.(family)]
    outData = merge(outData, allMean, all.x = T, by = c("family"))
    
    ###################################
    # At the family level
    outData[ is.na(meanWD), 
             ':='(meanWD = meanWDfam, 
                  nInd = nIndFam, 
                  levelWD = "family", 
                  sdWD = sd_10[taxo == "family", sd])]
  }
  
  # If the stand is provided, compute the wood density at the stand level  
  if(!is.null(stand))
  {
    standWD <- outData[, .(meanStand = mean(meanWD, na.rm = T), nIndStand = .N, sdStand = sd(meanWD, na.rm = T) ), by = stand]
    
    outData <- merge(outData, standWD, by = "stand", all.x = T)
    outData[is.na(meanWD), ':='(meanWD = meanStand,
                                nInd = nIndStand,
                                sdWD = sdStand,
                                levelWD = as.character(stand))]
    
  }
  
  # If some values are still NA, set the Wood density of the whole dataset
  WD_isnt_NA = outData[!is.na(meanWD), .(meanWD = mean(meanWD), .N, sdWD = sd(meanWD))]
  outData[is.na(meanWD), ':='(meanWD = WD_isnt_NA$meanWD, 
                              nInd = WD_isnt_NA$N, 
                              sdWD = WD_isnt_NA$sdWD, 
                              levelWD = "dataset")]
  
  result <- setDF( outData[order(id), .(family, genus, species, meanWD, sdWD, levelWD ,nInd)] )
  
  if(nrow(result) != nrow(inputData)) 
    warning(paste("The input and the output tables have a different number of rows"))
  
  return(result) 
}
