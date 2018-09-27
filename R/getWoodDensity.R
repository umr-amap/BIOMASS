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
#' 
getWoodDensity <- function(genus, species, stand = NULL, family = NULL, region = "World", 
                           addWoodDensityData = NULL)
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
  
  subWdData <- wdData
  if(region != "World") 
    subWdData <- wdData[wdData$regionId == region,] 
  
  if(nrow(subWdData) == 0) 
    stop("The region you entered is not recognized in the global wood density database")
  
  if(nrow(subWdData)<1000 & is.null(addWoodDensityData)) 
  {
    cat(paste("DRYAD data only stored", nrow(subWdData), "wood density values in your region of interest. 
              Would you want to try with the 'World' option ? (y/n)"))
    ans <- scan(what = character(), nmax = 1, quiet = TRUE)
    
    # Set the world option instead
    if(ans == "y")
      subWdData <- wdData 
  }
  
  subWdData <- subWdData[, c("family", "genus", "species", "wd")]
  
  if(!is.null(addWoodDensityData))
  {
    if(any(names(addWoodDensityData) != c("genus","species","wd")))
      stop("The additional wood density database should be organized in a dataframe with three columns: 
           \"genus\",\"species\",\"wd\" (column order and names should be respected)")
    addWoodDensityData$family <- getTaxonomy(addWoodDensityData$genus)$family
    addWoodDensityData <- addWoodDensityData[,c("family","genus","species","wd")]
    addWoodDensityData <- addWoodDensityData[!is.na(addWoodDensityData$wd),]
    subWdData <- rbind(subWdData, addWoodDensityData)
  }
  
  cat(paste("The reference dataset contains",nrow(subWdData), "wood density values \n"))
  
  # Creating an input dataframe
  inputData <- data.frame(genus = as.character(genus), species = as.character(species), stringsAsFactors = FALSE)
  
  if(!is.null(family))
  {
    if(length(genus) == length(family))
      inputData$family <- as.character(family)
    else
      stop("Your family vector and your genus/species vectors don't have the same length")
  }
  
  taxa <- unique(inputData)
  cat(paste("Your taxonomic table contains", nrow(taxa), "taxa \n"))
  
  if(!is.null(stand))
  {
    if(length(genus) == length(stand))
      inputData$stand <- as.character(stand)
    else
      stop("Your stand vector and your genus/species vectors don't have the same length")
  }
  
  inputData$id <- 1:length(species)
  
  # Select only the relevant data
  selectData <- subWdData[(subWdData$family %in% taxa$family | subWdData$genus %in% taxa$genus | subWdData$species %in% taxa$species),]
  aggregatedName <- paste(selectData$family, selectData$genus, selectData$species, sep = "_")
  selectData <- data.frame(selectData, aggregatedName)
  
  # Compute the mean at species, genus (and family) level
  # -- Species 
  allMeanSp <- unique(selectData[,c("family","genus","species","aggregatedName")])
  allMeanSp <- allMeanSp[order(allMeanSp$aggregatedName),]
  allMeanSp$meanWDsp <- tapply(selectData$wd, selectData$aggregatedName, mean) # Vector for Wood Specific Gravity measures
  allMeanSp$nIndSp <- tapply(selectData$wd, selectData$aggregatedName, length) # Vector for number of Wood Specific Gravity measures
  
  # -- Genus
  allMeanGen <- unique(selectData[,c("family","genus")])
  allMeanGen <- allMeanGen[order(allMeanGen$genus),]
  allMeanGen$meanWDgen <- tapply(allMeanSp$meanWDsp, allMeanSp$genus, mean)
  allMeanGen$nIndGen <- tapply(allMeanSp$meanWDsp, allMeanSp$genus, length)
  
  # Combine original datasets with mean WD at genus and species level
  meanData <- merge(inputData, allMeanGen, all.x = TRUE)
  meanData <- merge(meanData, allMeanSp, all.x = TRUE)
  
  #### Create the final WD estimates
  meanData[, c("meanWD", "nInd", "sdWD", "levelWD")] <- NA
  
  ###################################
  # At the species level
  filter <- !is.na(meanData$meanWDsp)
  meanData$meanWD[filter] <- meanData$meanWDsp[filter]
  meanData$nInd[filter] <- meanData$nIndSp[filter]
  meanData$levelWD[filter] <- "species"   
  meanData$sdWD[filter] <- sd_10$sd[sd_10$taxo == "species"]
  
  ###################################
  # At the genus level
  filter <- (is.na(meanData$meanWD) & !is.na(meanData$meanWDgen))
  meanData$meanWD[filter] <- meanData$meanWDgen[filter]
  meanData$nInd[filter] <- meanData$nIndGen[filter]
  meanData$levelWD[filter] <- "genus"  
  meanData$sdWD[filter] <- sd_10$sd[sd_10$taxo == "genus"]
  
  # If the family is provided, compute the wood density at the family level
  if(!is.null(family))
  {
    allMeanFam <- unique(selectData[, c("family")])
    allMeanFam <- data.frame(family = sort(allMeanFam))
    allMeanFam$meanWDfam <- tapply(allMeanGen$meanWDgen, allMeanGen$family, mean)
    allMeanFam$nIndFam <- tapply(allMeanGen$meanWDgen, allMeanGen$family, length)
    allMeanFam$sdWdfam <- tapply(allMeanGen$meanWDgen, allMeanGen$family, sd)
    
    meanData <- merge(meanData, allMeanFam, all.x = TRUE)
    
    ###################################
    # At the family level
    filter <- (is.na(meanData$meanWD) & !is.na(meanData$meanWDfam))    
    meanData$meanWD[filter] <- meanData$meanWDfam[filter]
    meanData$nInd[filter] <- meanData$nIndFam[filter]
    meanData$levelWD[filter] <- "family"  
    meanData$sdWD[filter] <- sd_10$sd[sd_10$taxo == "family"]
    
  }
  
  # If the stand is provided, compute the wood density at the stand level  
  if(!is.null(stand))
  {
    standWD <- data.frame(meanStand = tapply(meanData$meanWD, meanData$stand, mean, na.rm = TRUE),
                          sdStand = tapply(meanData$meanWD, meanData$stand, sd, na.rm = TRUE),
                          nIndStand = tapply(meanData$meanWD, meanData$stand, function(x) length(x[!is.na(x)])))
    
    meanData <- merge(meanData, standWD, by.x = "stand", by.y = "row.names", all.x = T)
    filter <- is.na(meanData$meanWD)
    meanData$meanWD[filter] <- meanData$meanStand[filter]
    meanData$nInd[filter] <- meanData$nIndStand[filter]
    meanData$sdWD[filter] <- meanData$sdStand[filter]
    meanData$levelWD[filter] <- meanData$stand[filter]
    
  }
  
  # If some values are still NA, set the Wood density of the whole dataset
  filter <- is.na(meanData$meanWD)
  meanData$meanWD[filter] <- mean(meanData$meanWD, na.rm = TRUE)
  meanData$nInd[filter] <- sum(!filter)
  meanData$sdWD[filter] <- sd(meanData$meanWD, na.rm = TRUE)
  meanData$levelWD[filter] <- "dataset"
  
  meanData <- meanData[with(meanData, order(meanData$id)), ]
  result <- meanData[, c("family", "genus", "species", "meanWD", "sdWD", "levelWD" ,"nInd")] 
  
  if(nrow(result) != nrow(inputData)) 
    warning(paste("The input and the output tables have a different number of rows"))
  
  return(result) 
}
