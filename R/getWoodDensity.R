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
