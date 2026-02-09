test <- data.table(
  family = c("Fabaceae","Cornaceae","Fabaceae","Fabaceae","Sapindaceae","Lauraceae","Phyllanthaceae","Lecythidaceae","Indet"),
  genus = c("Acacia","Alangium","Albizia","Albizia","Allophylus","Alseodaphne","Aporosa","Indet.Lecythidaceae",NA),
  species = c("sinuata","salviifolium","lebbeck","procera","cobbe","semecarpifolia","lindleyana","Indet.",NA),
  plotId = c("plot1","plot1","plot2","plot2","plot2","plot1","plot1","plot1","plot1")
)

context("Function to get the wood density")
test_that("Function getWoodDensity", {
  
  # Errors & warnings
  expect_error(
    getWoodDensity(test$genus, test$species[1:10]),
    "do not have the same length"
  )
  expect_error(
    getWoodDensity(test$genus, test$species, test$family[1:10]),
    "do not have the same length"
  )
  wrong_genus <- test$genus
  wrong_genus[1] <- "Alangium"
  expect_error(
    getWoodDensity(genus = wrong_genus, species = test$species, family = test$family),
    "Some genera are in two or more families"
  )
  
  expect_warning(getWoodDensity(genus = test$genus, species = test$species) , 
                 "You may provide 'family' to match wood density estimates at family level.")
  
  
  ### Basic tests
  # With genus and species only
  WD <- suppressWarnings(getWoodDensity(genus = test$genus, species = test$species))
  expect_is(WD, "data.frame")
  expect_equal(colnames(WD), c("genus", "species", "meanWD", "sdWD", "levelWD"))
  expect_equal(dim(WD), c(9, 5))
  
  # With family
  WD <- getWoodDensity(genus = test$genus, species = test$species, family = test$family)
  expect_is(WD, "data.frame")
  expect_equal(colnames(WD), c("family","genus", "species", "meanWD", "sdWD", "levelWD"))
  expect_equal(dim(WD), c(9, 6))
  
  # With stand
  WD <- suppressWarnings(getWoodDensity(genus = test$genus, species = test$species, stand = test$plotId))
  expect_is(WD, "data.frame")
  expect_equal(colnames(WD), c("genus", "species", "stand", "meanWD", "sdWD", "levelWD"))
  expect_equal(dim(WD), c(9, 6))
  
  # With family and stand
  WD <- getWoodDensity(genus = test$genus, species = test$species, family = test$family, stand = test$plotId)
  expect_is(WD, "data.frame")
  expect_equal(colnames(WD), c("family", "genus", "species", "stand", "meanWD", "sdWD", "levelWD"))
  expect_equal(dim(WD), c(9, 7))
  
  
  # Test addWoodDensityData
  LocalWoodDensity <- data.table(
    family = test$family,
    genus = test$genus,
    species = test$species,
    meanWD = runif(nrow(test), min = 0.1, max = 1.5),
    sdWD = runif(nrow(test), min = 0.02, max = 0.2)
  )
  LocalWoodDensity <- LocalWoodDensity[-9,]
  
  WD <- getWoodDensity(
    family = test$family, genus = test$genus, species = test$species, addWoodDensityData = LocalWoodDensity,
  )
  expect_equal(colnames(WD), c("family","genus", "species", "meanWD", "sdWD", "levelWD"))
  expect_equivalent(WD[-9, c("family","genus", "species", "meanWD","sdWD")], setDF(LocalWoodDensity))
  
  names(LocalWoodDensity) <- c("genus", "species", "woodDensity")
  expect_error(
    getWoodDensity(
      genus = test$genus, species = test$species,
      stand = test$plotId, addWoodDensityData = LocalWoodDensity
    ),
    "The additional wood density database should be organized in a dataframe with four"
  )
})
