data("KarnatakaForest")

KarnatakaForest = KarnatakaForest[1:100, ]

taxo = correctTaxo(KarnatakaForest$genus, KarnatakaForest$species)
taxo$family = getTaxonomy(taxo$genusCorrected )$family

context("Function to get the wood density")
test_that("Function getWoodDensity", {
  expect_error(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected[1:10], verbose = F), "don't have the same lenght")
  
  WD = getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, verbose = F)
  expect_is(WD, "data.frame")
  
  expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
  expect_equal(dim(WD), c(100, 7))
  
  WD = getWoodDensity(genus=taxo$genusCorr, species=taxo$speciesCorr, stand=KarnatakaForest$plotId, verbose = F)
  expect_is(WD, "data.frame")
  
  expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
  expect_equal(dim(WD), c(100, 7))
  WD_Z = WD[WD$genus == "Ziziphus" & WD$species == "oenopolia", "meanWD"][1]
  
  LocalWoodDensity<-data.frame(genus=c("Ziziphus","Terminalia","Garcinia"),
                               species=c("oenopolia","bellirica","indica"),
                               wd=c(0.65,0.72,0.65))
  WD = getWoodDensity(genus=taxo$genusCorr, species=taxo$speciesCorr, 
                      stand=KarnatakaForest$plotId, addWoodDensityData = LocalWoodDensity, verbose = F)
  expect_is(WD, "data.frame")
  
  expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
  expect_equal(dim(WD), c(100, 7))
  
  expect_false(WD[WD$genus == "Ziziphus" & WD$species == "oenopolia", "meanWD"][1] == WD_Z)
  
  names(LocalWoodDensity) = c("genus", "species", "woodDensity")
  expect_error(getWoodDensity(genus=taxo$genusCorr, species=taxo$speciesCorr, 
                              stand=KarnatakaForest$plotId, addWoodDensityData = LocalWoodDensity, verbose = F), 
               "The additional wood density database should be organized in a dataframe with three")
})


test_that("Function getWoodDensity with other parameters", {
  expect_error( getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, region = "feqz", verbose = F), 
                "The region you entered is not recognized in the global wood density database" )
  
  expect_true(any( is.na( getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, region = "SouthAmericaTrop", verbose = F)$family) ))
  
  expect_error(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, family = taxo$family[1:10], verbose = F), 
               "same length")
  
  expect_failure(expect_equal(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, family = taxo$family, verbose = F),
                              getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, verbose = F)))
  
  expect_error(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, stand = KarnatakaForest$plotId[1:10], verbose = F), 
               "same length")
  
  expect_failure(expect_equal(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, stand = KarnatakaForest$plotId, verbose = F),
                              getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, verbose = F)))
})







