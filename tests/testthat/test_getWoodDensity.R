data("KarnatakaForest")

KarnatakaForest <- KarnatakaForest[1:100, ]



context("Function to get the wood density")
test_that("Function getWoodDensity", {
  taxo <- correctTaxo(KarnatakaForest$genus, KarnatakaForest$species)
  taxo$family <- getTaxonomy(taxo$genusCorrected)$family

  expect_error(
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected[1:10])),
    "don't have the same length"
  )

  WD <- suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected))
  expect_is(WD, "data.frame")

  expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
  expect_equal(dim(WD), c(100, 7))

  WD <- suppressMessages(getWoodDensity(genus = taxo$genusCorr, species = taxo$speciesCorr, stand = KarnatakaForest$plotId))
  expect_is(WD, "data.frame")

  expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
  expect_equal(dim(WD), c(100, 7))
  WD_Z <- WD[WD$genus == "Ziziphus" & WD$species == "oenopolia", "meanWD"][1]

  LocalWoodDensity <- data.frame(
    genus = c("Ziziphus", "Terminalia", "Garcinia"),
    species = c("oenopolia", "bellirica", "indica"),
    wd = c(0.65, 0.72, 0.65)
  )
  WD <- getWoodDensity(
    genus = taxo$genusCorr, species = taxo$speciesCorr,
    stand = KarnatakaForest$plotId, addWoodDensityData = LocalWoodDensity
  )
  expect_is(WD, "data.frame")

  expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
  expect_equal(dim(WD), c(100, 7))

  expect_false(WD[WD$genus == "Ziziphus" & WD$species == "oenopolia", "meanWD"][1] == WD_Z)

  names(LocalWoodDensity) <- c("genus", "species", "woodDensity")
  expect_error(
    suppressMessages(getWoodDensity(
      genus = taxo$genusCorr, species = taxo$speciesCorr,
      stand = KarnatakaForest$plotId, addWoodDensityData = LocalWoodDensity
    )),
    "The additional wood density database should be organized in a dataframe with three"
  )
})


test_that("Function getWoodDensity with other parameters", {
  taxo <- correctTaxo(KarnatakaForest$genus, KarnatakaForest$species)
  taxo$family <- getTaxonomy(taxo$genusCorrected)$family

  expect_error(
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, region = "feqz")),
    "One of the region you entered is not recognized in the global wood density database"
  )

  expect_true(any(is.na(suppressMessages(getWoodDensity(taxo$genusCorrected,
    taxo$speciesCorrected,
    region = "SouthAmericaTrop"
  )$family))))

  expect_error(
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, family = taxo$family[1:10])),
    "same length"
  )

  expect_failure(expect_equal(
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, family = taxo$family)),
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected))
  ))

  expect_error(
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, stand = KarnatakaForest$plotId[1:10])),
    "same length"
  )

  expect_failure(expect_equal(
    suppressMessages(getWoodDensity(taxo$genusCorrected,
      taxo$speciesCorrected,
      stand = KarnatakaForest$plotId
    )),
    suppressMessages(getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected))
  ))
})
