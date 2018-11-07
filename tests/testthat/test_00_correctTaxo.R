library(BIOMASS)

data("KarnatakaForest")
genus <- KarnatakaForest$genus[1:10]
species <- KarnatakaForest$species[1:10]


context("CorrectTaxo")
test_that("CorrectTaxo", {
  skip_if_not_installed("httr")
  
  expect_is(correctTaxo(genus, species), "data.frame")

  taxo <- correctTaxo(genus, species)
  taxoFalse <- taxo[taxo$nameModified == FALSE, ]
  taxoBegin <- as.data.frame(cbind(as.character(genus), as.character(species)))

  expect_equivalent(taxoFalse[, 1:2], taxoBegin[taxo$nameModified == FALSE, ])
  expect_error(correctTaxo(genus, species[1:9]), "You should provide two vectors of genus and species of the same length")

  expect_equal(correctTaxo(paste(genus, species)), taxo)
  expect_error(correctTaxo(genus = rep(NA, 20), species = rep(NA, 20)), "Please supply at least one name")
  expect_error(correctTaxo(genus = rep(NA, 20)), "Please supply at least one name")

  expect_equal(
    correctTaxo(genus = c("Magnophyton", "?"), species = c("fulvum", "?")),
    data.frame(
      genusCorrected = c("Manniophyton", "?"),
      speciesCorrected = c("fulvum", "?"),
      nameModified = c("TRUE", "NoMatch(low_score)")
    )
  )
  path <- folderControl(correctTaxo = T)
  expect_true(dir.exists(rappdirs::user_data_dir("BIOMASS")))
  expect_true(file.exists(path))

  a <- read.table(path, header = T, sep = ",")
  expect_equal(names(a), c("query", "outName", "nameModified", "score1"))
  expect_gt(nrow(a), 0)

  expect_equal(
    correctTaxo(genus = c("Magnophyton", "?"), species = c("fulvum", "?"), score = 0.97),
    data.frame(
      genusCorrected = c("Magnophyton", "?"),
      speciesCorrected = c("fulvum", "?"),
      nameModified = c("NoMatch(low_score)", "NoMatch(low_score)")
    )
  )
})
