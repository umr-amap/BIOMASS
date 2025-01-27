context("CorrectTaxo")

test_that("CorrectTaxo", {

  skip_on_cran() #we do not want any query to tnrs when cran is testing
  
  data("NouraguesTrees")
  NouraguesTrees <- NouraguesTrees[1:100,]
  genus <- NouraguesTrees$Genus
  species <- NouraguesTrees$Species
  
  taxo <- correctTaxo(genus, species)
  taxo_pasted <- correctTaxo(paste(genus, species))
  
  expect_is(taxo, "data.frame")

  taxoFalse <- taxo[taxo$nameModified == FALSE, ]
  taxoBegin <- data.frame(as.character(genus), as.character(species), stringsAsFactors = FALSE)

  expect_equivalent(taxoFalse[, 1:2], taxoBegin[taxo$nameModified == FALSE, ])
  expect_error(correctTaxo(genus, species[1:9]), "You should provide two vectors of genera and species of the same length")

  expect_equal(taxo,taxo_pasted)
  expect_error(correctTaxo(genus = rep(NA, 20), species = rep(NA, 20)), "Please supply at least one name")
  expect_error(correctTaxo(genus = rep(NA, 20)), "Please supply at least one name")
  expect_equal(
    correctTaxo(genus = c(NA, "Astrocarium", "Astrocarium","Magnophyton","?"), species = c("lalal", NA, "standleanum","fulvum", "?")),
    data.frame(
      genusCorrected = c(NA, "Astrocaryum", "Astrocaryum","Manniophyton", "?"),
      speciesCorrected = c(NA, NA, "standleyanum","fulvum", "?"),
      nameModified = c(NA, "TRUE", "TRUE","TRUE", "TaxaNotFound"), stringsAsFactors = FALSE
    )
  )

  # path <- cacheManager("correctTaxo.log")
  # expect_true(file.exists(path))
  # 
  # a <- setDF(fread(path, header = TRUE, sep = ","))
  # expect_equal(names(a), c("submittedName", "score", "matchedName", "acceptedName"))
  # expect_gt(nrow(a), 0)

  # file.rename(path, paste0(path, 0))
  
  # test multiple things here :
  #   1) if the useCache is NULL but there is no Cache to remove
  #   2) if the genus is nonsense, there is no error message that appear if there is no cache
  #   3) The connectivity
  expect_equal(
    correctTaxo(genus = "bvgaeuigareuiguei", useCache = NULL),
    data.frame(
      genusCorrected = "bvgaeuigareuiguei",
      speciesCorrected = NA_character_,
      nameModified = "TaxaNotFound", stringsAsFactors = FALSE
    )
  )
  # If there is no cache and the genus is nonsense a second that come on sense
  expect_equal(
    correctTaxo(genus = c("bvgaeuigareuiguei", "Astrocarium standleanum"), useCache = NULL),
    data.frame(
      genusCorrected = c("bvgaeuigareuiguei", "Astrocaryum"),
      speciesCorrected = c(NA_character_, "standleyanum"),
      nameModified = c("TaxaNotFound", TRUE), stringsAsFactors = FALSE
    )
  )
  #file.remove(path)
  #file.rename(paste0(path, 0), path)
})
