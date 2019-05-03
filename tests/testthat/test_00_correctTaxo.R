# require(data.table)
# 
# data("KarnatakaForest")
# genus <- KarnatakaForest$genus[1:10]
# species <- KarnatakaForest$species[1:10]
# 
# skip_if_not_function <- function(name) {
#   if (!exists(name)) {
#     skip(paste("The function", name, "is internal"))
#   }
# }
# 
# context("CorrectTaxo")
# test_that("CorrectTaxo", {
#   skip_on_cran()
#   
#   skip_if_not_installed("httr")
#   
#   flushCache()
# 
#   expect_is(correctTaxo(genus, species), "data.frame")
# 
#   taxo <- correctTaxo(genus, species)
#   taxoFalse <- taxo[taxo$nameModified == FALSE, ]
#   taxoBegin <- data.frame(as.character(genus), as.character(species), stringsAsFactors = F)
# 
#   expect_equivalent(taxoFalse[, 1:2], taxoBegin[taxo$nameModified == FALSE, ])
#   expect_error(correctTaxo(genus, species[1:9]), "You should provide two vectors of genera and species of the same length")
# 
#   expect_equal(correctTaxo(paste(genus, species)), taxo)
#   expect_error(correctTaxo(genus = rep(NA, 20), species = rep(NA, 20)), "Please supply at least one name")
#   expect_error(correctTaxo(genus = rep(NA, 20)), "Please supply at least one name")
#   expect_equal(
#     correctTaxo(genus = c(NA, "Astrocarium", "Astrocarium"), species = c("lalal", NA, "standleanum")),
#     data.frame(
#       genusCorrected = c(NA, "Astrocaryum", "Astrocaryum"),
#       speciesCorrected = c(NA, NA, "standleyanum"),
#       nameModified = c(NA, "TRUE", "TRUE"), stringsAsFactors = F
#     )
#   )
# 
#   expect_equal(
#     correctTaxo(genus = c("Magnophyton", "?"), species = c("fulvum", "?")),
#     data.frame(
#       genusCorrected = c("Manniophyton", "?"),
#       speciesCorrected = c("fulvum", "?"),
#       nameModified = c("TRUE", "TaxaNotFound"), stringsAsFactors = F
#     )
#   )
# 
#   skip_if_not_function("cacheManager")
# 
#   path <- cacheManager("correctTaxo")
#   expect_true(dir.exists(rappdirs::user_data_dir("BIOMASS")))
#   expect_true(file.exists(path))
# 
#   a <- setDF(fread(path, header = T, sep = ","))
#   expect_equal(names(a), c("submittedName", "acceptedName", "sourceId", "score", "matchedName", "annotations", "uri"))
#   expect_gt(nrow(a), 0)
# 
#   expect_equal(
#     correctTaxo(genus = c("Magnophyton", "?"), species = c("fulvum", "?"), score = 0.97),
#     data.frame(
#       genusCorrected = c("Magnophyton", "?"),
#       speciesCorrected = c("fulvum", "?"),
#       nameModified = c("NoMatch(low_score)", "TaxaNotFound"), stringsAsFactors = F
#     )
#   )
# 
# 
#   file.rename(path, paste0(path, 0))
#   # test multiple things here :
#   #   1) if the useCache is NULL but there is no Cache to remove
#   #   2) if the genus is nonsense, there is no error message that appear if there is no cache
#   #   3) The connectivity
#   expect_equal(
#     correctTaxo(genus = "bvgaeuigareuiguei", useCache = NULL),
#     data.frame(
#       genusCorrected = "bvgaeuigareuiguei",
#       speciesCorrected = NA_character_,
#       nameModified = "TaxaNotFound", stringsAsFactors = F
#     )
#   )
#   # If there is no cache and the genus is nonsense a second that come on sense
#   expect_equal(
#     correctTaxo(genus = c("bvgaeuigareuiguei", "Astrocarium standleanum"), useCache = NULL),
#     data.frame(
#       genusCorrected = c("bvgaeuigareuiguei", "Astrocaryum"),
#       speciesCorrected = c(NA_character_, "standleyanum"),
#       nameModified = c("TaxaNotFound", TRUE), stringsAsFactors = F
#     )
#   )
#   file.remove(path)
#   file.rename(paste0(path, 0), path)
# })
