test <- fread("family       genus        species        plotId
Fabaceae      Acacia        sinuata        plot1
Cornaceae    Alangium   salviifolium        plot1
Fabaceae     Albizia        lebbeck        plot2
Fabaceae     Albizia        procera        plot2
Sapindaceae  Allophylus          cobbe        plot2
Lauraceae Alseodaphne semecarpifolia        plot1
Phyllanthaceae     Aporosa     lindleyana        plot1", data.table = F)

suppressMessages({
  context("Function to get the wood density")
  test_that("Function getWoodDensity", {
    taxo <- correctTaxo(test$genus, test$species)
    taxo$family <- getTaxonomy(taxo$genusCorrected)$family

    expect_error(
      getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected[1:10]),
      "do not have the same length"
    )

    WD <- getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected)
    expect_is(WD, "data.frame")

    expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
    expect_equal(dim(WD), c(7, 7))

    WD <- getWoodDensity(genus = taxo$genusCorr, species = taxo$speciesCorr, stand = test$plotId)
    expect_is(WD, "data.frame")

    expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
    expect_equal(dim(WD), c(7, 7))



    LocalWoodDensity <- data.table(
      family = test$family,
      genus = test$genus,
      species = test$species,
      wd = runif(nrow(test), min = 0.1, max = 1.5)
    )


    WD <- getWoodDensity(
      genus = taxo$genusCorr, species = taxo$speciesCorr,
      stand = test$plotId, addWoodDensityData = LocalWoodDensity, region = "Europe"
    )
    expect_is(WD, "data.frame")

    expect_equal(colnames(WD), c("family", "genus", "species", "meanWD", "sdWD", "levelWD", "nInd"))
    expect_equal(dim(WD), c(7, 7))

    expect_equivalent(WD[, c("family", "genus", "species", "meanWD")], setDF(LocalWoodDensity))

    names(LocalWoodDensity) <- c("genus", "species", "woodDensity")
    expect_error(
      getWoodDensity(
        genus = taxo$genusCorr, species = taxo$speciesCorr,
        stand = test$plotId, addWoodDensityData = LocalWoodDensity
      ),
      "The additional wood density database should be organized in a dataframe with three"
    )
  })


  test_that("Function getWoodDensity with other parameters", {
    taxo <- correctTaxo(test$genus, test$species)
    taxo$family <- getTaxonomy(taxo$genusCorrected)$family

    expect_error(
      getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, region = "feqz"),
      "One of the region you entered is not recognized in the global wood density database"
    )

    suppressWarnings({
      # expect an error if there is no genus and species to match in the smaller table
      expect_error(getWoodDensity(taxo$genusCorrected,
        taxo$speciesCorrected,
        region = "Europe"
      ), "exact match among the family")

      # but if there is family who correspond to some family in the smaller table then it's pass
      expect_failure(expect_error(
        getWoodDensity(taxo$genusCorrected,
          taxo$speciesCorrected,
          region = "Europe", family = taxo$family
        )
      ))

      # if there isn't any match in the smaller table
      expect_error(
        getWoodDensity(family = "Fabaceae", genus = "Abarema", species = "jupunba", region = "Europe"),
        "database"
      )
    })

    # If there is just the genus who correspond to the database
    expect_equal(unique(getWoodDensity(taxo$genusCorrected, letters[seq(nrow(taxo))])$levelWD), "genus")

    # If there is just the species who correspond to the database
    expect_error(
      getWoodDensity(letters[seq(nrow(taxo))], species = taxo$speciesCorrected),
      "exact match among the family"
    )

    # If the genus and the species correspond to the database but not the family
    expect_error(
      getWoodDensity(genus = taxo$genusCorrected, species = taxo$speciesCorrected, family = letters[seq(nrow(taxo))]),
      "exact match among the family"
    )

    expect_error(
      getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, family = taxo$family[1:10]),
      "same length"
    )

    # comparison of the result of if there is just the family who correspond
    expect_failure(expect_equal(
      getWoodDensity(c(taxo$genusCorrected, "a"), c(taxo$speciesCorrected, "a"), family = c(taxo$family, "Fabaceae")),
      getWoodDensity(c(taxo$genusCorrected, "a"), c(taxo$speciesCorrected, "a"))
    ))

    expect_error(
      getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, stand = test$plotId[1:2]),
      "same length"
    )


    expect_failure(expect_equal(
      getWoodDensity(c(taxo$genusCorrected, "a"), c(taxo$speciesCorrected, "a"), stand = c(test$plotId, "plot1")),
      getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected)
    ))
  })
})
