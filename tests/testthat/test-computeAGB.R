context("Function to compute the AGB")

test_that("ComputeAGB", {
  
  data("NouraguesTrees")
  data("NouraguesHD")
  data("NouraguesCoords")
  coord <- apply(NouraguesCoords[c("Long","Lat")] , 2, mean) # compute the mean of the corner coordinates
  
  NouraguesTrees <- NouraguesTrees[1:100, ]
  
  D <- NouraguesTrees$D
  
  WD <- suppressMessages(getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species))
  
  H <- retrieveH(D, model = modelHD(NouraguesHD$D, NouraguesHD$H, method = "log2", useWeight = TRUE))
  
  expect_error(computeAGB(D, WD$meanWD[1:65]), "different lenghts")

  expect_error(computeAGB(D, WD$meanWD), "You need to provide either H or coord")

  expect_length(computeAGB(D, WD$meanWD, H = H$H), 100)

  expect_error(computeAGB(D, WD$meanWD, H$H[1:50]))

  H1 <- H$H
  H1[1] <- NA
  expect_warning(
    computeAGB(D, WD$meanWD, H = H1),
    "NA values"
  )

  D1 <- D
  D1[1] <- NA
  expect_warning(
    computeAGB(D1, WD$meanWD, H = H1),
    "NA values in D"
  )
  
  # ComputeAGB with Dlim
  expect_true(any(computeAGB(D, WD$meanWD, H = H$H, Dlim = 15) == 0))
  
  # ComputeAGB with coord
  skip_on_cran()
  expect_length(computeAGB(D, WD$meanWD, coord = coord), 100)
  expect_error(computeAGB(D, WD$meanWD, coord = rbind(coord,coord)), "coord should be either")
})


