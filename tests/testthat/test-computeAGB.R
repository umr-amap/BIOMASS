
test_that("ComputeAGB", {
  
  data("NouraguesTrees")
  data("NouraguesHD")
  data("NouraguesCoords")
  coord <- apply(NouraguesCoords[c("Long","Lat")] , 2, mean) # compute the mean of the corner coordinates
  
  NouraguesTrees <- NouraguesTrees[1:100, ]
  
  D <- NouraguesTrees$D
  
  WD <- suppressMessages(suppressWarnings(getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species)))
  
  H <- retrieveH(D, model = modelHD(NouraguesHD$D, NouraguesHD$H, method = "log2", useWeight = TRUE, , bayesian = FALSE))
  
  # computeAGB() returns a list with one element per allometry
  expect_error(computeAGB(D = D, WD = WD$meanWD[1:65]), "different lengths")
  expect_error(computeAGB(D = D, WD = WD$meanWD), "You need to provide either H or coord")
  expect_type(computeAGB(D = D, WD = WD$meanWD, H = H$H), "list")
  expect_named(computeAGB(D = D, WD = WD$meanWD, H = H$H), "chave2014")
  expect_length(computeAGB(D = D, WD = WD$meanWD, H = H$H)[[1]], 100)
  expect_error(computeAGB(D = D, WD = WD$meanWD, H = H$H[1:50]), "H and D have different lengths")
  expect_error(computeAGB(D = D, WD = WD$meanWD, H = H$H, coord = coord), "Both height and coordinates are provided.")
  expect_error(computeAGB(D = D, WD = WD$meanWD, coord = "blop"), "coord should be either")
  expect_error(computeAGB(allometry = "chave2014", D = D, WD = WD$meanWD, H = H$H), "'allometry' must be a function or a list of functions")

  H1 <- H$H
  H1[1] <- NA
  expect_warning(
    computeAGB(D = D, WD = WD$meanWD, H = H1),
    "NA values"
  )

  D1 <- D
  D1[1] <- NA
  expect_warning(
    computeAGB(D = D1, WD = WD$meanWD, H = H1),
    "NA values in D"
  )
  
  # ComputeAGB with Dlim
  expect_true(any(computeAGB(D = D, WD = WD$meanWD, H = H$H, Dlim = 15)[[1]] == 0))

  # Custom allometry, and a list of allometries
  my_allom <- function(D, WD, ...) 0.1 * WD * D^2.5 / 1000
  expect_equal(computeAGB(allometry = my_allom, D = D, WD = WD$meanWD)[[1]], my_allom(D, WD$meanWD))
  expect_length(computeAGB(allometry = list(chave2014 = chave2014, my_allom = my_allom), D = D, WD = WD$meanWD, H = H$H), 2)
  
  # ComputeAGB with coord
  skip_on_cran()
  expect_length(computeAGB(D = D, WD = WD$meanWD, coord = coord)[[1]], 100)
  expect_error(computeAGB(D = D, WD = WD$meanWD, coord = rbind(coord,coord)), "coord should be either")
})


