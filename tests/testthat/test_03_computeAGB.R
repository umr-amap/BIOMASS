data("KarnatakaForest")
data("NouraguesHD")

KarnatakaForest <- KarnatakaForest[1:100, ]

D <- KarnatakaForest$D
coord <- cbind(KarnatakaForest$long, KarnatakaForest$lat)

WD <- suppressMessages(getWoodDensity(KarnatakaForest$genus, KarnatakaForest$species))

H <- retrieveH(D, model = modelHD(NouraguesHD$D, NouraguesHD$H, method = "log2", useWeight = TRUE))

context("Function to compute the AGB")
test_that("ComputeAGB with H", {
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
})

# test_that("ComputeAGB with coord", {
#   expect_length(computeAGB(D, WD$meanWD, coord = coord), 100)
# 
#   # expect_silent(computeAGB(D, WD$meanWD, coord = c(74.91944, 14.36806)))
#   expect_error(computeAGB(D, WD$meanWD, coord = coord[1:50, ]), "coord should be either")
# })

test_that("ComputeAGB with Dlim", {
  expect_true(any(computeAGB(D, WD$meanWD, H = H$H, Dlim = 5) == 0))
  expect_true(any(computeAGB(D, WD$meanWD, H = H$H, Dlim = 5) > 0))
})

