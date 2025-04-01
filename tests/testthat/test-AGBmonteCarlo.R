data("NouraguesTrees")
data("NouraguesHD")

NouraguesTrees <- NouraguesTrees[1:100, ]

HDmodel <- modelHD(
  D = NouraguesHD$D,
  H = NouraguesHD$H,
  method = "log2",
  useWeight = TRUE
)

nIter <- 50

D <- NouraguesTrees$D

data("NouraguesCoords")
coord <- apply(NouraguesCoords[c("Long","Lat")] , 2, mean) # compute the mean of the corner coordinates

H <- predictHeight(D, HDmodel)

WD <- suppressMessages(getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species))

context("AGBmonteCarlo")
test_that("AGBmonteCarlo error", {
  
  expect_error(AGBmonteCarlo(D), "The WD and errWD arguments must be not NULL")

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD),
    "Input missing, you need to provide one of the following arguments"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H),
    "Cannot propagate height errors without information on associated errors"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, n = 1500),
    "n cannot be smaller than 50 or larger than 1000"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, n = 20),
    "n cannot be smaller than 50 or larger than 1000"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, Dpropag = D[1:50]),
    "Dpropag should be set to one of these options"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD[1], H = H),
    "One of vector WD or errWD does not have the same length as D"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, coord = coord),
    "Too many input"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, coord = coord),
    "Too many input"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, coord = rbind(coord, coord)),
    "coord should be either"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, coord = coord, H = H, errH = 0),
    "Too many input"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, coord = coord, HDmodel = HDmodel),
    "Too many input"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, errH = 0, HDmodel = HDmodel),
    "Too many input"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, errH = seq(1, 10)),
    "H must be the same length as D and errH must be either one value or the same length as D"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H[1:10], errH = seq(1, 10)),
    "H must be the same length as D and errH must be either one value or the same length as D"
  )

  expect_error(
    AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H[1:10], errH = 0),
    "H must be the same length as D and errH must be either one value or the same length as D"
  )
})


test_that("AGB monte Carlo with HDmodel", {
  set.seed(10)
  AGB <- AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, n = nIter)
  expect_length(AGB, 5)

  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")

  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")

  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")

  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")

  expect_equal(dim(AGB$AGB_simu), c(length(D), nIter))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1, 1], "numeric")
})


test_that("AGB monte Carlo with H", {
  set.seed(10)
  AGB <- AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, H = H, errH = HDmodel$RSE, n = nIter)
  expect_length(AGB, 5)

  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")

  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")

  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")

  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")

  expect_equal(dim(AGB$AGB_simu), c(length(D), nIter))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1, 1], "numeric")
})


test_that("AGB monte Carlo with coord", {
  skip_on_cran()
  set.seed(10)
  AGB <- AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, coord = coord, n = nIter)
  expect_length(AGB, 5)

  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")

  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")

  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")

  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")

  expect_equal(dim(AGB$AGB_simu), c(length(D), nIter))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1, 1], "numeric")

})

test_that("AGBMonteCarlo with Dpropag", {
  set.seed(10)
  AGB <- AGBmonteCarlo(D,
    Dpropag = rnorm(length(D), mean = mean(D), sd = 0.1),
    WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, n = nIter
  )
  expect_length(AGB, 5)

  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")

  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")

  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")

  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")

  expect_equal(dim(AGB$AGB_simu), c(length(D), nIter))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1, 1], "numeric")
})


test_that("AGB monte Carlo with Carbon", {
  
  set.seed(10)
  AGB <- AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, n = nIter, Carbon = TRUE)
  expect_length(AGB, 5)

  expect_length(AGB$meanAGC, 1)
  expect_is(AGB$meanAGC, "numeric")

  expect_length(AGB$medAGC, 1)
  expect_is(AGB$medAGC, "numeric")

  expect_length(AGB$sdAGC, 1)
  expect_is(AGB$sdAGC, "numeric")

  expect_length(AGB$credibilityAGC, 2)
  expect_is(AGB$credibilityAGC, "numeric")

  expect_equal(dim(AGB$AGC_simu), c(length(D), nIter))
  expect_is(AGB$AGC_simu, "matrix")
  expect_is(AGB$AGC_simu[1, 1], "numeric")
})

test_that("AGB monte Carlo with Dlim", {
  set.seed(10)
  AGB <- AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, n = nIter, Carbon = TRUE, Dlim = 20)
  expect_length(AGB, 5)

  expect_length(AGB$meanAGC, 1)
  expect_is(AGB$meanAGC, "numeric")

  expect_length(AGB$medAGC, 1)
  expect_is(AGB$medAGC, "numeric")

  expect_length(AGB$sdAGC, 1)
  expect_is(AGB$sdAGC, "numeric")

  expect_length(AGB$credibilityAGC, 2)
  expect_is(AGB$credibilityAGC, "numeric")

  expect_equal(dim(AGB$AGC_simu), c(length(D), nIter))
  expect_is(AGB$AGC_simu, "matrix")
  expect_is(AGB$AGC_simu[1, 1], "numeric")

  expect_equal(unique(which(AGB$AGC_simu == 0, arr.ind = TRUE)[, 1]), which(D < 20))
})


test_that("AGB with NA", {
  D[1:5] <- NA
  set.seed(10)
  AGB <- AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, n = nIter)

  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")

  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")

  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")

  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")

  expect_equal(dim(AGB$AGB_simu), c(length(D), nIter))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1, 1], "numeric")

  expect_true(all(is.na(AGB$AGB_simu[1:5, ])))
  expect_false(all(is.na(AGB$AGB_simu[1:6, ])))
})


test_that("With the plot value", {
  HDmodel <- modelHD(
    D = NouraguesHD$D,
    H = NouraguesHD$H,
    method = "log2",
    useWeight = TRUE,
    plot = NouraguesHD$plotId
  )

  WD <- suppressMessages(getWoodDensity(NouraguesHD$genus, NouraguesHD$species))
  D <- NouraguesHD$D

  set.seed(2)
  AGB <- AGBmonteCarlo(D, WD$meanWD, WD$sdWD, HDmodel = HDmodel, n = nIter, plot = NouraguesHD$plotId)
  expect_equal(dim(AGB$AGB_simu), c(length(D), nIter))

  expect_error(AGBmonteCarlo(D, WD$meanWD, WD$sdWD, HDmodel = HDmodel, n = nIter, plot = "Plot1"), "The 'model' argument contains the following stand specific HD models which are not present in the 'plot' argument: Plot2")

})
