data(NouraguesHD)
data(KarnatakaForest)

# Modelling height-diameter relationship
HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")

# Retrieving wood density values
KarnatakaWD <- suppressMessages(getWoodDensity(KarnatakaForest$genus, KarnatakaForest$species,
  stand = KarnatakaForest$plotId
))

# Propagating errors with a standard error in wood density in one plot
filt <- KarnatakaForest$plotId %in% c("BSP20", "BSP14")
resultMC <- AGBmonteCarlo(
  D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt],
  errWD = KarnatakaWD$sdWD[filt], HDmodel = HDmodel
)

plot <- KarnatakaForest$plotId[ filt ]

context("summary by plot")
test_that("summary by plot", {
  sum <- summaryByPlot(resultMC$AGB_simu, plot)
  expect_equal(sum, summaryByPlot(resultMC, plot))

  expect_is(sum, "data.frame")
  expect_equal(nrow(sum), length(unique(plot)))
  expect_equal(ncol(sum), 4)
  expect_equal(colnames(sum), c("plot", "AGB", "Cred_2.5", "Cred_97.5"))

  plot[ sample(1:length(plot), 100) ] <- NA
  expect_failure(expect_equal(sum, summaryByPlot(resultMC$AGB_simu, plot)))
})

test_that("summary by plot with the vector", {
  H <- predictHeight(D = KarnatakaForest$D[filt], model = HDmodel)
  resultAGB <- computeAGB(D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt], H = H)

  sum <- summaryByPlot(resultAGB, plot)
  expect_is(sum, "data.frame")
  expect_length(unique(plot), nrow(sum))
  expect_equal(ncol(sum), 2)

  plot[ sample(1:length(plot), 100) ] <- NA
  expect_failure(expect_equal(sum, summaryByPlot(resultAGB, plot)))
  expect_equal(nrow(summaryByPlot(resultAGB, plot)), length(unique(plot)) - 1)
})

test_that("summary by plot error", {
  expect_error(
    summaryByPlot(resultMC$AGB_simu, plot[1:10]),
    "vector"
  )
  expect_error(
    summaryByPlot(as.data.frame(resultMC$AGB_simu), plot),
    "matrix"
  )
})
