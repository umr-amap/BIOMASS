context("summary by plot")

test_that("summary by plot", {
  
  data(NouraguesHD)
  data(NouraguesTrees)
  NouraguesTrees <- NouraguesTrees[c(1:100,551:650),]
  
  # Modelling height-diameter relationship
  HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
  
  # Retrieving wood density values
  NouraguesWD <- suppressMessages(
    getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species))
  
  # Propagating errors with a standard error in wood density
  resultMC <- AGBmonteCarlo(
    D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
    errWD = NouraguesWD$sdWD, HDmodel = HDmodel
  )
  
  sum <- summaryByPlot(resultMC$AGB_simu, NouraguesTrees$Plot)
  
  expect_equal(sum, summaryByPlot(resultMC, NouraguesTrees$Plot))

  expect_error(
    summaryByPlot(as.data.frame(resultMC$AGB_simu), plot),
    "The AGB_val must be a matrix you have for the result of the function 'AGBmonteCarlo', or just the result of the function."
  )
  expect_error(
    summaryByPlot(resultMC$AGB_simu, c(1,2)),
    "Your 'plot' vector have not the same length as your number of row in the matrix"
  )
  
  expect_is(sum, "data.frame")
  expect_equal(nrow(sum), length(unique(NouraguesTrees$Plot)))
  expect_equal(ncol(sum), 4)
  expect_equal(colnames(sum), c("plot", "AGB", "Cred_2.5", "Cred_97.5"))
  
  # trees without label
  plot_vec <- NouraguesTrees$Plot ; plot_vec[1] <- NA
  expect_equal(
    sum[,c(1,3,4)] ,
    summaryByPlot(AGB_val = resultMC$AGB_simu, plot = plot_vec)[,c(1,3,4)], 
    tol = 1e-3 )
  
  expect_equal(
    summaryByPlot(AGB_val = resultMC$AGB_simu, plot = NouraguesTrees$Plot),
    summaryByPlot(AGB_val = resultMC$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = T))

})
