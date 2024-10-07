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

test_that("summary by plot with the vector and subplot", {
  # cut the plots in multiple part
  coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
  coord[1:4, ] <- coord[1:4, ] + 5000
  coord[5:8, ] <- coord[5:8, ] + 6000
  corner <- rep(c(1, 4, 2, 3), 2)
  cornerplot <- rep(c("plot1", "plot2"), each = 4)
  cut <- cutPlot(projCoord = coord, plot = cornerplot, corner = corner, gridsize = 100, dimX = 200, dimY = 200)
  
  # attribute the trees to the subplot
  xy <- data.frame(x = runif(10, min = 0, max = 100), y = runif(10, min = 0, max = 100))
  treeplot <- rep(c("plot1", "plot2"), e=5)
  a <- attributeTree(xy, treeplot, cut)
  # attribute an AGB to the trees
  agb <- runif(10,min=0.001,max=5)
  summary_agb <- summaryByPlot(agb, a,subplot = cut)
  
  # Tests if empty subplots have a 0 AGB value
  expect_equal(summary_agb$polygon$AGB[! summary_agb$polygon$plot %in% c("plot1_0_0","plot2_0_0")] , rep(0,6))
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
