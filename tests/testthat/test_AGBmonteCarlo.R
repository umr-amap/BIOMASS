data("KarnatakaForest")
data("NouraguesHD")

KarnatakaForest = KarnatakaForest[1:100,]

HDmodel<-modelHD(D=NouraguesHD$D,
                 H=NouraguesHD$H,
                 method="log2",
                 useWeight =TRUE)

D = KarnatakaForest$D
coord = cbind(KarnatakaForest$long, KarnatakaForest$lat)
H = predictHeight(D, HDmodel)


taxo = correctTaxo(KarnatakaForest$genus, KarnatakaForest$species)
WD = getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected, verbose = F)

context("AGBmonteCarlo")
test_that("AGBmonteCarlo error",{
  expect_error(AGBmonteCarlo(D), "The WD and errWD arguments must be not NULL")
  
  expect_error( AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD), 
                "Input missing, you need to provide one of the following arguments" )
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H), 
               "Cannot propagate height errors without information on associated errors")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, n = 1500), 
               "n cannot be smaller than 50 or larger than 1000")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, n = 20), 
               "n cannot be smaller than 50 or larger than 1000")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, Dpropag = D[1:50]), 
               "Dpropag should be set to one of these options")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD[1], H = H), 
               "Your wood density vector")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, coord = coord), 
               "Too many input")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, coord = coord), 
               "Too many input")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, coord = coord[1:50,]), 
               "coord should be either")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, coord = coord, H = H, errH = 0), 
               "Too many input")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, coord = coord, HDmodel = HDmodel), 
               "Too many input")
  
  expect_error(AGBmonteCarlo(D, WD = WD$meanWD, errWD = WD$sdWD, H = H, errH = 0, HDmodel = HDmodel), 
               "Too many input")
})


test_that("AGB monte Carlo on the HDmodel", {
  AGB = AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel, n = 500)
  expect_length(AGB, 5)
  
  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")
  
  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")
  
  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")
  
  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")
  
  expect_equal(dim(AGB$AGB_simu), c(100, 500))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1,1], "numeric")
})


test_that("AGB monte Carlo on the H", {
  AGB = AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, H = H, errH = HDmodel$RSE, n = 500)
  expect_length(AGB, 5)
  
  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")
  
  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")
  
  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")
  
  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")
  
  expect_equal(dim(AGB$AGB_simu), c(100, 500))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1,1], "numeric")
})


test_that("AGB monte Carlo on the coord", {
  AGB = AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, coord = coord, n = 500)
  expect_length(AGB, 5)
  
  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")
  
  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")
  
  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")
  
  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")
  
  expect_equal(dim(AGB$AGB_simu), c(100, 500))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1,1], "numeric")
  
  expect_is(AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, coord = coord[1,], n = 500), 'list')
})

test_that("AGB monte Carlo on the Dpropag", {
  AGB = AGBmonteCarlo(D, Dpropag = rnorm(length(D), mean = mean(D), sd = 0.1), 
                      WD = WD$meanWD, errWD = WD$sdWD, coord = coord, n = 500)
  expect_length(AGB, 5)
  
  expect_length(AGB$meanAGB, 1)
  expect_is(AGB$meanAGB, "numeric")
  
  expect_length(AGB$medAGB, 1)
  expect_is(AGB$medAGB, "numeric")
  
  expect_length(AGB$sdAGB, 1)
  expect_is(AGB$sdAGB, "numeric")
  
  expect_length(AGB$credibilityAGB, 2)
  expect_is(AGB$credibilityAGB, "numeric")
  
  expect_equal(dim(AGB$AGB_simu), c(100, 500))
  expect_is(AGB$AGB_simu, "matrix")
  expect_is(AGB$AGB_simu[1,1], "numeric")
})


test_that("AGB monte Carlo on the Carbon", {
  AGB = AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, coord = coord, n = 500, Carbon = T)
  expect_length(AGB, 5)
  
  expect_length(AGB$meanAGC, 1)
  expect_is(AGB$meanAGC, "numeric")
  
  expect_length(AGB$medAGC, 1)
  expect_is(AGB$medAGC, "numeric")
  
  expect_length(AGB$sdAGC, 1)
  expect_is(AGB$sdAGC, "numeric")
  
  expect_length(AGB$credibilityAGC, 2)
  expect_is(AGB$credibilityAGC, "numeric")
  
  expect_equal(dim(AGB$AGC_simu), c(100, 500))
  expect_is(AGB$AGC_simu, "matrix")
  expect_is(AGB$AGC_simu[1,1], "numeric")
})

test_that("AGB monte Carlo on the Dlim", {
  AGB = AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, coord = coord, n = 500, Carbon = T, Dlim = 20)
  expect_length(AGB, 5)
  
  expect_length(AGB$meanAGC, 1)
  expect_is(AGB$meanAGC, "numeric")
  
  expect_length(AGB$medAGC, 1)
  expect_is(AGB$medAGC, "numeric")
  
  expect_length(AGB$sdAGC, 1)
  expect_is(AGB$sdAGC, "numeric")
  
  expect_length(AGB$credibilityAGC, 2)
  expect_is(AGB$credibilityAGC, "numeric")
  
  expect_equal(dim(AGB$AGC_simu), c(100, 500))
  expect_is(AGB$AGC_simu, "matrix")
  expect_is(AGB$AGC_simu[1,1], "numeric")
  
  expect_equal( unique( which(AGB$AGC_simu == 0, arr.ind = T)[,1] ), which(D<20) )
})
