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
WD = getWoodDensity(taxo$genusCorrected, taxo$speciesCorrected)

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


test_that("AGB monte Carlo on the H", {
  AGB = AGBmonteCarlo(D, Dpropag = "chave2004", WD = WD$meanWD, errWD = WD$sdWD, HDmodel = HDmodel)
  
  
})


