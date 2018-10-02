options(stringsAsFactors = FALSE)

data("KarnatakaForest")
data("NouraguesHD")
HDmodel<-modelHD(D=NouraguesHD$D,
                 H=NouraguesHD$H,
                 method="log2",
                 useWeight =TRUE)

# Argument repetitive
D = KarnatakaForest$D
coord = cbind(KarnatakaForest$long, KarnatakaForest$lat)


skip_if_not_function = function(name){
  if (!exists(name))
    skip(paste("The function", name, "is internal"))
}

context("Function computeE")
test_that("Compute E", {
  skip_if_not_function("computeE")
  
  E = computeE(coord)
  
  expect_is(E, "numeric")
  expect_equal(length(E), 61965)
  
  expect_that(computeE(cbind(12, 50)), equals(1.129928, tolerance = 0.1) )
})





context("Function getBioclimParam")
test_that("getBioclimParam", {
  skip_if_not_function("getBioclimParam")
  B = getBioclimParam(coord)
  
  expect_is(B, "data.frame")
  expect_equal(dim(B), c(61965, 3))
  
  expect_equal(getBioclimParam(cbind(12, 50)), 
               data.frame("tempSeas" = 6.62375, "precSeas" = 0.01925, "CWD" = -0.0921875), 
               tolerance = 0.1 )
})





context("Function retriveH")
test_that("With the HDmodel", {
  expect_that( retrieveH(KarnatakaForest$D), throws_error("Either"))
  
  H = retrieveH(KarnatakaForest$D, model = HDmodel)
  expect_is(H, "list")
  expect_is(H$H, "numeric")
  expect_is(H$RSE, "numeric")
  expect_equal(length(H$H), 61965)
  
  expect_equal(H$RSE, 4.222, tolerance = 0.001)
  RSE = H$RSE
  
  H = retrieveH(c(2,3), model = HDmodel)
  expect_equal(H$H, c(3.968837, 5.672954), tolerance = 0.0001)
  expect_equal(H$RSE, RSE)
})


test_that("With the coordonate", {
  
  H = retrieveH(KarnatakaForest$D, coord = coord)
  
  expect_is(H, "list")
  expect_is(H$H, "AsIs")
  expect_is(H$RSE, "numeric")
  expect_equal(length(H$H), 61965)
  expect_equal(H$RSE, 0.243)
  RSE = H$RSE
  
  expect_that( retrieveH(c(2,3), coord = coord), throws_error("coord"))
  H = retrieveH(c(2,3), coord = c(70, 25))
  expect_equal(H$H, I(c(0.8351121, 1.1087833)), tolerance = 10^-6)
  expect_equal(H$RSE, RSE)
  
})

test_that("With the region", {
  expect_that( retrieveH(D, region = c("SEAsia", "SEAsia")), throws_error("region"))
  
  H = retrieveH(D, region = "SEAsia")
  
  expect_is(H, "list")
  expect_is(H$H, "numeric")
  expect_is(H$RSE, "numeric")
  expect_equal(length(H$H), 61965)
  expect_equal(H$RSE, 5.691)
  RSE = H$RSE
  
  H = retrieveH(c(2,3), region = "SEAsia")
  expect_equal(H$H, c(3.310947, 4.611247), tolerance = 10^-6)
  expect_equal(H$RSE, RSE)
})


# extract(Rast, cbind(74.14583, 14.78889), "bilinear")




context("Function getTaxonomy")
test_that("Without finding the order", {
  Taxo = getTaxonomy(KarnatakaForest$genus[1:50])
  
  expect_equal(Taxo[,1], as.character(KarnatakaForest$genus[1:50]))
  expect_is(Taxo, "data.frame")
  expect_equal(dim(Taxo), c(50, 2))
  expect_is(Taxo[,1], "character")
  expect_is(Taxo[,2], "character")
  
  Taxo = Taxo[order(Taxo$inputGenus), ]
  expect_equivalent(Taxo[1:3,], 
                    data.frame("inputGenus" = c("Alangium", "Albizia", "Albizia"), 
                               "family" = c("Cornaceae", "Fabaceae", "Fabaceae")))
})

test_that("With finding the order", {
  
  Taxo = getTaxonomy(KarnatakaForest$genus[1:50], findOrder = T)
  expect_equal(Taxo[,1], as.character(KarnatakaForest$genus[1:50]))
  
  expect_is(Taxo, "data.frame")
  expect_equal(dim(Taxo), c(50, 3))
  expect_is(Taxo[,1], "character")
  expect_is(Taxo[,2], "character")
  expect_is(Taxo[,3], "character")
  
  Taxo = Taxo[order(Taxo$inputGenus), ]
  expect_equivalent(Taxo[1:3,], 
                    data.frame("inputGenus" = c("Alangium", "Albizia", "Albizia"), 
                               "family" = c("Cornaceae", "Fabaceae", "Fabaceae"), 
                               "order" = c("Cornales", "Fabales", "Fabales")))
})


context("Internal function of HD model ")
test_that("loglog function", {
  skip_if_not_function("loglogFunction")
  
  data = data.frame(H = c(5,5), D = c(2,3))
  
  expect_is( loglogFunction( data,  "log1" ), "lm" )
  expect_is( loglogFunction( data,  "log2" ), "lm" )
  expect_is( loglogFunction( data,  "log3" ), "lm" )
  
})

test_that("Michaelis function", {
  skip_if_not_function("michaelisFunction")
  data = data.frame(H = retrieveH(D, coord = coord)$H, D = D)
  
  expect_is( michaelisFunction( data ), "nls" )
})

test_that("Weibull function", {
  skip_if_not_function("weibullFunction")
  data = data.frame(H = retrieveH(D, coord = coord)$H, D = D)
  
  expect_is( weibullFunction( data ), "nls" )
})



context("Predict Height of the tree")
test_that("predictHeight", {
  skip_if_not_function("predictHeight")
  for(method in c("log1", 'log2', "log3", "weibull", "michaelis")){
    HDmodel<-modelHD(D=NouraguesHD$D,
                     H=NouraguesHD$H,
                     method=method,
                     useWeight =TRUE)
    for (err in c(T, F)){
      expect_length( predictHeight(D, HDmodel, err = err), length(D))
      expect_is( predictHeight(D, HDmodel, err = err), "numeric")
    }
    
  }
})



