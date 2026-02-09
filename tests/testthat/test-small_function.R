options(stringsAsFactors = FALSE)

data("NouraguesTrees")
data("NouraguesHD")
data("NouraguesCoords")

NouraguesTrees <- NouraguesTrees[1:50, ]

# Argument repetitive
D <- NouraguesTrees$D
modHD <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2", drawGraph = FALSE, bayesian = FALSE)
data <- data.frame(H = retrieveH(D, model =modHD )$H, D = D)


skip_if_not_function <- function(name) {
  if (!exists(name)) {
    skip(paste("The function", name, "is internal"))
  }
}

context("Function computeE")
test_that("Compute E", {
  
  skip_on_cran()
  
  coord <- apply(NouraguesCoords[c("Long","Lat")] , 2, mean) # compute the mean of the corner coordinates

  E <- computeE(coord)

  expect_is(E, "numeric")
  expect_length(E, 1)
  
  expect_equal(computeE(cbind(12, 50)), 1.129928, tolerance = 0.1)
  expect_error(computeE(cbind(long = -20, lat = 4)), "coordinate")
})

context("Function getBioclimParam")
test_that("getBioclimParam", {
  
  skip_if_not_function("getBioclimParam")
  
  coord <- NouraguesCoords[c("Long","Lat")]
  
  B <- getBioclimParam(coord)

  expect_is(B, "data.frame")
  expect_equal(dim(B), c(16, 3))

  skip_on_cran()
  expect_equal(getBioclimParam(cbind(12, 50)),
    data.frame("tempSeas" = 6.62375, "precSeas" = 0.01925, "CWD" = -0.0921875),
    tolerance = 0.1
  )
})

context("Deprecated getTaxonomy")
test_that("With finding the order", {
  withr::local_options(lifecycle_verbosity = "quiet")
  Taxo <- getTaxonomy(NouraguesTrees$Genus, findOrder = TRUE)
  expect_equal(Taxo[, 1], as.character(NouraguesTrees$Genus))

  expect_is(Taxo, "data.frame")
  expect_equal(dim(Taxo), c(50, 3))
  expect_is(Taxo[, 1], "character")
  expect_is(Taxo[, 2], "character")
  expect_is(Taxo[, 3], "character")

  Taxo <- Taxo[order(Taxo$inputGenus), ]

  res <- "inputGenus      family      order
  Abarema  Fabaceae      Fabales
  Amaioua Rubiaceae  Gentianales
  Amphirrhox Violaceae Malpighiales"

  expect_equivalent(unique(Taxo)[1:3,], fread(res, data.table = FALSE))
})

test_that("getTaxonomy is deprecated", {
  expect_snapshot(getTaxonomy(NouraguesTrees$Genus, findOrder = TRUE))
})

context("Internal function of HD model ")
test_that("loglog function", {
  skip_if_not_function("loglogFunction")

  expect_is(loglogFunction(data, method="log1", bayesian=FALSE), "lm")
  expect_is(loglogFunction(data, method="log2", bayesian=FALSE), "lm")
})

test_that("Michaelis function", {
  skip_if_not_function("michaelisFunction")

  expect_is(michaelisFunction(data, weight = NULL, bayesian = FALSE), "nls")
})

test_that("Weibull function", {
  
  skip_if_not_function("weibullFunction")

  expect_is(weibullFunction(data, weight = NULL, bayesian = FALSE), "nls")
})


context("compute Feld Region")
test_that("compute Feld Region", {
  
  coord <- cbind(-52.68, 4.08)
  
  expect_equal(unique(computeFeldRegion(coord)), "GuianaShield")
  expect_length(computeFeldRegion(coord), nrow(coord))

  expect_equal(unique(computeFeldRegion(coord, level = "world")), "Pantropical")
  expect_equal(
    computeFeldRegion(coord, level = rep(c("region", "continent", "world"), length.out = nrow(coord))),
    rep(c("GuianaShield", "SEAsia", "Pantropical"), length.out = nrow(coord))
  )

  expect_error(computeFeldRegion(coord, level = c("region", "region")))
  expect_error(computeFeldRegion(coord, level = "fgaz"))
})


context("Analysis of Procrust")
test_that("Analysis of Procrust", {
  X <- expand.grid(X = c(0, 1), Y = c(0, 1))
  Y <- expand.grid(X = c(11, 10), Y = c(10, 11))

  lis <- procrust(Y, X)
  expect_is(lis, "list")
  expect_length(lis, 2)

  expect_is(lis$rotation, "matrix")
  expect_is(lis$translation, "matrix")

  expect_equivalent(as.matrix(Y), sweep(as.matrix(X) %*% lis$rotation, 2, lis$translation, "+"))
})


context("lat long to UTM")
test_that("lat long to UTM", {
  long <- c(-52.68, -51.12, -53.11)
  lat <- c(4.08, 3.98, 4.12)
  coord <- cbind(long, lat)
  
  skip_if_not_installed("proj4") 
  UTM <- latlong2UTM(coord)
  expect_is(UTM, "data.frame")

  expect_equal(dim(UTM), c(3, 5))

  for (i in c(1, 2, 4, 5)) expect_is(UTM[, i], "numeric")
  expect_is(UTM[, 3], "character")
  expect_equal(unique(UTM[, 3]), "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
})

