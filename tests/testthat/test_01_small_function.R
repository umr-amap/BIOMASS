options(stringsAsFactors = FALSE)

data("KarnatakaForest")
data("NouraguesHD")

KarnatakaForest <- KarnatakaForest[1:50, ]

# Argument repetitive
D <- KarnatakaForest$D
coord <- cbind(KarnatakaForest$long, KarnatakaForest$lat)


skip_if_not_function <- function(name) {
  if (!exists(name)) {
    skip(paste("The function", name, "is internal"))
  }
}

context("Function computeE")
test_that("Compute E", {
  skip_if_not_function("computeE")

  E <- computeE(coord)

  expect_is(E, "numeric")
  expect_length(E, 50)

  expect_equal(computeE(cbind(12, 50)), 1.129928, tolerance = 0.1)
  
  expect_error(computeE(cbind(long = -20, lat = 4)), "coordinate")
})





context("Function getBioclimParam")
test_that("getBioclimParam", {
  skip_if_not_function("getBioclimParam")
  B <- getBioclimParam(coord)

  expect_is(B, "data.frame")
  expect_equal(dim(B), c(50, 3))

  expect_equal(getBioclimParam(cbind(12, 50)),
    data.frame("tempSeas" = 6.62375, "precSeas" = 0.01925, "CWD" = -0.0921875),
    tolerance = 0.1
  )
})







# extract(Rast, cbind(74.14583, 14.78889), "bilinear")




context("Function getTaxonomy")
test_that("Without finding the order", {
  Taxo <- getTaxonomy(KarnatakaForest$genus)

  expect_equal(Taxo[, 1], as.character(KarnatakaForest$genus))
  expect_is(Taxo, "data.frame")
  expect_equal(dim(Taxo), c(50, 2))
  expect_is(Taxo[, 1], "character")
  expect_is(Taxo[, 2], "character")

  Taxo <- Taxo[order(Taxo$inputGenus), ]


  res <- "inputGenus      family
  Acacia    Fabaceae
  Alangium   Cornaceae
  Albizia    Fabaceae
  Allophylus Sapindaceae
  Alseodaphne   Lauraceae"

  expect_equivalent(unique(Taxo), fread(res, data.table = F))
})

test_that("With finding the order", {
  Taxo <- getTaxonomy(KarnatakaForest$genus, findOrder = T)
  expect_equal(Taxo[, 1], as.character(KarnatakaForest$genus))

  expect_is(Taxo, "data.frame")
  expect_equal(dim(Taxo), c(50, 3))
  expect_is(Taxo[, 1], "character")
  expect_is(Taxo[, 2], "character")
  expect_is(Taxo[, 3], "character")

  Taxo <- Taxo[order(Taxo$inputGenus), ]

  res <- "inputGenus      family      order
  Acacia    Fabaceae    Fabales
  Alangium   Cornaceae   Cornales
  Albizia    Fabaceae    Fabales
  Allophylus Sapindaceae Sapindales
  Alseodaphne   Lauraceae   Laurales"


  expect_equivalent(unique(Taxo), fread(res, data.table = F))
})


context("Internal function of HD model ")
test_that("loglog function", {
  skip_if_not_function("loglogFunction")

  data <- data.frame(H = c(5, 5), D = c(2, 3))

  expect_is(loglogFunction(data, "log1"), "lm")
  expect_is(loglogFunction(data, "log2"), "lm")
  expect_is(loglogFunction(data, "log3"), "lm")
})

test_that("Michaelis function", {
  skip_if_not_function("michaelisFunction")
  data <- data.frame(H = retrieveH(D, coord = coord)$H, D = D)

  expect_is(michaelisFunction(data), "nls")
})

test_that("Weibull function", {
  skip_if_not_function("weibullFunction")
  data <- data.frame(H = retrieveH(D, coord = coord)$H, D = D)

  expect_is(weibullFunction(data), "nls")
})



context("Predict Height of the tree")

for (method in c("log1", "log2", "log3", "weibull", "michaelis")) {
  test_that(paste("predictHeight", method), {
    skip_if_not_function("predictHeight")
    HDmodel <- modelHD(
      D = NouraguesHD$D,
      H = NouraguesHD$H,
      method = method,
      useWeight = TRUE
    )
    for (err in c(T, F)) {
      expect_length(predictHeight(D, HDmodel, err = err), length(D))
      expect_is(predictHeight(D, HDmodel, err = err), "numeric")
      
      if (err == T){
        H = predictHeight(rep(10, 10), HDmodel, err = err)
        expect_false(all(H == H[1]))
      }
        
    }
  })
}

test_that("predictHeigth with plot argument", {
  HDmodel <- modelHD(
    D = NouraguesHD$D,
    H = NouraguesHD$H,
    method = "log2",
    useWeight = TRUE
  )

  expect_failure(expect_error(predictHeight(D, HDmodel, plot = "Plot1")))

  HDmodel <- modelHD(
    D = NouraguesHD$D,
    H = NouraguesHD$H,
    method = "log2",
    useWeight = TRUE,
    plot = NouraguesHD$plotId
  )

  expect_error(predictHeight(D, HDmodel), "model")
  expect_length(predictHeight(D, HDmodel, plot = "Plot1"), length(D))
  expect_failure(expect_equal(
    predictHeight(D, HDmodel, plot = "Plot1"),
    predictHeight(D, HDmodel, plot = "Plot2")
  ))
  expect_error(predictHeight(D, HDmodel, plot = "AAA"), "Cannot")
  expect_error(predictHeight(D, HDmodel, plot = c("Plot1", "Plot2")), "length")

  plot <- rep(c("Plot1", "Plot2"), length.out = length(D))

  Res <- predictHeight(D, HDmodel, plot = plot)
  expect_failure(expect_equal(Res, predictHeight(D, HDmodel, plot = "Plot1")))
  expect_equal(Res[plot == "Plot1"], predictHeight(D, HDmodel, plot = "Plot1")[plot == "Plot1"])
  expect_equal(Res[plot == "Plot2"], predictHeight(D, HDmodel, plot = "Plot2")[plot == "Plot2"])
  expect_failure(expect_equal(Res[plot == "Plot1"], predictHeight(D, HDmodel, plot = "Plot2")[plot == "Plot1"]))
  expect_failure(expect_equal(Res[plot == "Plot2"], predictHeight(D, HDmodel, plot = "Plot1")[plot == "Plot2"]))
})




context("compute Feld Region")
test_that("compute Feld Region", {
  expect_equal(unique(computeFeldRegion(coord)), "SEAsia")
  expect_length(computeFeldRegion(coord), nrow(coord))

  expect_equal(unique(computeFeldRegion(coord, level = "world")), "Pantropical")
  expect_equal(
    computeFeldRegion(coord, level = rep(c("region", "continent", "world"), length.out = nrow(coord))),
    rep(c("SEAsia", "SEAsia", "Pantropical"), length.out = nrow(coord))
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




long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)


context("lat long to UTM")
test_that("lat long to UTM", {
  skip_if_not_installed("proj4")


  UTM <- latlong2UTM(coord)
  expect_is(UTM, "data.frame")

  expect_equal(dim(UTM), c(3, 5))

  for (i in c(1, 2, 4, 5)) expect_is(UTM[, i], "numeric")
  expect_is(UTM[, 3], "character")
  expect_equal(unique(UTM[, 3]), "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
})
