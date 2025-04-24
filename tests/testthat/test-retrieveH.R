data("NouraguesTrees")
D <- NouraguesTrees$D[1:50]
data("NouraguesCoords")
coord <- apply(NouraguesCoords[c("Long","Lat")] , 2, mean) # compute the mean of the corner coordinates

context("Function retriveH")

test_that("With the HDmodel", {
  expect_error(retrieveH(D), "Either")

  HDmodel <- modelHD(
    D = NouraguesHD$D,
    H = NouraguesHD$H,
    method = "log2",
    useWeight = TRUE
  )

  H <- retrieveH(D, model = HDmodel)
  expect_is(H, "list")
  expect_is(H$H, "numeric")
  expect_is(H$RSE, "numeric")
  expect_length(H$H, length(D))
  expect_length(H$RSE, 1)

  D[2] <- NA
  H <- retrieveH(D, model = HDmodel)
  expect_false(all(is.na(H$H)))
  
  expect_error(retrieveH(D, model = HDmodel, region = "") , "Too many input")
})

test_that("With the coordinates", {
  
  expect_error(retrieveH(D, coord = rbind(coord,coord)) , "coord should be either")
  
  skip_on_cran()
  
  H <- retrieveH(D, coord = coord)

  expect_is(H, "list")
  expect_length(H, 2)
  expect_is(H$H, "numeric")
  expect_equal(H$RSE, NA)
  expect_length(H$H, length(D))
  expect_length(H$RSE, 1)

  D[2] <- NA
  H <- retrieveH(D, coord = coord)
  expect_false(all(is.na(H$H)))
})

test_that("With the region", {
  expect_error(retrieveH(D, region = rep("SEAsia", 2)), "region")

  H <- retrieveH(D, region = "SEAsia")

  expect_is(H, "list")
  expect_is(H$H, "numeric")
  expect_is(H$RSE, "numeric")
  expect_length(H$H, length(D))
  expect_length(H$RSE, 1)

  D[2] <- NA
  H <- retrieveH(D, region = "SEAsia")
  expect_false(all(is.na(H$H)))
})

test_that("With the plot", {
  model <- modelHD(NouraguesHD$D,
    NouraguesHD$H,
    method = "log2",
    plot = NouraguesHD$plotId
  )

  D <- NouraguesHD$D

  H <- retrieveH(D, model, plot = NouraguesHD$plotId)
  expect_length(H$H, length(D))
  expect_length(H$RSE, length(unique(NouraguesHD$plotId)))
})
