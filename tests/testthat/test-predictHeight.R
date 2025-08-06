context("Predict Height of the tree")

data("NouraguesTrees")
data("NouraguesHD")

# Note: all tests on bayesian models usin brms are available at tests/test/function/brms_modelHD_*.R

test_that("predictHeight errors", {
  
  HDmodel <- modelHD(
    D = NouraguesHD$D, H = NouraguesHD$H,
    method = "log2", useWeight = FALSE)
  
  expect_error(predictHeight(D = NouraguesHD$D, model = HDmodel), "`D` must be a n x m matrix containing tree diameters")
  expect_error(predictHeight(D = as.matrix(NouraguesHD$D), model = HDmodel$model), "`model` must be the output of the `modelHD()` function", fixed = TRUE)
  expect_error(predictHeight(D = as.matrix(NouraguesHD$D), model = HDmodel, plot = "plot1"), "The argument plot and D have not the same length")
  expect_warning(predictHeight(D = as.matrix(NouraguesHD$D), model = HDmodel, plot = NouraguesHD$plotId), "only one model")
  
  mult_HDmodel <- modelHD(
    D = NouraguesHD$D, H = NouraguesHD$H,
    method = "log2", useWeight = FALSE, plot = NouraguesHD$plotId)
  
  expect_error(predictHeight(D = as.matrix(NouraguesHD$D), model = mult_HDmodel), "The 'model' argument contains several stand-specific HD models, use the `plot` argument to assign the corresponding stand-specific model to each tree.")
  
  expect_error(predictHeight(as.matrix(NouraguesHD$D), mult_HDmodel, plot = rep("Plot1",nrow(NouraguesHD))), "The 'model' argument contains the following stand specific HD models which are not present in the 'plot' argument: Plot2")
  
  wrong_ID <- NouraguesHD$plotId ; wrong_ID[1] <- "Plot3"
  expect_error(predictHeight(as.matrix(NouraguesHD$D), mult_HDmodel, plot = wrong_ID), "Cannot find a HD model corresponding to Plot3")
  
})


for (method in c("log1", "log2", "weibull", "michaelis")) {
  test_that(paste("predictHeight", method), {
    HDmodel <- modelHD(
      D = NouraguesHD$D,
      H = NouraguesHD$H,
      method = method,
      useWeight = TRUE
    )
    for (err in c(TRUE, FALSE)) {
      expect_length(predictHeight(as.matrix(NouraguesHD$D), HDmodel, err = err), length(NouraguesHD$D))
      expect_is(predictHeight(as.matrix(NouraguesHD$D), HDmodel, err = err), "numeric")
      
      if (err == TRUE) {
        H <- predictHeight(as.matrix(rep(10, 10)), HDmodel, err = err)
        expect_false(all(H == H[1]))
      }
    }
  })
}

test_that("predictHeigth with plot argument", {
  HDmodel <- modelHD(
    D = NouraguesHD$D, H = NouraguesHD$H,
    method = "log2", useWeight = FALSE)
  
  mult_HDmodel <- modelHD(
    D = NouraguesHD$D, H = NouraguesHD$H,
    method = "log2", useWeight = FALSE, plot = NouraguesHD$plotId)

  mult_HDmodel <- modelHD(
    D = NouraguesHD$D, H = NouraguesHD$H,
    method = "log2", useWeight = FALSE, plot = NouraguesHD$plotId)
  
  res <- predictHeight(D = as.matrix(NouraguesHD$D), model = HDmodel)
  mult_res <- predictHeight(D = as.matrix(NouraguesHD$D), model = mult_HDmodel, plot =  NouraguesHD$plotId)
  
  expect_equal(res[1:2] , mult_res[1:2], tolerance = 0.2)
  
  mult_res_err <- predictHeight(
    D = matrix(rep(NouraguesHD$D,50), nrow = nrow(NouraguesHD)),
    model = mult_HDmodel, plot =  NouraguesHD$plotId, err = TRUE)
  expect_equal(dim(mult_res_err) , c(nrow(NouraguesHD),50))
  expect_equal(mult_res , apply(mult_res_err,1,median), tolerance = 3)
  
})
