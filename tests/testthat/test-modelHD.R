data("NouraguesHD")
D <- NouraguesHD$D
H <- NouraguesHD$H

output <- data.frame(D = D, H = H)

# Note: all tests on bayesian models usin brms are available at tests/test/function/brms_modelHD_*.R

for (method in c("log1", "log2", "michaelis", "weibull")) {
    for (useWeight in c(TRUE, FALSE)) {
    test_that(paste("Method", method, "useWeight", useWeight), {
      #skip_on_cran()
      HDmodel <- modelHD(D, H, method = method, useWeight = useWeight, bayesian = FALSE)

      logMethod <- grepl("log", method)

      expect_true(is.list(HDmodel))
      expect_equal(length(HDmodel), ifelse(logMethod, 8, 7))

      expect_equal(HDmodel$input$D, output$D)
      expect_equal(HDmodel$input$H, output$H)

      #expect_equal(HDmodel$model, ifelse(logMethod, "lm", "nls"))

      expect_equal(length(HDmodel$residuals), nrow(output))

      expect_equal(HDmodel$method, method)

      expect_equal(length(HDmodel$predicted), nrow(output))
      expect_type(HDmodel$predicted, "double")

      expect_type(HDmodel$RSE, "double")

      expect_type(HDmodel$RSElog, ifelse(logMethod, "double", "NULL"))
    })
  }
}

test_that("errors and warnings", {
  expect_error(modelHD(D, 1:100) , "do not have the same length")
  expect_error(modelHD(D, 1:10) , "(less than 15 non NA)")
  expect_error(modelHD(D, H, method = "toto") , "Chose your method among those ones")
  expect_error(modelHD(D, H, useWeight = "") , "UseWeight argument must be a boolean")
  expect_error(modelHD(D, H, drawGraph = "") , "drawGraph argument must be a boolean")
  expect_error(modelHD(D, H, drawGraph = "") , "drawGraph argument must be a boolean")
  expect_warning(suppressMessages(modelHD(c(1,2,rep(3,13)), rnorm(15,10),method = "log2", bayesian = FALSE)) , "Be careful, your diameter values are not evenly distributed.")
})

test_that("NA characters", {
  #skip_on_cran()
  H1 <- H
  H1[ seq(length(H) - 5) ] <- NA

  expect_error(modelHD(D, H1), "NA")
})

test_that("Without parameters", {
  expect_message(modelHD(D = D, H = H, useWeight = FALSE), "build a HD model")
  Res <- modelHD(D = D, H = H, useWeight = FALSE)
  expect_true(is.data.frame(Res))
  expect_equal(ncol(Res),4)

  res <- "method  RSE    RSElog Average_bias
log1 4.305060 0.2231136  -0.01559652
log2  4.222718 0.2215495  -0.01595885
weibull 4.220562        NA  -0.01878870
michaelis 4.235974        NA  -0.02018496
"
  expect_equal(Res, fread(res, data.table = FALSE), tolerance = 10^-6)
})

test_that("With the plot arguments", {
  #skip_on_cran()
  plot <- NouraguesHD$plotId
  expect_message(modelHD(D, H, plot = plot, bayesian = FALSE), "build a HD model")
  
  Res <-modelHD(D, H, plot = plot, bayesian = FALSE)

  expect_true(is.list(Res))
  expect_length(Res, length(unique(plot)))
  expect_equal(names(Res), unique(plot))

  invisible(sapply(Res, function(x) {
    expect_true(is.data.frame(x))
    expect_equal(ncol(x), 4)
  }))
  expect_failure(expect_equal(Res[[1]], Res[[2]]))

  Res <- modelHD(D, H, plot = plot, method = "log2", bayesian = FALSE)

  expect_true(is.list(Res))
  expect_length(Res, length(unique(plot)))
  expect_equal(names(Res), unique(plot))

  invisible(sapply(Res, function(x) {
    expect_true(is.list(x))
    expect_equal(length(x), 8)
  }))
  expect_failure(expect_equal(Res[[1]], Res[[2]]))

  Res <- modelHD(D, H, plot = "plot1", bayesian = FALSE)
  expect_true(is.data.frame(Res))

  expect_error(modelHD(D, H, plot = rep("plot", 2)), "length")
})


