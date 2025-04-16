#require(data.table)

D <- NouraguesHD$D
H <- NouraguesHD$H

output <- data.frame(D = D, H = H)
output <- na.omit(output)


context("Methods of function modelHD")

for (method in c("log1", "log2", "michaelis", "weibull")) {
    for (useWeight in c(TRUE, FALSE)) {
    test_that(paste("Method", method, "useWeight", useWeight), {
      #skip_on_cran()
      HDmodel <- modelHD(D, H, method = method, useWeight = useWeight)

      logMethod <- grepl("log", method)

      expect_is(HDmodel, "list")
      expect_equal(length(HDmodel), ifelse(logMethod, 10, 9))

      expect_equal(HDmodel$input$D, output$D)
      expect_equal(HDmodel$input$H, output$H)

      expect_is(HDmodel$model, ifelse(logMethod, "lm", "nls"))

      expect_equal(length(HDmodel$residuals), nrow(output))

      expect_is(HDmodel$coefficients, "matrix")

      expect_is(HDmodel$R.squared, ifelse(logMethod, "numeric", "NULL"))

      expect_is(HDmodel$formula, "call")

      expect_equal(HDmodel$method, method)

      expect_equal(length(HDmodel$predicted), nrow(output))
      expect_is(HDmodel$predicted, "numeric")

      expect_is(HDmodel$RSE, "numeric")

      expect_is(HDmodel$RSElog, ifelse(logMethod, "numeric", "NULL"))
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
  expect_warning(suppressMessages(modelHD(c(1,2,rep(3,13)), rnorm(15,10))) , "Be careful, your diameter values are not evenly distributed.")
})

test_that("NA characters", {
  #skip_on_cran()
  H1 <- H
  H1[ seq(length(H) - 5) ] <- NA

  expect_error(modelHD(D, H1), "NA")
})

test_that("Without parameters", {
  #skip_on_cran()
  Res <- expect_message(modelHD(D, H, useWeight = TRUE), "build a HD model")

  expect_is(Res, "data.frame")
  expect_equal(ncol(Res),4)

  res <- "method  RSE    RSElog Average_bias
log1 4.305060 0.2231136  0.004227454
log2  4.222718 0.2215495  0.003121671
weibull 4.307951        NA  0.002823978
michaelis 4.294488        NA  0.014564152
"


  expect_equal(Res, fread(res, data.table = FALSE), tolerance = 10^-6)
})

test_that("With the plot arguments", {
  #skip_on_cran()
  plot <- NouraguesHD$plotId

  Res <- expect_message(modelHD(D, H, plot = plot), "build a HD model")

  expect_is(Res, "list")
  expect_length(Res, length(unique(plot)))
  expect_equal(names(Res), unique(plot))

  invisible(sapply(Res, function(x) {
    expect_is(x, "data.frame")
    expect_equal(ncol(x), 4)
  }))
  expect_failure(expect_equal(Res[[1]], Res[[2]]))

  Res <- modelHD(D, H, plot = plot, method = "log2")

  expect_is(Res, "list")
  expect_length(Res, length(unique(plot)))
  expect_equal(names(Res), unique(plot))

  invisible(sapply(Res, function(x) {
    expect_is(x, "list")
    expect_equal(length(x), 10)
  }))
  expect_failure(expect_equal(Res[[1]], Res[[2]]))

  Res <- modelHD(D, H, plot = "plot1")
  expect_is(Res, "data.frame")

  expect_error(modelHD(D, H, plot = rep("plot", 2)), "length")
})

test_that("snapshot of plot", {
  toto <- modelHD(D, H, method = "log2", useWeight = T, drawGraph = T)
  vdiffr::expect_doppelganger("plot_modelHD", modelHD(D, H, method = "log2", useWeight = T, drawGraph = FALSE)$fitPlot )
})
