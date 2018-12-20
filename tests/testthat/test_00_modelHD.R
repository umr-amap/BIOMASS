require(data.table)

D <- NouraguesHD$D
H <- NouraguesHD$H

output <- data.frame(D = D, H = H)
output <- na.omit(output)


context("Methods of function modelHD")

for (method in c("log1", "log2", "log3", "michaelis", "weibull")) {
  for (useWeight in c(TRUE, FALSE)) {
    test_that(paste("Method", method, "useWeight", useWeight), {
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

test_that("NA characters", {
  H1 <- H
  H1[ seq(length(H) - 5) ] <- NA

  expect_error(modelHD(D, H1), "NA")
})

test_that("Without parameters", {
  Res <- expect_message(modelHD(D, H, useWeight = T), "build a HD model")

  expect_is(Res, "data.frame")
  expect_equal(ncol(Res), 5)

  res <- "method  color      RSE    RSElog Average_bias
log1   blue 4.305060 0.2231136  0.004227454
log2  green 4.222718 0.2215495  0.003121671
log3    red 4.225362 0.2216716  0.003157274
weibull orange 4.307951        NA  0.002823978
michaelis purple 4.294488        NA  0.014564152
"


  expect_equal(Res, fread(res, data.table = F), tolerance = 10^-6)
})

test_that("With the plot arguments", {
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
