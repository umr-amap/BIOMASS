options(stringsAsFactors = FALSE)

data("NouraguesHD")
D <- NouraguesHD$D
H <- NouraguesHD$H

output <- data.frame(D = D, H = H)
output <- na.omit(output)


context("Methods of function modelHD")

for (method in c("log1", "log2", "log3", "michaelis", "weibull")) {
  for (useWeight in c(TRUE, FALSE)) {
    test_that(paste("Method", method, "useWeight", useWeight), {
      HDmodel <- modelHD(D, H, method = method, useWeight = useWeight)

      logMethod <- ifelse(length(grep("log", method)) == 0, FALSE, TRUE)

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
  H1[ sample(1:length(H), size = 1038) ] <- NA

  expect_error(modelHD(D, H1), "NA")
})
