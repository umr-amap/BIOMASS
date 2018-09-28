library(BIOMASS)

options(stringsAsFactors = FALSE)

data("NouraguesHD")
D=NouraguesHD$D
H=NouraguesHD$H

output = data.frame(D= D, H = H)
output = na.omit( output )


context("Methods of function modelHD")
test_that("Method log", {
  for (method in c("log1", "log2", "log3")) {
    HDmodel = modelHD(D, H, method = method, useWeight = T)
    
    expect_is(HDmodel, "list")
    expect_equal(length(HDmodel), 10)
    
    expect_equal(HDmodel$input$D, output$D)
    expect_equal(HDmodel$input$H, output$H)
    
    expect_is(HDmodel$model, "lm")
    
    expect_equal(length(HDmodel$residuals), nrow(output))
    
    expect_is(HDmodel$coefficients, "matrix")
    
    expect_is(HDmodel$R.squared, "numeric")
    
    expect_is(HDmodel$formula, "call")
    
    expect_equal(HDmodel$method, method)
    
    expect_equal(length(HDmodel$predicted), nrow(output))
    expect_is(HDmodel$predicted, "numeric")
    
    expect_is(HDmodel$RSE, "numeric")
    
    expect_is(HDmodel$RSElog, "numeric")
  }
})


test_that("Other Method", {
  for (method in c("michaelis", "weibull")) {
    HDmodel = modelHD(D, H, method = method, useWeight = T)
    
    expect_is(HDmodel, "list")
    expect_equal(length(HDmodel), 9)
    
    expect_equal(HDmodel$input$D, output$D)
    expect_equal(HDmodel$input$H, output$H)
    
    expect_is(HDmodel$model, "nls")
    
    expect_equal(length(HDmodel$residuals), nrow(output))
    
    expect_is(HDmodel$coefficients, "matrix")
    
    expect_is(HDmodel$R.squared, "NULL")
    
    expect_is(HDmodel$formula, "call")
    
    expect_equal(HDmodel$method, method)
    
    expect_equal(length(HDmodel$predicted), nrow(output))
    expect_is(HDmodel$predicted, "numeric")
    
    expect_is(HDmodel$RSE, "numeric")
  }
})


