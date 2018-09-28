context("Mesure if the computeE is working")
library(BIOMASS)


test_that("Is a good value in return", {
  data("KarnatakaForest")
  E = computeE(cbind(KarnatakaForest$long, KarnatakaForest$lat))
  
  expect_that(E, is_a("numeric"))
  expect_equal(length(E), 61965)

  expect_that(computeE(cbind(12, 50)), equals(1.129928, tolerance = 0.1) )
})