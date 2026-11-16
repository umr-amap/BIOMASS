# Goodman et al. (2013) palm allometry, ln(AGB kg) = -3.3488 + 2.7483 ln(D),
# RSE 0.5876, in Mg
goodman <- list(
  formula = ~ a + b * log(D),
  parameters = data.frame(a = -3.3488 - log(1000), b = 2.7483, sigma = 0.5876),
  link = "log")

test_that("AGBmonteCarlo runs a user formula allometry as documented", {
  set.seed(1)
  D <- c(10, 20, 30)
  res <- AGBmonteCarlo(D = D, WD = rep(0.6, 3), errWD = rep(0.05, 3),
    Dpropag = 0, n = 1000, fitted_allom = goodman)
  expect_equal(dim(res$AGB_simu), c(3, 1000))
  # Mean of the log-normal
  agb_mean <- exp(-3.3488 + 2.7483 * log(D) + 0.5876^2 / 2) / 1000
  expect_equal(rowMeans(res$AGB_simu), agb_mean, tolerance = 0.1)
  # Log-normal residual error
  expect_equal(apply(log(res$AGB_simu), 1, sd), rep(0.5876, 3), tolerance = 0.1)
})

test_that("each iteration uses one set of parameters for all trees", {
  # Two parameter sets, one for each half of the iterations
  allom <- list(
    formula = ~ a * D,
    parameters = data.frame(a = rep(c(1, 1000), each = 25), sigma = 0),
    link = NULL)
  res <- AGBmonteCarlo(D = c(1, 2, 3), WD = rep(0.6, 3), errWD = rep(0, 3),
    Dpropag = 0, n = 50, fitted_allom = allom)
  expect_equal(res$AGB_simu[, 1], c(1, 2, 3))
  expect_equal(res$AGB_simu[, 50], c(1000, 2000, 3000))
})

test_that("a single row of parameters is used in every iteration", {
  allom <- list(formula = ~ a * D, parameters = data.frame(a = 2, sigma = 0),
    link = "identity")
  res <- AGBmonteCarlo(D = c(1, 2), WD = rep(0.6, 2), errWD = rep(0, 2),
    Dpropag = 0, n = 50, fitted_allom = allom)
  expect_true(all(res$AGB_simu == c(2, 4)))
})

test_that("AGBmonteCarlo rejects malformed allometries", {
  expect_error(AGBmonteCarlo(D = 10, WD = 0.6, errWD = 0, n = 50,
    fitted_allom = "chave2015"), "fitted_allom must be")
  expect_error(AGBmonteCarlo(D = 10, WD = 0.6, errWD = 0, n = 50,
    fitted_allom = list(formula = ~ a * D, parameters = data.frame(a = 1))),
    "sigma")
  expect_error(AGBmonteCarlo(D = 10, WD = 0.6, errWD = 0, n = 50,
    fitted_allom = list(formula = ~ a * D,
      parameters = data.frame(a = 1, sigma = 0), link = "logit")),
    "link")
})
