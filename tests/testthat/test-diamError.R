simErr <- function(D, a, b, p_large, sd_large) {
  sd <- ifelse(runif(length(D)) < p_large, sd_large, a + b * D)
  D + rnorm(length(D), 0, sd)
}

test_that("diamError checks its arguments", {
  expect_s3_class(diamError(a = 0.1, b = 0.01), "diamError")
  expect_error(diamError(a = -1, b = 0), "`a` must be")
  expect_error(diamError(a = 0.1, b = c(0, 1)), "`b` must be")
  expect_error(diamError(a = 0.1, b = 0, p_large = 2), "`p_large` must be")
  expect_error(diamError(a = 0.1, b = 0, p_large = 0.1), "`sd_large` must be")
  expect_output(print(diamError(a = 0.1, b = 0.01, 0.05, 4)), "Large errors")
})

test_that("fitDiamError recovers the parameters of simulated errors", {
  set.seed(1)
  D <- rlnorm(3000, log(25), 0.6)
  D1 <- simErr(D, 0.0904, 0.0062, 0.05, 4.64)
  D2 <- simErr(D, 0.0904, 0.0062, 0.05, 4.64)

  err <- fitDiamError(D1, D2)
  expect_s3_class(err, "diamError")
  expect_equal(err$convergence, 0)
  expect_equal(err$n, 3000)
  expect_equal(err$p_large, 0.05, tolerance = 0.3)
  expect_equal(err$sd_large, 4.64, tolerance = 0.2)
  # Small error sd at the median diameter
  expect_equal(err$a + err$b * 25, 0.0904 + 0.0062 * 25, tolerance = 0.15)
  expect_lt(abs(err$bias), 4 * err$bias_se)
  expect_equal(nrow(err$pairs), 3000)
  expect_true(all(err$pairs$prob_large >= 0 & err$pairs$prob_large <= 1))
  # Pairs with very large differences relative to small errors are flagged as
  # large errors
  big <- abs(err$pairs$diff) > 5 & (err$pairs$D1 + err$pairs$D2) / 2 < 100
  expect_true(all(err$pairs$prob_large[big] > 0.9))
})

test_that("fitDiamError without large errors", {
  set.seed(2)
  D <- runif(500, 10, 80)
  err <- fitDiamError(simErr(D, 0.2, 0.01, 0, NA), simErr(D, 0.2, 0.01, 0, NA),
    large = FALSE)
  expect_equal(err$p_large, 0)
  expect_true(all(err$pairs$prob_large == 0))
  # The error of a single measurement, not of the difference between two
  expect_equal(err$a + err$b * 45, 0.2 + 0.01 * 45, tolerance = 0.15)

  # A systematic difference is reported as bias
  err <- fitDiamError(simErr(D, 0.2, 0.01, 0, NA) + 0.5,
    simErr(D, 0.2, 0.01, 0, NA), large = FALSE)
  expect_equal(err$bias, 0.5, tolerance = 0.1)

  # Constant error
  err <- fitDiamError(simErr(D, 0.5, 0, 0, NA), simErr(D, 0.5, 0, 0, NA),
    large = FALSE, slope = FALSE)
  expect_equal(err$b, 0)
  expect_equal(err$a, 0.5, tolerance = 0.15)
})

test_that("fitDiamError input checks", {
  expect_error(fitDiamError(1:20, 1:10), "same length")
  expect_error(fitDiamError(c(1:5, rep(NA, 10)), 1:15), "At least 10 pairs")
  set.seed(3)
  D <- runif(50, 10, 80)
  w <- capture_warnings(fitDiamError(D + rnorm(50, 0, 0.3), D))
  expect_match(w, "poorly estimated", all = FALSE)
})

test_that("simulateD propagates diameter errors", {
  set.seed(4)
  D <- c(10, 50, 100)

  expect_equal(BIOMASS:::simulateD(D, NULL, 50), matrix(D, 3, 50))

  D_simu <- BIOMASS:::simulateD(D, 1, 1000)
  expect_equal(dim(D_simu), c(3, 1000))
  expect_equal(apply(D_simu, 1, sd), c(1, 1, 1), tolerance = 0.1)

  # Without large errors, sd increases with diameter
  D_simu <- BIOMASS:::simulateD(D, diamError(a = 0, b = 0.02), 1000)
  expect_equal(apply(D_simu, 1, sd), 0.02 * D, tolerance = 0.1)

  # Large errors are drawn for each stem in each simulation, even for few stems
  D_simu <- BIOMASS:::simulateD(D, "chave2004", 1000)
  prop_large <- mean(abs(D_simu - D) > 2)
  expect_gt(prop_large, 0.02)
  expect_lt(prop_large, 0.06)

  # Always large errors
  D_simu <- BIOMASS:::simulateD(D, diamError(a = 0, b = 0, p_large = 1,
    sd_large = 5), 1000)
  expect_equal(apply(D_simu, 1, sd), c(5, 5, 5), tolerance = 0.1)
})

test_that("AGBmonteCarlo accepts a diameter error model", {
  set.seed(5)
  D <- runif(20, 10, 80)
  AGB <- AGBmonteCarlo(D, WD = rep(0.6, 20), errWD = 0.05, H = 0.8 * D,
    errH = 2, Dpropag = diamError(a = 0.1, b = 0.01, p_large = 0.05,
      sd_large = 4), n = 50)
  expect_equal(dim(AGB$AGB_simu), c(20, 50))

  # Larger diameter errors give more variable AGB
  set.seed(5)
  AGB0 <- AGBmonteCarlo(D, WD = rep(0.6, 20), errWD = 0, H = 0.8 * D,
    errH = 0, Dpropag = diamError(a = 0, b = 0), n = 500)
  set.seed(5)
  AGB1 <- AGBmonteCarlo(D, WD = rep(0.6, 20), errWD = 0, H = 0.8 * D,
    errH = 0, Dpropag = diamError(a = 0, b = 0, p_large = 1, sd_large = 10),
    n = 500)
  expect_gt(AGB1$sdAGB, AGB0$sdAGB)

  expect_error(AGBmonteCarlo(D, WD = rep(0.6, 20), errWD = 0.05,
    H = 0.8 * D, errH = 2, Dpropag = "chave"), "Dpropag should be set")
})
