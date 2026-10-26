mc_hd <- customHD(~ a * D^b / (k + D^b),
  parameters = c(a = 57.1, b = 0.73, k = 21.79), sigma = 0.181, link = "log")

test_that("customHD checks its arguments", {
  expect_s3_class(mc_hd, "customHD")
  expect_error(customHD(H ~ a * D, c(a = 1), sigma = 0.1), "one-sided")
  expect_error(customHD(~ a * D, c(1), sigma = 0.1), "named")
  expect_error(customHD(~ a * D^b, c(a = 1), sigma = 0.1), "names of `parameters`")
  expect_error(customHD(~ a, c(a = 1), sigma = 0.1), "diameter")
  expect_error(customHD(~ a * D, c(a = 1), sigma = -1), "sigma")
})

test_that("retrieveH and predictHeight give the mean height of a customHD model", {
  D <- c(10, 50, 100)
  H_median <- 57.1 * D^0.73 / (21.79 + D^0.73)
  H_mean <- H_median * exp(0.181^2 / 2)
  expect_equal(retrieveH(D, model = mc_hd)$H, H_mean)
  expect_equal(retrieveH(D, model = mc_hd)$RSE, 0.181)
  expect_equal(as.vector(predictHeight(as.matrix(D), mc_hd)), H_mean)

  # Identity link: no correction
  id_hd <- customHD(~ a + b * D, c(a = 2, b = 0.5), sigma = 1, link = "identity")
  expect_equal(retrieveH(D, model = id_hd)$H, 2 + 0.5 * D)
})

test_that("predictHeight with error draws log-normal heights around the median", {
  set.seed(1)
  D <- matrix(50, nrow = 2, ncol = 20000)
  H <- predictHeight(D, mc_hd, err = TRUE)
  H_median <- 57.1 * 50^0.73 / (21.79 + 50^0.73)
  expect_equal(dim(H), dim(D))
  expect_equal(median(H), H_median, tolerance = 0.01)
  expect_equal(sd(log(H)), 0.181, tolerance = 0.02)
})

test_that("AGBmonteCarlo accepts a customHD model as HDmodel", {
  set.seed(2)
  D <- c(10, 30, 60)
  WD <- c(0.5, 0.6, 0.7)
  res <- AGBmonteCarlo(D = D, WD = WD, errWD = rep(0, 3), HDmodel = mc_hd,
    Dpropag = 0, n = 1000)
  expect_equal(dim(res$AGB_simu), c(3, 1000))
  # Chave et al. (2014) Eq. 4 with the mean height, within Monte Carlo error
  H_mean <- retrieveH(D, model = mc_hd)$H
  agb_det <- computeAGB(D, WD, H = H_mean)
  expect_equal(rowMeans(res$AGB_simu), agb_det, tolerance = 0.1)
})
