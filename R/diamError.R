#' Diameter measurement error model
#'
#' @description Defines a model of diameter measurement error, to be used to
#' propagate diameter measurement errors with [AGBmonteCarlo()]. Follows the
#' form of Chave et al. (2004): most measurements have a small error, whose
#' standard deviation increases linearly with diameter, and a small proportion
#' of measurements have a large error, e.g. due to transcription errors or
#' measurement of the wrong stem, with a constant standard deviation.
#'
#' The model can be estimated from repeated measurements of the same stems with
#' [fitDiamError()].
#'
#' @param a Intercept of the standard deviation of small errors (in cm)
#' @param b Slope of the standard deviation of small errors with diameter
#' @param p_large Probability that a measurement has a large error, between 0
#'   and 1
#' @param sd_large Standard deviation of large errors (in cm). Not needed if
#'   `p_large` is 0
#'
#' @details The standard deviation of the error of a measurement of a stem of
#' diameter `D` is `a + b * D` with probability `1 - p_large`, and `sd_large`
#' with probability `p_large`.
#'
#' `diamError(a = 0.0904, b = 0.0062, p_large = 0.05, sd_large = 4.64)` is the
#' model of Chave et al. (2004), used by `AGBmonteCarlo(Dpropag = "chave2004")`.
#'
#' @return An object of class `"diamError"`, a list with elements `a`, `b`,
#'   `p_large` and `sd_large`
#'
#' @references Chave, J. et al. (2004). _Error propagation and scaling for
#'   tropical forest biomass estimates_. Philosophical Transactions of the Royal
#'   Society B: Biological Sciences, 359(1443), 409-420.
#'
#' @seealso [fitDiamError()], [AGBmonteCarlo()]
#'
#' @examples
#' # Chave et al. (2004)
#' diamError(a = 0.0904, b = 0.0062, p_large = 0.05, sd_large = 4.64)
#'
#' # A constant error of 0.5 cm, without large errors
#' diamError(a = 0.5, b = 0)
#'
#' @export
diamError <- function(a, b, p_large = 0, sd_large = NA) {
  if (!is.numeric(a) || length(a) != 1 || is.na(a) || a < 0) {
    stop("`a` must be a single number >= 0")
  }
  if (!is.numeric(b) || length(b) != 1 || is.na(b) || b < 0) {
    stop("`b` must be a single number >= 0")
  }
  if (!is.numeric(p_large) || length(p_large) != 1 || is.na(p_large) ||
    p_large < 0 || p_large > 1) {
    stop("`p_large` must be a single number between 0 and 1")
  }
  if (p_large > 0 && (!is.numeric(sd_large) || length(sd_large) != 1 ||
    is.na(sd_large) || sd_large <= 0)) {
    stop("`sd_large` must be a single number > 0 if `p_large` > 0")
  }

  structure(
    list(a = a, b = b, p_large = p_large, sd_large = sd_large),
    class = "diamError"
  )
}


#' Fit a diameter measurement error model from repeated measurements
#'
#' @description Estimates the parameters of a diameter measurement error model
#' (see [diamError()]) from pairs of independent measurements of the same
#' stems. The fitted model can be passed to `AGBmonteCarlo(Dpropag = )` to
#' propagate diameter measurement errors to AGB.
#'
#' @param D1 Vector of diameters from the first measurement (in cm)
#' @param D2 Vector of diameters from the second measurement of the same stems,
#'   at the same point of measurement (in cm)
#' @param large Logical. If `TRUE` (default), a proportion of measurements with
#'   large errors is estimated. If `FALSE`, all measurements are assumed to have
#'   small errors. Estimating large errors requires many pairs, normally several
#'   hundred
#' @param slope Logical. If `TRUE` (default), the standard deviation of small
#'   errors increases linearly with diameter. If `FALSE`, it is constant
#'
#' @details Pairs with a missing diameter are removed. Both measurements are
#' assumed to have the same error, so the model describes the error of a single
#' measurement. Let `d = D1 - D2`, `Dm` be the mean of `D1` and `D2`,
#' `s = a + b Dm` the standard deviation of small errors, `L = sd_large` and
#' `p = p_large`. Each measurement has a large error independently with
#' probability `p`, so:
#'
#' `d ~ (1 - p)^2 N(bias, 2 s^2) + 2 p (1 - p) N(bias, s^2 + L^2) + p^2 N(bias, 2 L^2)`
#'
#' The parameters are estimated by maximum likelihood.
#'
#' `p_large` is constrained to be < 0.5.
#'
#' `bias` is the mean difference between measurements. A non-zero `bias`
#' indicates a systematic difference between the two measurements, e.g. due to
#' different measurement teams or instruments, which is not propagated by
#' [AGBmonteCarlo()].
#'
#' Repeated measurements should be independent, i.e. made without knowledge of
#' the first measurement, at the same point of measurement, and close enough in
#' time that stems have not grown. They should cover the range of diameters of
#' the stems to which the model is applied.
#'
#' @return An object of class `"diamError"` (see [diamError()]), with
#'   additional elements:
#'   - `bias`: Estimated mean difference `D1 - D2` (in cm)
#'   - `bias_se`: Standard error of `bias`
#'   - `large`, `slope`: The arguments used
#'   - `n`: Number of pairs used
#'   - `D_range`: Range of diameters of the pairs used
#'   - `logLik`, `AIC`: Log-likelihood and AIC of the model
#'   - `convergence`: Convergence code from [stats::optim()], 0 for successful
#'     convergence
#'   - `pairs`: A data frame with columns `D1`, `D2`, `diff` (`D1 - D2`) and
#'     `prob_large`, the probability that at least one measurement of the pair
#'     has a large error.
#'     Pairs with a high `prob_large` should be checked for data errors
#'
#' @references Chave, J. et al. (2004). _Error propagation and scaling for
#'   tropical forest biomass estimates_. Philosophical Transactions of the Royal
#'   Society B: Biological Sciences, 359(1443), 409-420.
#'
#' @seealso [diamError()], [AGBmonteCarlo()]
#'
#' @examples
#' # Simulate repeated measurements of 500 stems, with the error model of
#' # Chave et al. (2004)
#' set.seed(1)
#' D <- rlnorm(500, log(25), 0.6)
#' simErr <- function(D) {
#'   sd <- ifelse(runif(length(D)) < 0.05, 4.64, 0.0904 + 0.0062 * D)
#'   D + rnorm(length(D), 0, sd)
#' }
#' err <- fitDiamError(simErr(D), simErr(D))
#' err
#'
#' # Pairs most likely to include a large error
#' head(err$pairs[order(-err$pairs$prob_large), ])
#'
#' @importFrom stats dnorm optim optimHess mad median sd
#' @export
fitDiamError <- function(D1, D2, large = TRUE, slope = TRUE) {
  if (!is.numeric(D1) || !is.numeric(D2) || length(D1) != length(D2)) {
    stop("`D1` and `D2` must be numeric vectors of the same length")
  }
  keep <- !is.na(D1) & !is.na(D2)
  D1 <- D1[keep]
  D2 <- D2[keep]
  n <- length(D1)
  if (n < 10) {
    stop("At least 10 pairs of measurements are needed, ", n, " found")
  }
  if (large && n < 100) {
    warning("Only ", n, " pairs of measurements, the proportion of large ",
      "errors may be poorly estimated. Consider `large = FALSE`")
  }

  d <- D1 - D2
  Dm <- (D1 + D2) / 2

  # Parameters on an unconstrained scale:
  # bias, log(a), log(b), qlogis(2 * p_large), log(sd_large)
  unpack <- function(theta) {
    list(
      bias = theta[1],
      a = exp(theta[2]),
      b = if (slope) exp(theta[3]) else 0,
      p_large = if (large) 0.5 * stats::plogis(theta[4]) else 0,
      sd_large = if (large) exp(theta[5]) else NA)
  }

  # Density of `d` for pairs without large errors (`small`), and with at
  # least one large error (`big`)
  densities <- function(par) {
    s <- par$a + par$b * Dm
    p <- par$p_large
    small <- (1 - p)^2 * dnorm(d, par$bias, sqrt(2) * s)
    big <- if (large) {
      2 * p * (1 - p) * dnorm(d, par$bias, sqrt(s^2 + par$sd_large^2)) +
        p^2 * dnorm(d, par$bias, sqrt(2) * par$sd_large)
    } else {
      0
    }
    list(small = small, big = big)
  }

  nll <- function(theta) {
    dens <- densities(unpack(theta))
    -sum(log(pmax(dens$small + dens$big, .Machine$double.xmin)))
  }

  # Starting values from robust estimates of the spread of differences. The
  # spread of differences is sqrt(2) times the error of one measurement
  s0 <- max(mad(d) / sqrt(2), 0.01)
  theta0 <- c(median(d), log(s0 / 2), log(max(s0 / 2 / median(Dm), 1e-4)),
    stats::qlogis(0.1), log(max(10 * s0, sd(d) / sqrt(2))))
  used <- c(TRUE, TRUE, slope, large, large)

  nllUsed <- function(theta_used) {
    theta <- theta0
    theta[used] <- theta_used
    nll(theta)
  }

  fit <- optim(theta0[used], nllUsed, method = "Nelder-Mead",
    control = list(maxit = 5000))
  fit <- optim(fit$par, nllUsed, method = "BFGS", control = list(maxit = 1000))

  theta <- theta0
  theta[used] <- fit$par
  par <- unpack(theta)

  if (fit$convergence != 0) {
    warning("Model did not converge, convergence code ", fit$convergence)
  }
  if (large && par$sd_large < par$a + par$b * max(Dm)) {
    warning("The standard deviation of large errors is smaller than that of ",
      "small errors, large errors may not be identifiable. ",
      "Consider `large = FALSE`")
  }

  # Standard error of the bias, from the observed information
  hess <- tryCatch(optimHess(fit$par, nllUsed), error = function(e) NULL)
  bias_se <- tryCatch(sqrt(solve(hess)[1, 1]), error = function(e) NA_real_)

  dens <- densities(par)
  prob_large <- if (large) dens$big / (dens$small + dens$big) else rep(0, n)

  out <- diamError(a = par$a, b = par$b, p_large = par$p_large,
    sd_large = par$sd_large)
  out$bias <- unname(par$bias)
  out$bias_se <- bias_se
  out$large <- large
  out$slope <- slope
  out$n <- n
  out$D_range <- range(Dm)
  out$logLik <- -fit$value
  out$AIC <- 2 * fit$value + 2 * sum(used)
  out$convergence <- fit$convergence
  out$pairs <- data.frame(D1 = D1, D2 = D2, diff = d, prob_large = prob_large)
  out
}


#' @export
print.diamError <- function(x, ...) {
  cat("Diameter measurement error model\n")
  cat(sprintf("  Small errors: sd = %.4g + %.4g * D (cm)\n", x$a, x$b))
  if (x$p_large > 0) {
    cat(sprintf("  Large errors: p = %.3g, sd = %.4g cm\n", x$p_large,
      x$sd_large))
  } else {
    cat("  No large errors\n")
  }
  if (!is.null(x$n)) {
    cat(sprintf("  Fitted from %d pairs, D = %.1f-%.1f cm\n", x$n,
      x$D_range[1], x$D_range[2]))
    cat(sprintf("  Bias (D1 - D2): %.3g cm (se %.2g)\n", x$bias, x$bias_se))
  }
  invisible(x)
}


# Simulate diameters with measurement errors
#
# D: vector of diameters (cm)
# Dpropag: NULL, "chave2004", a "diamError" object, or a single standard
#   deviation or a vector of standard deviations of the same length as D
# n: number of simulations
#
# Returns a matrix of length(D) rows and n columns. Large errors in a
# "diamError" model are drawn independently for each stem in each simulation.
simulateD <- function(D, Dpropag, n) {
  len <- length(D)
  if (is.null(Dpropag)) {
    return(matrix(D, nrow = len, ncol = n))
  }
  if (is.character(Dpropag)) {
    # Chave et al. (2004), p. 412
    Dpropag <- diamError(a = 0.0904, b = 0.0062, p_large = 0.05,
      sd_large = 4.64)
  }
  if (inherits(Dpropag, "diamError")) {
    D_sd <- matrix(Dpropag$a + Dpropag$b * D, nrow = len, ncol = n)
    if (Dpropag$p_large > 0) {
      D_sd[runif(len * n) < Dpropag$p_large] <- Dpropag$sd_large
    }
  } else {
    D_sd <- matrix(Dpropag, nrow = len, ncol = n)
  }
  D_simu <- suppressWarnings(myrtruncnorm(len * n, mean = rep(D, n),
    sd = D_sd, lower = 0.1, upper = 500))
  matrix(D_simu, nrow = len, ncol = n)
}

# Truncated normal random values
myrtruncnorm <- function(n, lower = -1, upper = 1, mean = 0, sd = 1) {
  qnorm(p = runif(n = n, min = pnorm(lower, mean = mean, sd = sd),
    max = pnorm(upper, mean = mean, sd = sd)), mean = mean, sd = sd)
}
