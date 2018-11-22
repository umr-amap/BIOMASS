#' Predicting tree height
#'
#' The function predicts height from diameter based on a fitted model.
#'
#' @param D Vector of diameter (in cm).
#' @param model A height-diameter model output by the function \code{link{modelHD}}
#' @param err If \code{TRUE}, An error is taken randomly from a normal distribution with a mean of
#' zero and a standard deviation equalled to the residual standard error of the model (RSE). Only used
#' for the Monte Carlo approach (see \code{\link{AGBmonteCarlo}}), otherwise it should be
#' let as \code{FALSE}, the default case.
#'
#' @details In the case where the error is \code{FALSE} and the model is a log-log model, we use the
#' Baskerville correction, a bias correction factor used to get unbiased backtransformation values.
#'
#' @return Returns a vector of total tree height (in m).
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY, Arthur PERE
#' @seealso \code{\link[minpack.lm]{nlsLM}}
#'
#'
#'
#' @keywords Internal
#'
#'
predictHeight <- function(D, model, err = FALSE) {
  ### From the diameter and the model, compute (with or without error) the heigth
  method <- model$method
  logmod <- any(grepl("log", method))

  if (!err) {
    if (logmod) {
      e <- 0.5 * model$RSElog^2 # Baskerville correction
    } else {
      e <- 0 # No error
    }
  } else {
    if (logmod) {
      e <- rnorm(length(D), 0, model$RSElog) # Log-log error
    } else {
      e <- rnorm(length(D), 0, model$RSE) # Michaelis or Weibull error
    }
  }

  if (logmod) {
    logD <- data.frame(logD = log(D))
    Hpredict <- exp(predict(model$model, data.frame(logD = log(D))) + e)
  } else {
    Hpredict <- predict(model$model, data.frame(D = D)) + e
  }

  names(Hpredict) <- NULL

  # If H predicted values are negative due to random error assignment
  Hpredict[Hpredict <= 0] <- 0.1

  return(as.vector(Hpredict))
}
