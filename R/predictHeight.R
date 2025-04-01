#' Tree height predictions
#'
#' The function predicts height from diameter based on a fitted model.
#'
#' @param D Vector of diameter (in cm).
#' @param model A height-diameter model output by the function [modelHD()]
#' @param err If `TRUE`, An error is taken randomly from a normal distribution with a mean of
#' zero and a standard deviation equalled to the residual standard error of the model (RSE). Only used
#' for the Monte Carlo approach (see [AGBmonteCarlo()]), otherwise it should be
#' let as `FALSE`, the default case.
#' @param plot (optional) Plot ID, must be either one value, or a vector of the same length as D. This argument is used to build
#' stand-specific HD models.
#'
#' @details In the case where the error is `FALSE` and the model is a log-log model, we use the
#' Baskerville correction, a bias correction factor used to get unbiased backtransformation values.
#'
#' @return Returns a vector of total tree height (in m).
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY, Arthur PERE
#' @seealso [minpack.lm::nlsLM()]
#'
#' @export
#' @importFrom data.table data.table
#' @keywords Internal
predictHeight <- function(D, model, err = FALSE, plot = NULL) {
  ### From the diameter and the model, compute (with or without error) the height

  if(!is.null(plot)) { # if 'plot' is provided 

    if(! "method" %in% names(model[[1]]) ) { # Check that model is a list containing each stand-specific model
      stop("The 'plot' argument is used to create stand-specific local H-D models but your 'model' argument contains only one model.")
    }
    
    if( any( ! names(model) %in% unique(plot) ) ) { # stop if 'plot' does'nt contain all stand-specific HD models
      stop(paste("The 'model' argument contains the following stand specific HD models which are not present in the 'plot' argument:", paste(names(model)[! names(model) %in% unique(plot)], collapse = ", ")))
    }
    # get the method used to build the models
    method <- unique(sapply(model, function(x) x$method))
  } else { # if plot is not provided
    method <- model$method
  }
  
  logmod <- any(grepl("log", method))

  if (is.null(plot) && "method" %in% names(model[[1]]) ) {
    stop("The argument model contains different HD models, use the argument plot to assign a given model to the trees")
  }

  if (!is.null(plot) && "method" %in% names(model[[1]]) ) {
    if (length(plot) == 1) {
      plot <- rep(plot, length(D))
    }

    if (length(plot) != length(D)) {
      stop("The argument plot and D have not the same length")
    }

    if (any(!plot %in% names(model))) {
      stop(
        paste("Cannot find a HD model corresponding to ", paste(unique(plot[ !plot %in% names(model) ]), collapse = ", "))
      )
    }


    data <- data.table(D = D, plot = as.character(plot))

    data[, H := predictHeight(D, model = model[[unique(plot)]], err), by = plot]

    return(data[, H])
  }


  D <- data.table(D = D)

  if (!err) {
    if (logmod) {
      e <- 0.5 * model$RSElog^2 # Baskerville correction
    } else {
      e <- 0 # No error
    }
  } else {
    if (logmod) {
      e <- rnorm(nrow(D), 0, model$RSElog) # Log-log error
    } else {
      e <- rnorm(nrow(D), 0, model$RSE) # Michaelis or Weibull error
    }
  }

  if (logmod) {
    Hpredict <- exp(predict(model$model, D) + e)
  } else {
    Hpredict <- predict(model$model, D) + e
  }

  names(Hpredict) <- NULL

  # If H predicted values are negative due to random error assignment
  Hpredict[Hpredict <= 0] <- 0.1

  return(Hpredict)
}
