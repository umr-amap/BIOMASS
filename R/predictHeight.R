#' Tree height predictions
#'
#' The function predicts height from diameter based on a fitted model.
#'
#' @param D a n x m matrix containing tree diameters (in cm), where n is the number of trees and m is the number of Monte Carlo simulations (m = 1 if no error propagation).
#' @param model A height-diameter model output by the function [modelHD()].
#' @param err If `TRUE`, An error is taken randomly from a normal distribution with a mean of zero and a standard deviation equaled to the residual standard error of the model (RSE). Only used for the Monte Carlo approach (see [AGBmonteCarlo()]), otherwise it should be let as `FALSE`, the default case.
#' @param plot (optional) Plot ID, must be either one value, or a vector of the same length as D. This argument is used to build
#' stand-specific HD models.
#'
#' @details In the case where the error is `FALSE` and the model is a log-log model, we use the
#' Baskerville correction, a bias correction factor used to get unbiased backtransformation values.
#'
#' @return Returns a vector of total tree height (in m).
#' @author Arthur BAILLY
#' @seealso [minpack.lm::nlsLM()]
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom brms is.brmsfit as_draws_df
#' @keywords Internal
predictHeight <- function(D, model, err = FALSE, plot = NULL) {
  
  # Naming D rows to keep the order in case of multiple disordered plots
  rownames(D) <- 1:nrow(D)
  
  # If 'model' contains only one model, put model in a list of one element to keep the same structure as when there are several plots
  if(is.null(plot)) {
    if( all.equal(names(model)[1:4],c("input","model","residuals","method")) ) {
      model <- list(single_plot = model)
    } else {
      stop("The argument model contains different HD models, use the argument plot to assign the corresponding stand-specific model to each tree.")
    }
  }
  
  ### Checks 
  if(!is.null(plot)) {
    if (length(plot) != nrow(D)) stop("The argument plot and D have not the same length")
    # Check that 'model' is a list containing each stand-specific model
    if( length(model) == 1) {
      warning("The 'plot' argument will be ignored because it is used to create stand-specific local H-D models, but your 'model' argument contains only one model.")
    }
    # Check that 'plot' contains all the plots included in 'model'
    if( any( ! names(model) %in% unique(plot) ) ) { # stop if 'plot' does'nt contain all stand-specific HD models
      stop(paste("The 'model' argument contains the following stand specific HD models which are not present in the 'plot' argument:", paste(names(model)[! names(model) %in% unique(plot)], collapse = ", ")))
    }
    # Check that 'model' contains all the plots defined in 'plot'
    if (any(!plot %in% names(model))) {
      stop( paste("Cannot find a HD model corresponding to ", paste(unique(plot[ !plot %in% names(model) ]), collapse = ", ")) )
    }
  }
  
  H_predict <- do.call(rbind, lapply(names(model), function(mod_name) {
    
    if(mod_name == "single_plot") {
      mod <- model[["single_plot"]]
      D_stand <- D
    } else {
      mod <- model[[mod_name]]
      D_stand <- as.matrix(D[as.character(plot) == mod_name, ])
    }
    
    # See man/figures/predictHeight_help.pdf for details 
    
    # If model is a brmsfit model, sample posterior parameters in brm draws
    if(is.brmsfit(mod$model)) {
      param_draws <- as.data.frame(as_draws_df(mod$model))
      n_draws <- nrow(param_draws)
      n_trees <- nrow(D_stand)
      
      if(err) { # if error propagation: draws model coefficients in the posterior distributions
        n_simu <- ncol(D_stand)
        if( n_simu > n_draws ) stop("The number of iterations of the MonteCarlo simulation must be smaller than the number of posterior draws (equals to (iter-warmup) * chains, see brm()) ")
        # Sample n_simu draws 
        param_draws <- param_draws[sample(1:nrow(param_draws), n_simu),
                                   !names(param_draws) %in% c("Intercept","lprior","lp__",".chain",".iteration",".draw")]
        # Convert parameter draws in a n by m matrix where n is the number of trees an m the number of draws (same dimensions as D_stand)
        param_draws <- lapply(param_draws, function(x) matrix(rep(x,n_trees), nrow = n_trees, byrow = TRUE))
        if( !mod$weighted ) { #if no weights were used
          # for each simulation (each draw) apply an error on each line (trees) drawn on sigma draws
          e <- matrix(rnorm(n = length(param_draws$sigma), sd = param_draws$sigma), nrow = n_trees)
        } else { # if weights, sigma is the weighted RSE, hence, we will take the mod$RSE as sigma estimates
          sd_mod <- ifelse(mod$method %in% c("log1","log2") , mod$RSElog, mod$RSE)
          e <- matrix(rnorm(n = n_trees*n_simu, sd = sd_mod), nrow = n_trees)
        }
        
      } else { # if no error propagation, get parameter draws and apply Baskerville correction for log models
        param_draws <- param_draws[!names(param_draws) %in% c("Intercept","lprior","lp__",".chain",".iteration",".draw")]
        # Convert parameter draws in a n by m matrix where n is the number of trees an m the number of draws
        param_draws <- lapply(param_draws, function(x) matrix(rep(x,n_trees), nrow = n_trees, byrow = TRUE))
        # Convert D_stand in a matrix with the same dimensions as above (keeping original rownames)
        D_stand_rownames <- rownames(D_stand)
        D_stand <- matrix(rep(D_stand,n_draws), ncol = n_draws)
        rownames(D_stand) <- D_stand_rownames
        # Baskerville correction for log models
        if(mod$method %in% c("log1","log2")) {
          if( !mod$weighted) { #if no weights were used
            e <- 0.5*param_draws$sigma^2
          } else { # if weights, sigma is the weighted RSE, hence, we will take the mod$RSE as sigma estimates
            e <- 0.5*mod$RSElog^2
          }
        } else { # no Baskerville correction for Michaelis and Weibull models
          e <- 0
        }
      }
      
    } else { # if model is an lm or nls model, retrieve model coefficients
      if(mod$method %in% c("log1","log2")) {
        param_draws <- as.list(mod$model$coefficients)
      } else {
        param_draws <- as.list(mod$model$m$getPars())
      }
      
      if(err) { # if error propagation: sample random errors from the RSE of the model
        sd_mod <- ifelse(mod$method %in% c("log1","log2") , mod$RSElog, mod$RSE)
        e <- matrix( rnorm( n = length(D_stand), sd = sd_mod) , nrow = nrow(D_stand))
        
      } else { # if no error propagation, apply Baskerville correction for log models
        e <- ifelse(mod$method %in% c("log1","log2"), 0.5*mod$RSElog^2, 0)
      }
    }
    
    # Calculating predictions
    if( mod$method == "log1") { # log(H) ~ a + b * log(D)
      H_simu <- exp(param_draws[[1]] + param_draws[[2]] * log(D_stand) + e ) 
    } else if(mod$method == "log2") { # log(H) ~ a + b * log(D) + c * log(D)²
      H_simu <- exp(param_draws[[1]] + param_draws[[2]] * log(D_stand) + param_draws[[3]] * (log(D_stand)^2) + e )
    } else if(mod$method == "michaelis") { # H ~ b1 * D / (b2 + D)
      H_simu <- param_draws[[1]] * D_stand / (param_draws[[2]] + D_stand) + e
    } else if(mod$method == "weibull") { # H ~ a * (1-exp(-(D/b)^c))
      H_simu <- param_draws[[1]] * ( 1 - exp( -(D_stand / param_draws[[2]]) ^ param_draws[[3]]) ) + e
    }
    # if bayesian model without error propagation, get the median of all estimates
    if(is.brmsfit(mod$model) && !err) H_simu <- as.matrix(apply(H_simu , 1, median, na.rm=TRUE))
    as.matrix(H_simu)
  }))
  
  # Retrieve original orders (rownames are kept)
  H_predict <- H_predict[rownames(D),]
  
  # If H predicted values are negative due to random error assignment
  H_predict[H_predict <= 0] <- 0.1
  
  return(H_predict)
}

