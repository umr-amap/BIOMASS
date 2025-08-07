#' Fitting height-diameter models
#'
#' @description This function fits and compares (optional) height-diameter models.
#'
#' @param D Vector with diameter measurements (in cm). NA values are accepted but a minimum of 10 valid entries (i.e. having a corresponding height in H) is required.
#' @param H Vector with total height measurements (in m). NA values are accepted but a minimum of 10 valid entries (i.e. having a corresponding diameter in D) is required.
#' @param method Method used to fit the relationship.
#' To be chosen between:
#'   - log1, log2
#'     + log 1: \eqn{(log(H) = a+ b*log(D))} (equivalent to a power model)
#'     + log 2: \eqn{(log(H) = a+ b*log(D) + c*log(D)^2)}
#'   - weibull: \eqn{H = a*(1-exp(-(D/b)^c))}
#'   - michaelis: \eqn{H = (A * D)/(B + D)}
#'
#' If `NULL`, all the methods will be compared.
#' @param useWeight If weight is `TRUE`, model weights will be \eqn{(D^2)*H} (i.e. weights are proportional to tree
#' volume, so that larger trees have a stronger influence during the construction of the model).
#' @param drawGraph If `TRUE`, a graphic will illustrate the relationship between H and D. Only if argument `plot` is null.
#' @param plot (optional) a vector of character containing the plot ID's of the trees (linked to D and H). Must be either one value, or a vector of the same length as D. This argument is used to build stand-specific HD models.
#' @param bayesian a logical. If FALSE (by default) the model is estimated using a frequentist framework (lm or nls). If TRUE, the model is estimated in a Bayesian framework using the brms package.
#' @param useCache a logical. If bayesian = TRUE, determine wether to use the cache when building a Bayesian model (see Details).
#' @param chains (only relevant if bayesian = TRUE): Number of Markov chains (defaults to 3), see [brms::brm()]
#' @param thin (only relevant if bayesian = TRUE): Thinning rate, see [brms::brm()]
#' @param iter (only relevant if bayesian = TRUE): number of total iterations per chain (including warmup; defaults to 5000), see [brms::brm()]
#' @param warmup (only relevant if bayesian = TRUE): number of warmup (aka burnin) iterations (defaults to 1000), see [brms::brm()]
#' @param ... Further arguments passed to `brm()`, e.g: prior, cores, etc. See [brms::brm()]
#'
#' @details All the back transformations for log-log models are done using the Baskerville correction (\eqn{0.5 * RSE^2},
#' where RSE is the Residual Standard Error).
#' 
#' If useCache = TRUE and this is the first time the model is being built, the model will be saved as a .rds file in the defined cache path (see [createCache()]).
#' If useCache = TRUE and the model has already been built using the user cache, the model will be loaded and updated to avoid wasting time re-compiling it.
#' If useCache = NULL, the cache is first cleared before building the model.
#'
#' @return
#' If `plot` is NULL or has a single value, a single list is returned. If there is more than one plot,
#' multiple embedded lists are returned with plots as the list names.
#' 
#' If `model` is not null (model comparison), returns a list :
#'   - `input`: list of the data used to construct the model (list(H, D))
#'   - `model`: outputs of the model (same outputs as given by [stats::lm()], [stats::nls()])
#'   - `residuals`: Residuals of the model
#'   - `method`: Name of the method used to construct the model
#'   - `predicted`: Predicted height values
#'   - `RSE`: Residual Standard Error of the model
#'   - `RSElog`: Residual Standard Error of the log model (\code{NULL} if other model)
#'   - `fitPlot`: a ggplot object containing the model fitting plot
#'   - `weighted`: a logical indicating whether weights were used during the fit
#'
#'
#' If the parameter model is null, the function return a plot with all the methods for
#' comparison, the function also returns a data.frame with:
#'   - `method`: The method that had been used to construct the plot
#'   - `RSE`: Residual Standard Error of the model
#'   - `RSElog`: Residual Standard Error of the log model (`NULL` if other model)
#'   - `Average_bias`: The average bias for the model
#'
#'
#'
#' @author Maxime REJOU-MECHAIN, Arthur PERE, Ariane TANGUY, Arthur Bailly
#' @seealso [retrieveH()]
#'
#' @export
#'
#' @examples
#'
#' # Load a data set
#' data(NouraguesHD)
#'
#' # Fit H-D models for the Nouragues dataset
#' \donttest{ HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, drawGraph = TRUE) }
#'
#' # For a selected model
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H,
#'                    method = "log2", drawGraph = TRUE)
#'
#' # Using weights
#' HDmodel <- modelHD(
#'   D = NouraguesHD$D, H = NouraguesHD$H,
#'   method = "log2", useWeight = TRUE,
#'   drawGraph = TRUE)
#' 
#' # With multiple stands (plots)
#' HDmodel <- modelHD(
#'   D = NouraguesHD$D, H = NouraguesHD$H,
#'   method = "log2", useWeight = TRUE, 
#'   plot = NouraguesHD$plotId, drawGraph = TRUE)
#' 
#' ### Using log2 bayesian model
#' \donttest{HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, 
#'   method = "log2", bayesian = TRUE, useCache = TRUE)
#' plot(HDmodel$model) }
#'   
#' ### Using weibull bayesian model (time consuming)
#' # As the algorithm is likely to find numerous local minima,
#' # defining priors is strongly recommended (see "Some tricks" part in the vignette)
#' # Also, since  model parameters and chain iterations are strongly correlated,
#' # an increase of 'thin', 'iter' and 'warmup' may be required.
#' \donttest{HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, 
#'  method = "weibull", bayesian = TRUE, useCache = TRUE,
#'  thin = 20, iter = 12000, warmup = 2000,
#'  prior = c(brms::set_prior(prior = "uniform(0,80)",
#'                            lb = 0, ub = 80, class = "b", nlpar = "a"),
#'            brms::set_prior(prior = "uniform(0,100)",
#'                            lb = 0, ub = 100, class = "b", nlpar = "b"),
#'            brms::set_prior(prior = "uniform(0.1,0.9)",
#'                            lb = 0.1, ub = 0.9, class = "b", nlpar = "c"))) }
#' 
#' @importFrom graphics legend lines par plot grid axis
#' @importFrom stats SSmicmen lm median na.omit quantile rnorm sd predict coef
#' @importFrom utils data
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal theme scale_x_continuous scale_y_continuous element_text 

modelHD <- function(D, H, method = NULL, useWeight = FALSE, drawGraph = FALSE, plot = NULL, bayesian = FALSE, useCache = FALSE, chains = 3, thin = 5, iter = 5000, warmup = 500, ...) {
  
  # Checking arguments -------------------------------------------------
  
  # Check if there is enough data to compute an accurate model
  nbNonNA <- sum(!is.na(H))
  if (nbNonNA < 15) {
    stop("The data has not enough height data (less than 15 non NA)")
  }
  if (length(H) != length(D)) {
    stop("Your vector D and H do not have the same length")
  }
  if (!is.null(method)) {
    method <- tolower(method)
  }
  methods <- c("log1", "log2", "weibull", "michaelis")
  if (!is.null(method) && !(method %in% methods)) {
    stop("Chose your method among those ones : ", paste(methods, collapse = ", "))
  }
  if (!is.logical(useWeight)) {
    stop("UseWeight argument must be a boolean")
  }
  if (!is.logical(drawGraph)) {
    stop("drawGraph argument must be a boolean")
  }
  if (!is.null(plot) && !length(plot) %in% c(1, length(D))) {
    stop("The length of the 'plot' vector must be either 1 or the length of D")
  }
  # Check if package brms is available
  if (!requireNamespace("brms", quietly = TRUE)) {
    warning(
      'To build bayesian models, you must install the "brms" library \n\n',
      '\t\tinstall.packages("brms")'
    )
    return(invisible(NULL))
  }
  
  # Multiple plots management ---------------------------------------------------
  
  # if there is multiple plots in the plot vector
  if (!is.null(plot) && length(unique(plot)) != 1) {
    Hdata <- data.table(H = H, D = D, plot = plot)
    
    output <- lapply(split(Hdata, by = "plot", keep.by = TRUE), function(subData) {
      if(bayesian) {
        message(paste("Considering stand",unique(subData$plot)))
      }
      modelHD(
        D = subData$D, H = subData$H,
        method = method, useWeight = useWeight,
        drawGraph = drawGraph, plot = unique(subData$plot),
        bayesian = bayesian, useCache = useCache,
        chains = chains, thin = thin, iter = iter, warmup = warmup, ...)
    })
    
    if (is.null(method)) {
      message("To build a HD model you must use the parameter 'method' in this function")
    }
    
    return(output)
  }
  
  
  # functions ----------------------------------------------------------------
  
  # function to select the method
  modSelect <- function(Hdata, weight, method, useGraph = FALSE, bayesian, useCache, chains, thin, iter, warmup, ...) {
    output <- list()
    
    ################## Log-log model
    if (grepl("log", method)) {
      mod <- loglogFunction(data = Hdata, weight, method, bayesian, useCache, chains, thin, iter, warmup, ...)
      
      if(!bayesian) {
        if(is.null(weight)) {
          output$RSElog <- summary(mod)$sigma
        } else { # if weighted model, summary(mod)$sigma is not the appropriate RSE
          res <- na.omit(log(Hdata$H)) - predict(mod)
          output$RSElog <- sqrt(sum(res^2) / summary(mod)$df[2])
        }
        # Baskerville correction 1972
        output$Hpredict <- exp(predict(mod) + 0.5 * output$RSElog^2)
      } else {
        if(is.null(weight)) {
          output$RSElog <- summary(mod)$spec_pars$Estimate
        } else { # if weighted model, summary(mod)$spec_pars$Estimate is not the appropriate RSE
          res <- na.omit(log(Hdata$H)) - predict(mod)[,1]
          output$RSElog <- sqrt(sum(res^2) / summary(mod)$nobs)  
        }
        output$Hpredict <- exp(predict(mod)[,1] + 0.5 * output$RSElog^2)
      }
      
      if (useGraph) {
        if(!bayesian) {
          output$Hpredict_plot <- exp(predict(mod, newdata = D_Plot) + 0.5 * output$RSElog^2)
        } else {
          output$Hpredict_plot <- exp(predict(mod, newdata = D_Plot)[,1] + 0.5 * output$RSElog^2)
        }
      }
      
    } else {
      ######### The others HD models
      mod <- switch(method,
                    michaelis = michaelisFunction(data = Hdata, weight, bayesian, useCache, chains, thin, iter, warmup, ...), # Michaelis-Menten function
                    weibull = weibullFunction(data = Hdata, weight, bayesian, useCache, chains, thin, iter, warmup, ...) # Weibull 3 parameters
      )
      
      if(!bayesian) {
        output$Hpredict <- predict(mod)
        if (useGraph) {
          output$Hpredict_plot <- predict(mod, newdata = D_Plot)
        }
      } else {
        output$Hpredict <- predict(mod)[,1]
        if (useGraph) {
          output$Hpredict_plot <- predict(mod, newdata = D_Plot)[,1]
        }
      }
      output$RSElog <- NA_real_
      
    }
    
    names(output$Hpredict) <- NULL
    res <- na.omit(Hdata$H) - output$Hpredict 
    
    output$method <- method
    if(!bayesian) {
      output$RSE <- sqrt(sum(res^2) / summary(mod)$df[2]) # Residual standard error
    } else {
      output$RSE <- sqrt(sum(res^2) / summary(mod)$nobs) # Residual standard error
    }
    output$Average_bias <- (mean(output$Hpredict) - mean(Hdata$H, na.rm = TRUE)) / mean(Hdata$H, na.rm = TRUE)
    output$residuals <- res
    output$mod <- mod
    
    return(output)
  }
  
  
  # function to draw the base of the graph
  drawPlotBegin <- function(givenMethod = FALSE, plotId) {
    main_title <- ifelse(givenMethod == FALSE, "Model comparison", paste("Selected model : ", givenMethod))
    
    starting_plot <- ggplot(data = na.omit(Hdata), mapping = aes(x=D, y=H)) + 
      geom_point(col="grey50") + 
      labs(title = main_title, x="D (cm)", y="H (m)") + 
      scale_x_continuous(transform = "log10", n.breaks = 8, ) +
      scale_y_continuous(transform = "log10", n.breaks = 8) + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    return(starting_plot)
  }
  
  
  # Data processing ---------------------------------------------------------
  Hdata <- data.table(H = H, D = D)
  weight <- NULL
  
  # Warn if there is less than 2 diameter values in the following quantile intervals : [0-0.5], ]0.5;0.75] and ]0.75,1]
  if ( any( table(findInterval(D, c(-1, quantile(D, probs = c(0.5, 0.75)), max(D) + 1))) < 3 ) ) {
    if(is.null(plot)) {
      warning("Be careful, your diameter values are not evenly distributed. You should check their distribution.")
    } else {
      warning(paste("Be careful, in plot", unique(plot), "your diameter values are not evenly distributed. You should check their distribution."))
    }
  }
  
  # Vector of diameter used only for visualisation purpose
  D_Plot <- data.frame(D = Hdata[, 10^seq(log10(floor(min(D))), log10(ceiling(max(D))), l=100 )])
  
  # If the measures need to be weighted
  if (useWeight == TRUE) {
    weight <- Hdata[, D^2 * H]
    weight <- weight/sum(weight, na.rm = TRUE)
  } # weight is proportional to tree volume
  
  
  # If one method is supplied  ----------------------------------------------
  if (!is.null(method)) {
    output <- modSelect(Hdata = Hdata, weight = weight, method = method, useGraph = drawGraph, bayesian = bayesian, useCache = useCache, chains = chains, thin = thin, iter = iter, warmup = warmup, ...)
    
    ################## Return the selected model
    out <- list(
      input = list(H = Hdata$H, D = Hdata$D),
      model = output$mod,
      residuals = output$residuals,
      method = method,
      predicted = output$Hpredict,
      RSE = output$RSE
    )
    
    if (grepl("log", method)) {
      out$RSElog <- output$RSElog
    }
    
    ####### if drawGraph is true
    if (drawGraph) {
      fitPlot <- drawPlotBegin(method, plot) + 
        geom_smooth(
          data = data.frame(x = D_Plot$D, y = output$Hpredict_plot), mapping = aes(x,y),
          formula = y~x, method = "loess"
        )
      print(fitPlot)
      out$fitPlot <- fitPlot
    }
    
    out$weighted <- useWeight
    
    return(out)
  } else { # If no method was supplied
    
    # Compare Models ----------------------------------------------------------
    
    output_list <- list()
    df_plot_list <- list()
    for(i in 1:length(methods)) {
      method <- methods[i]
      
      out <- modSelect(Hdata, weight = weight, method = method, useGraph = drawGraph, bayesian = bayesian, ...)
      
      output_list[[i]] <- list(
        method = method,
        RSE = out$RSE, # Residual standard error
        RSElog = out$RSElog,
        Average_bias = out$Average_bias
      )
      
      if (drawGraph) {
        df_plot_list[[i]] <- data.frame(x = D_Plot$D, y = out$Hpredict_plot, method=methods[i])
      }
    }
    result <- rbindlist(output_list,fill=T)
    
    if (drawGraph) {
      fitPlot <- drawPlotBegin(plotId = plot)
      df_plot <- rbindlist(df_plot_list)
      fitPlot <- fitPlot +  
        geom_smooth(
          data = df_plot, mapping = aes(x,y,colour = method,linetype = method),
          formula = y~x, method = "loess") +
        theme(legend.position = "bottom")
      print(fitPlot)
    }
    
    message("To build a HD model you must use the parameter 'method' in this function")
    
    
    return(data.frame(result))
  }
}