#' Calibrate a bayesian model to fit log(AGBD) ~ log(raster metric) 
#'
#' @description 
#' After applying the [subplot_summary()] function, this function fits a log-log bayesian regression model with spatially varying coefficient process, on AGBD and raster metric simulated values (see Details).
#' 
#' @param long_AGB_simu The '$long_AGB_simu' output of the [subplot_summary()] function (see Details).
#' @param nb_rep Number of simulation to provide in the brms fit (nb_rep > 50 will not improved significantly the model and will be much longer to fit).
#' @param useCache A logical that determines wether to use the cache when building a Bayesian model (see Details).
#' @param plot_model A logical indicating whether the model should be plot.
#' @param chains Number of Markov chains (defaults to 3), see [brms::brm()]
#' @param thin Thinning rate, see [brms::brm()]
#' @param iter Number of total iterations per chain (including warmup; defaults to 5000), see [brms::brm()]
#' @param warmup Number of warmup (aka burnin) iterations (defaults to 1000), see [brms::brm()]
#' @param cores Number of cores to use when executing the chains in parallel, see [brms::brm()]
#' @param ... Further arguments passed to `brm()`, e.g: prior, cores, etc. See [brms::brm()]
#'
#' @return
#' The function return a brmsfit object. 
#' 
#' @details
#' The 'long_AGB_simu' argument must be a data frame or data frame extension containing the following variables: 
#'  - 'N_simu': a numeric indicating the simulation number.
#'  - 'x_center' and 'y_center': the coordinates of the plots/subplots in the projected coordinate system.
#'  - 'AGBD': the AGBD value of the simulation.
#'  - 'raster_metric': the raster metric value of the simulation.
#' 
#' Speak about the model (fixed intercept), capturing the spatial autocorrelation, and cite Gelfand et al. 2003 (Spatial modeling with spatially varying coefficient processes)
#' 
#' If useCache = TRUE and this is the first time the model is being built, the model will be saved as a .rds file in the defined cache path (see [createCache()]).
#' If useCache = TRUE and the model has already been built using the user cache, the model will be loaded and updated to avoid wasting time re-compiling it.
#' If useCache = NULL, the cache is first cleared before building the model.
#'
#' @export
#' 
#' @author Arthur Bailly
#'
#' @importFrom data.table is.data.table copy
#' 
#' @examples
#' \dontrun{
#' 
#' }

calibrate_model <- function(long_AGB_simu, nb_rep = 30, useCache = FALSE, plot_model = TRUE, chains = 4, thin = 20, iter = 2300, warmup = 300, cores = 4, ...) {
  
  # Checking arguments ---------------------------------------------------------
  
  
  # Check if package brms is available
  if (!requireNamespace("brms", quietly = TRUE)) {
    warning(
      'To build bayesian models, you must install the "brms" library \n\n',
      '\t\tinstall.packages("brms")'
    )
    return(invisible(NULL))
  }
  
  # Data processing ------------------------------------------------------------
  if(is.data.table(long_AGB_simu)) {
    dt_inf <- copy(long_AGB_simu)
  }
  
  # Log the AGBD and raster metric
  dt_inf[, c("log_AGBD","log_CHM") := list( log(AGBD), log(raster_metric) ) ]
  
  # If 'raster_metric' was the result of simulated corner coordinates
  dt_inf[, log_CHM := median(log_CHM) , by = subplot_ID]
  
  # Convert projected coordinates in km
  dt_inf[, c("X_km", "Y_km", "subplot_ID") := list(x_center/1000, y_center/1000, as.factor(subplot_ID))]
  
  # select number of simulations
  dt_inf <- dt_inf[N_simu %in% sample(1:200, nb_rep),]
  
  
  # Fit SVC model with brms ----------------------------------------------------
  
  cache_path <- cacheManager(nameFile = "brms_fit.rds")
  
  if( is.null(useCache) ) { # if useCache = NULL -> remove cache
    file.remove(cache_path)
    useCache <- TRUE
  }
  
  bf_formula <- brms::bf(log_AGBD  ~  0 + betatilde * log_CHM,
                         betatilde ~ 1 + gp(X_km, Y_km, gr = T, scale = T,
                                            cov = "matern32"),
                         nl = T
  )
  
  if (useCache & file.exists(cache_path) ) { # if the model has already been build (and compiled)
    message(paste("Loading SVC brms model using the cache..."))
    fit_brms <- readRDS(cache_path)
    fit_brms <- update(fit_brms, newdata = dt_inf, chains = chains, thin = thin, iter = iter, warmup = warmup, cores = cores, ...)
    saveRDS(fit_brms, file = cache_path)
    message(paste("Saving SVC brms model udpated in",cache_path))
  } else { # else, build the model (and save it as .rds if useCache = TRUE)
    
    message(paste("Building SVC model using brms library."))
    message("Compiling the Stan programme may take some time the first time around. Next time, consider using 'useCache' = TRUE to avoid this compilation time.")
    fit_brms <- brms::brm(data = dt_inf,
                          family = gaussian(link = "identity"),
                          formula = bf_formula,
                          iter = iter, warmup = warmup, chains = chains, cores = cores, thin = thin,
                          control = list(adapt_delta = 0.9, max_treedepth = 14 )
    )
    if(useCache) {
      saveRDS(fit_brms, file = cache_path)
      message(paste("Saving SVC brms model in",cache_path))
    }
  }
  
  if(plot_model) {
    plot(fit_brms)
  }
  
  return(fit_brms)
  
}
