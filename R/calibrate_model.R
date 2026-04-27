#' Calibrate a bayesian model to fit log(AGBD) ~ log(raster metric) 
#'
#' @description 
#' After applying the [subplot_summary()] function, this function fits a log-log bayesian regression model with spatially varying coefficient process, on AGBD and raster metric simulated values (see Details).
#' 
#' @param long_AGB_simu The '$long_AGB_simu' output of the [subplot_summary()] function (see Details).
#' @param nb_rep Number of simulation to provide in the brms fit (defaults to 30; nb_rep > 50 will not improved significantly the model and will be much longer to fit).
#' @param useCache A logical that determines wether to use the cache when building a Bayesian model (see Details).
#' @param plot_model A logical indicating whether the model should be plotted (defaults to TRUE).
#' @param intercept A logical indicating whether the regression model should include an intercept (defaults to FALSE).
#' @param chains Number of Markov chains (defaults to 3), see [brms::brm()]
#' @param thin Thinning rate (defaults to 20), see [brms::brm()]
#' @param iter Number of total iterations per chain (including warmup; defaults to 3000), see [brms::brm()]
#' @param warmup Number of warmup (aka burnin) iterations (defaults to 1000), see [brms::brm()]
#' @param cores Number of cores to use when executing the chains in parallel (defaults to 3), see [brms::brm()]
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
#' 
#' The model describing the relationship between plot-level AGBD and LiDAR metrics is a log-log regression, 
#' with a Gaussian error model. To capture the spatial structure that may exists in the data, we use a 
#' Spatially Varying Coefficient (SVC) regression with Gaussian random fields (Gelfand et al. 2003, 
#' Spatial modeling with spatially varying coefficient processes).
#' 
#' The general equation can be written as follow, for a subplot \eqn{s_i}:
#'  
#' \eqn{Y_i \sim \mathrm{N}(\mu_i, \sigma)}
#' 
#' \eqn{\mu_i = \beta_0 + (\beta_1 + \eta_i) \times X_i}
#' 
#' \eqn{\eta_i \sim \mathrm{MVNormal}(0, \Sigma)}
#' 
#' \eqn{X_i} stands for the logarithm of plot-level AGBD, \eqn{Y_i} is the logarithm of 
#' a LiDAR metric measurement for the corresponding plot.
#' 
#' \eqn{\Sigma}, the covariance matrix, is defined by the \eqn{\frac{3}{2}} Matern kernel between 
#' two locations \eqn{s_i} and \eqn{s_j}:
#' \eqn{k(\mathbf{s}_i, \mathbf{s}_j) = \psi^2 \left( 1 + \frac{\sqrt{3}d_{i,j}}{l} \right) \exp \left( -\frac{\sqrt{3}d_{i,j}}{l} \right)}
#' \eqn{d_{i,j}} is the distance between locations \eqn{s_i} and \eqn{s_j}, parameter \eqn{\psi} controls 
#' the magnitude and parameter \eqn{l} the range of the kernel.
#' 
#' 
#' If useCache = TRUE and this is the first time the model is being built, the model will be saved as a .rds file in the defined cache path (see [createCache()]).
#' If useCache = TRUE and the model has already been built using the user cache, the model will be loaded and updated to avoid wasting time re-compiling it.
#' If useCache = NULL, the cache is first cleared before building the model.
#'
#' @export
#' 
#' @author Arthur BAILLY, Dominique LAMONICA
#'
#' @importFrom data.table is.data.table copy setnames
#' 

calibrate_model <- function(long_AGB_simu, nb_rep = 30, useCache = FALSE, plot_model = TRUE, intercept = FALSE, chains = 3, thin = 20, iter = 3000, warmup = 1000, cores = 3, ...) {
  
  # Checking arguments ---------------------------------------------------------
  if (max(long_AGB_simu$N_simu) < nb_rep){
    stop("The number of simulated AGBD values per subplot in long_AGB_simu should be higher (or equal) to nb_rep")
  }
  
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
  
  # subplot_ID as a factor
  dt_inf[, subplot_ID := as.factor(subplot_ID)]
  # rename x_center and y_center
  setnames(x = dt_inf, old = c("x_center","y_center"), new = c("x","y"))
  
  # select number of simulations
  dt_inf <- dt_inf[N_simu %in% sample(1:max(N_simu), nb_rep),]
  
  # Fit SVC model with brms ----------------------------------------------------
  
  cache_path <- cacheManager(nameFile = "brms_fit.rds")
  
  if( is.null(useCache) ) { # if useCache = NULL -> remove cache
    file.remove(cache_path)
    useCache <- TRUE
  }
  
  if (intercept){
    bf_formula <- brms::bf(log_AGBD  ~ beta0 + betatilde * log_CHM,
                           betatilde ~ 1 + gp(x, y, gr = T, scale = T,
                                              cov = "matern32"),
                           beta0 ~ 1,
                           nl = T
    )
    
  }else{
  bf_formula <- brms::bf(log_AGBD  ~  0 + betatilde * log_CHM,
                         betatilde ~ 1 + gp(x, y, gr = T, scale = T,
                                            cov = "matern32"),
                         nl = T
  )
  }
  
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
                          control = list(adapt_delta = 0.92, max_treedepth = 14 )
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
