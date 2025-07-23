#' @rdname HDmethods
#' @return Result of a model (nlsM object if bayesian = FALSE, brm object if bayesian = TRUE)
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom methods is
#' @importFrom stats update
#' @importFrom brms brm brmsformula

weibullFunction <- function(data, weight = NULL, bayesian, useCache, chains, thin, iter, warmup, ...) {
  
  ### Compute the weibull model of the H-D relationship
  
  if(bayesian){ # using brms
    
    if (is.null(weight)) {
      cache_path <- cacheManager(nameFile = "weibull_weights_F_model.rds")
      bf_formula <- brmsformula(H ~ (a * (1-exp(-(D/b)^c))), a + b + c ~ 1, nl = TRUE)
    } else {
      cache_path <- cacheManager(nameFile = "weibull_weights_T_model.rds")
      data$weight <- data$D^2 * data$H
      bf_formula <- brmsformula( H | weights(weight, scale = TRUE) ~ a * (1-exp(-(D/b)^c)), a + b + c ~ 1, nl=TRUE)
    }
    
    if( is.null(useCache) ) { # if useCache = NULL -> remove cache
      file.remove(cache_path)
      useCache <- TRUE
    } 
    
    if (useCache & file.exists(cache_path) ) { # if the model has already been build (and compiled)
      
      message("Loading Weibull model using the cache...")
      mod <- readRDS(cache_path)
      mod <- update(mod, newdata = data, refresh=0, chains = chains, thin = thin, iter = iter, warmup = warmup, ...)
      
    } else { # else, build the model (and save it as .rds if useCache = TRUE)
      
      message("Building Weibull model using brms library.")
      message("Compiling the Stan programme may take some time the first time around. Next time, consider using 'useCache' = TRUE to avoid this compilation time.")
      if(!useCache) {
        cache_path <- NULL
      }
      mod <- brm(data = data,
                 family = gaussian(link = "identity"),
                 formula = bf_formula,
                 file = cache_path,
                 chains = chains, thin = thin, iter = iter, warmup = warmup,
                 ...)
    }
    
  } else { # using nlsLM
    
    H <- data$H
    D <- data$D
    
    Hmax <- quantile(H, probs = 0.90, na.rm = TRUE)
    init <- list(a = as.double(Hmax), b = 24.9, c = 0.8)
    
    count <- 1
    maxIter <- 50
    converge <- FALSE
    
    while (converge == FALSE && count <= 10) {
      tt <- tryCatch({
        if (is.null(weight)) {
          nlsLM(H ~ a * (1 - exp(-(D / b)^c)),
                start = init,
                control = nls.lm.control(maxiter = maxIter)
          )
        } else {
          nlsLM(H ~ a * (1 - exp(-(D / b)^c)),
                start = init,
                weights = weight,
                control = nls.lm.control(maxiter = maxIter)
          )
        }
      },
      error = function(e) e,
      warning = function(w) w
      )
      
      if (is(tt, "warning")) {
        count <- count + 1
        maxIter <- maxIter + 50
      } else {
        converge <- TRUE
      }
    }
    
    mod <- if (is.null(weight)) {
      nlsLM(H ~ a * (1 - exp(-(D / b)^c)),
            start = init,
            control = nls.lm.control(maxiter = maxIter)
      )
    } else {
      nlsLM(H ~ a * (1 - exp(-(D / b)^c)),
            start = init,
            weights = weight,
            control = nls.lm.control(maxiter = maxIter)
      )
    }
  }
  
  return(mod)
}