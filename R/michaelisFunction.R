#' @rdname HDmethods
#' @return Result of a model (nlsM object if bayesian = FALSE, brm object if bayesian = TRUE)
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom methods is
#' @importFrom stats update gaussian

michaelisFunction <- function(data, weight = NULL, bayesian, useCache, chains, thin, iter, warmup, ...) {
  
  ### Compute the michaelis model of the H-D relationship
  
  if(bayesian){ # using brms
    
    if (is.null(weight)) {
      cache_path <- cacheManager(nameFile = "michaelis_weights_F_model.rds")
      bf_formula <- brms::brmsformula( H ~ A * D / (B + D), A + B ~ 1, nl=TRUE)
    } else {
      cache_path <- cacheManager(nameFile = "michaelis_weights_T_model.rds")
      data$weight <- data$D^2 * data$H
      bf_formula <- brms::brmsformula( H | weights(weight, scale = TRUE) ~ A * D / (B + D), A + B ~ 1, nl=TRUE)
    }
    
    if( is.null(useCache) ) { # if useCache = NULL -> remove cache
      file.remove(cache_path)
      useCache <- TRUE
    } 
    
    if (useCache & file.exists(cache_path) ) { # if the model has already been build (and compiled)
      
      message("Loading Michaelis-Menten model using the cache...")
      mod <- readRDS(cache_path)
      mod <- update(mod, newdata = data, chains = chains, thin = thin, iter = iter, warmup = warmup, ...)
      message(paste("Saving the updated H-D model in",cache_path,"\n"))
      saveRDS(mod, file = cache_path)
      
    } else { # else, build the model (and save it as .rds if useCache = TRUE)
      
      message("Building Michaelis-Menten model using brms library.")
      message("Compiling the Stan programme may take some time the first time around. Next time, consider using 'useCache' = TRUE to avoid this compilation time.")
      mod <- brms::brm(data = data,
                       family = gaussian(link = "identity"),
                       formula = bf_formula,
                       file = cache_path,
                       chains = chains, thin = thin, iter = iter, warmup = warmup,
                       ...)
      if(!useCache) {
        message(paste("Saving the H-D model in",cache_path,"\n"))
        saveRDS(mod, file = cache_path)
      }
    }
    
  } else {
    
    H <- data$H
    D <- data$D
    
    count <- 1
    maxIter <- 50
    converge <- FALSE
    
    while (converge == FALSE && count <= 10) {
      tt <- tryCatch({
        if (is.null(weight)) {
          nlsLM(H ~ SSmicmen(D, A, B),
                control = nls.lm.control(maxiter = maxIter)
          )
        } else {
          nlsLM(H ~ SSmicmen(D, A, B),
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
      }
      else {
        converge <- TRUE
      }
    }
    mod <- if (is.null(weight)) {
      nlsLM(H ~ SSmicmen(D, A, B),
            control = nls.lm.control(maxiter = maxIter)
      )
    } else {
      nlsLM(H ~ SSmicmen(D, A, B),
            weights = weight,
            control = nls.lm.control(maxiter = maxIter)
      )
    }
  }
  
  return(mod)
}