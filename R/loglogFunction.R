#' @rdname HDmethods
#' @return Result of a model (lm object if bayesian = FALSE, brm object if bayesian = TRUE)
#' @importFrom stats formula as.formula update gaussian

loglogFunction <- function(data, weight = NULL, method, bayesian, useCache, chains, thin, iter, warmup, ...) {
  ### Compute the loglog model of the H-D relationship
  
  # take the pow of the method
  method_pow <- strtoi(substr(method, nchar(method), nchar(method)))
  
  # rigth part of the formula, i.e I(log(D)^1) + I(log(D)^2) + ...
  end_formula <- paste(sapply(
    seq(method_pow),
    function(x) {
      sprintf("I(log(D)^%i)", x)
    }
  ),
  collapse = " + "
  )
  
  if(bayesian){
    
    if (is.null(weight)) {
      cache_path <- cacheManager(nameFile = paste0(method,"_weights_F_model.rds"))
    } else {
      cache_path <- cacheManager(nameFile = paste0(method,"_weights_T_model.rds"))
    }
    
    if( is.null(useCache) ) { # if useCache = NULL -> remove cache
      file.remove(cache_path)
      useCache <- TRUE
    }
    
    # rest of the formula
    if(is.null(weight)){
      formula <- as.formula(paste("I(log(H))", end_formula, sep = " ~ "))
    } else {
      data$weight <- weight
      formula <- as.formula(paste("I(log(H)) | weights(weight, scale = TRUE)", end_formula, sep = " ~ "))
    }
    
    if (useCache & file.exists(cache_path) ) { # if the model has already been build (and compiled)
      message(paste("Loading",method,"model using the cache..."))
      mod <- readRDS(cache_path)
      mod <- update(mod, newdata = data, chains = chains, thin = thin, iter = iter, warmup = warmup, ...)
    } else { # else, build the model (and save it as .rds if useCache = TRUE)
      message(paste("Building", method, "model using brms library."))
      message("Compiling the Stan programme may take some time the first time around. Next time, consider using 'useCache' = TRUE to avoid this compilation time.")
      if(!useCache) {
        cache_path <- NULL
      }
      mod <- brms::brm(data = data,
                       family = gaussian(link = "identity"),
                       formula = formula,
                       file = cache_path,
                       chains = chains, thin = thin, iter = iter, warmup = warmup,
                       ...
      )
    }
  } else {
    formula <- as.formula(paste("I(log(H))", end_formula, sep = " ~ "))
    mod <- lm(formula = formula, data = data, weights = weight)
  }
  
  return(mod)
}
