#' Computing tree above ground biomass (AGB)
#'
#' Compute tree above ground biomass (AGB) using a provided function
#'
#' @param allometry a function which returns AGB estimates as a numeric vector
#' @param ... arguments passed to function provided in `allometry`
#'
#' @return numeric vector with tree AGB estimates
#' 
#' @examples
#' allom <- function(D, G) {
#'   D*G
#' }
#' 
#' d <- c(10:99)
#' g <- c(10:99)
#' 
#' computeAGB(allom, D = d, G = g)
#' 
#' @export
#' 
computeAGB <- function(allometry = chave2014, ...) {
  
  # Capture arguments passed by ellipses
  args <- list(...)

  # Validate allometry function
  if (!is.function(allometry)) {
    stop("'allometry' must be a valid function.")
  }

  # Extract required arguments from allometry function
  fn_formals <- formals(allometry)
  is_required <- sapply(fn_formals, function(x) identical(x, quote(expr = )))
  required_args <- names(fn_formals)[is_required]
  
  # Ignore ellipsis if target function accepts it
  required_args <- required_args[required_args != "..."]

  # Check if any required arguments are missing from arguments in ellipsis
  missing_args <- setdiff(required_args, names(args))
  
  # If any arguments missing, stop
  if (length(missing_args) > 0) {
    stop(sprintf(
      "Missing required arguments for the specified allometry function: %s", 
      paste(missing_args, collapse = ", ")
    ))
  }

  # Run allometry
  agb <- do.call(allometry, args)
  
  # Return
  return(agb)
}

