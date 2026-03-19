#' Computing tree above ground biomass (AGB)
#'
#' Compute tree above ground biomass (AGB) using multiple biomass allometry
#'
#' @param allometry a function which returns AGB estimates as a numeric vector, or a list of functions
#' @param ... arguments passed to function(s) provided in `allometry`
#'
#' @return numeric vector with tree AGB estimates
#' 
#' @examples
#' allom <- function(D, G) {
#'   D*G
#' }
#' allom2 <- function(G) {
#'   G^2
#' }
#' 
#' d <- c(10:99)
#' g <- c(100:189)
#' 
#' computeAGB(list(allom, allom2), D = d, G = g)
#' 
#' @export
#' 
computeAGB <- function(allometry = chave2014, ...) {
  
  # Capture arguments passed by ellipses
  args <- list(...)

  # Capture unevaluated input to extract names later
  allom_expr <- substitute(allometry)
  
  # Standardise to list
  if (is.function(allometry)) {
    func_name <- deparse(allom_expr)[1] 
    allometry <- list(allometry)
    names(allometry) <- func_name
  }

  if (!is.list(allometry)) {
    stop("'allometry' must be a function or a list of functions.")
  }

  # Validate allometry function
  if (!all(unlist(lapply(allometry, is.function)))) {
    stop("'allometry' must only contain functions.")
  }

  # Automatically add names to function list if missing
  nm <- names(allometry)
  if (is.null(nm)) {
    nm <- rep("", length(allometry))
  }
  
  # Try to extract names if the user typed `list(...)` inline
  if (is.call(allom_expr) && identical(allom_expr[[1]], quote(list))) {
    inline_names <- sapply(as.list(allom_expr)[-1], function(x) deparse(x)[1])
    nm[nm == ""] <- inline_names[nm == ""]
  }
  
  # Fallback for any remaining blanks (pre-defined unnamed lists)
  blanks <- nm == ""
  if (any(blanks)) {
    nm[blanks] <- seq_len(sum(blanks))
  }
  
  # Apply the cleaned-up names back to the list
  names(allometry) <- nm

  # For each function:
  out <- lapply(seq_along(allometry), function(i) {

    x <- allometry[[i]]
    func_id <- names(allometry)[i]

    # Extract required arguments from allometry function
    fn_formals <- formals(x)
    is_required <- sapply(fn_formals, function(y) {
      identical(y, quote(expr = ))
    })
    required_args <- names(fn_formals)[is_required]
  
    # Ignore ellipsis if target function accepts it
    required_args <- required_args[required_args != "..."]

    # Check if any required arguments are missing from arguments in ellipsis
    missing_args <- setdiff(required_args, names(args))
  
    # If any arguments missing, stop
    if (length(missing_args) > 0) {
      stop(sprintf(
        "Missing required arguments for '%s': %s", 
        func_id, paste(missing_args, collapse = ", ")
      ))
    }

    # Filter arguments: Only pass arguments this specific function accepts
    if ("..." %in% names(fn_formals)) {
      valid_args <- args
    } else {
      valid_args <- args[names(args) %in% names(fn_formals)]
    }

    # Run allometry
    do.call(x, valid_args)
  })

  names(out) <- names(allometry)
  
  # Return
  return(out)
}
