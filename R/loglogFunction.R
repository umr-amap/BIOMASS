#' @rdname HDmethods
#' @importFrom stats formula as.formula

loglogFunction <- function(data, method) {
  ### Compute the loglog model of the H-D relationship

  # take the pow of the method
  method_pow <- strtoi(substr(method, nchar(method), nchar(method)))

  # do the rigth part of the formula i.e I(log(D)^1) + I(log(D)^2) + ...
  formula <- paste(sapply(
    seq(method_pow),
    function(x) {
      sprintf("I(log(D)^%i)", x)
    }
  ),
  collapse = " + "
  )

  # for the rest of the formula
  formula <- as.formula(paste("I(log(H))", formula, sep = " ~ "))

  return(lm(formula, data))
}
