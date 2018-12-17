#' @rdname HDmethods
#'
#' @importFrom minpack.lm nlsLM nls.lm.control

weibullFunction <- function(data, weight = NULL) {
  ### Compute the weibull model of the H-D relationship

  H <- data$H
  D <- data$D

  Hmax <- quantile(H, probs = 0.90, na.rm = TRUE)
  init <- list(a = as.double(Hmax), b = 24.9, c = 0.8)

  count <- 1
  maxIter <- 50
  converge <- FALSE

  if (anyNA(weight)) weight <- NULL

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

  model <- if (is.null(weight)) {
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

  return(model)
}
