#' @rdname HDmethods
#' @return Result of a model (nlsM object)
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom methods is

michaelisFunction <- function(data, weight = NULL) {
  ### Compute the michaelis model of the H-D relationship

  H <- data$H
  D <- data$D

  count <- 1
  maxIter <- 50
  converge <- FALSE

  if (anyNA(weight)) weight <- NULL

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
  model <- if (is.null(weight)) {
    nlsLM(H ~ SSmicmen(D, A, B),
      control = nls.lm.control(maxiter = maxIter)
    )
  } else {
    nlsLM(H ~ SSmicmen(D, A, B),
      weights = weight,
      control = nls.lm.control(maxiter = maxIter)
    )
  }

  return(model)
}
