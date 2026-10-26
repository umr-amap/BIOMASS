#' Define a height-diameter model from a published equation
#'
#' Creates a height-diameter model from a published equation, which can be
#' used in place of a model fitted with [modelHD()] in [retrieveH()],
#' [predictHeight()] and [AGBmonteCarlo()] (`HDmodel`).
#'
#' @param formula One-sided formula giving tree height (m) as a function of
#'   diameter `D` (cm) and the parameters, e.g. `~ a * D^b / (k + D^b)`
#' @param parameters Named numeric vector, or named list, of parameter values
#' @param sigma Residual standard deviation of the model. On the log scale if
#'   `link = "log"`, otherwise in m
#' @param link `"log"` (default) if the residual error is log-normal, with
#'   `formula` giving the median height, or `"identity"` if the residual error
#'   is normal, with `formula` giving the mean height
#' @param name (optional) Name of the model, e.g. its reference
#'
#' @details
#' Predictions without error, from [retrieveH()] or
#' `predictHeight(err = FALSE)`, are the mean height. With `link = "log"`,
#' this is the median given by `formula` multiplied by `exp(sigma^2 / 2)`
#' (Baskerville correction), as for the log models of [modelHD()].
#'
#' With error propagation, `predictHeight(err = TRUE)` and [AGBmonteCarlo()],
#' heights are drawn from the residual error distribution around `formula`.
#' Parameter uncertainty is not propagated.
#'
#' @return An object of class `"customHD"`: a list with `formula`,
#'   `parameters`, `sigma`, `link`, `name` and `RSE` (equal to `sigma`)
#'
#' @seealso [modelHD()], [retrieveH()]
#'
#' @export
#'
#' @examples
#' # Martinez-Cano et al. (2019), Barro Colorado Island, Panama
#' mod <- customHD(~ a * D^b / (k + D^b),
#'   parameters = c(a = 57.1, b = 0.73, k = 21.79),
#'   sigma = 0.181, link = "log", name = "Martinez-Cano et al. (2019)")
#' retrieveH(D = c(10, 50, 100), model = mod)
customHD <- function(formula, parameters, sigma, link = c("log", "identity"),
  name = NULL) {
  link <- match.arg(link)
  if (!inherits(formula, "formula") || length(formula) != 2) {
    stop("`formula` must be a one-sided formula, e.g. ~ a * D^b")
  }
  parameters <- unlist(parameters)
  if (!is.numeric(parameters) || is.null(names(parameters)) ||
    any(names(parameters) == "")) {
    stop("`parameters` must be a named numeric vector or named list")
  }
  vars <- all.vars(formula)
  if (!"D" %in% vars) {
    stop("`formula` must include the diameter `D`")
  }
  if (!all(vars %in% c("D", names(parameters)))) {
    stop("Variables in `formula` must be `D` or names of `parameters`: ",
      paste(setdiff(vars, c("D", names(parameters))), collapse = ", "))
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || is.na(sigma) || sigma < 0) {
    stop("`sigma` must be a single number >= 0")
  }
  structure(
    list(formula = formula, parameters = parameters, sigma = sigma,
      link = link, name = name, RSE = sigma),
    class = "customHD")
}

# Height from a customHD model for a vector or matrix of diameters `D`: the
# median (log link) or mean (identity link) height, without error
customHDpred <- function(model, D) {
  out <- eval(model$formula[[2]], c(list(D = D), as.list(model$parameters)))
  if (is.matrix(D)) {
    out <- matrix(out, nrow = nrow(D), ncol = ncol(D), dimnames = dimnames(D))
  }
  out
}

#' @export
print.customHD <- function(x, ...) {
  cat("Custom height-diameter model",
    if (!is.null(x$name)) paste0(": ", x$name), "\n", sep = "")
  cat("  H =", deparse(x$formula[[2]]), "\n")
  cat("  parameters:", paste(names(x$parameters), signif(x$parameters, 4),
    sep = " = ", collapse = ", "), "\n")
  cat("  residual error:", if (x$link == "log") "log-normal" else "normal",
    "with sigma =", x$sigma, "\n")
  invisible(x)
}
