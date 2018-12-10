#' Fitting height-diameter models
#'
#' This function fits and compares (optional) height-diameter models.
#'
#' @param D Vector with diameter measurements (in cm). NA values are accepted but a
#' minimum of 10 valid entries (i.e. having a corresponding height in H) is required.
#' @param H Vector with total height measurements (in m). NA values are accepted but a minimum of 10 valid entries (i.e. having a corresponding diameter in D) is required.
#' @param method Method used to fit the relationship.
#' To be chosen between:
#'   \itemize{
#'     \item log1, log2, log3
#'     \itemize{
#'       \item log 1: \eqn{(log(H) = a+ b*log(D))} (equivalent to a power model)
#'       \item log 2: \eqn{(log(H) = a+ b*log(D) + c*log(D)^2)}
#'       \item log 3: \eqn{(log(H) = a+ b*log(D) + c*log(D)^2 + d*log(D)^3)}
#'     }
#'     \item weibull: \eqn{H = a*(1-exp(-(D/b)^c))}
#'     \item michaelis: \eqn{H = (A * D)/(B + D)}
#'   }
#' If \code{NULL}, all the methods will be compared.
#' @param useWeight If weight is \code{TRUE}, model weights will be \eqn{(D^2)*H} (i.e. weights are proportional to tree volume, so that larger trees have a stronger influence during the construction of the model).
#' @param drawGraph If \code{TRUE}, a graphic will illustrate the relationship between H and D.
#'
#' @details All the back transformations for log-log models are done using the Baskerville correction (\eqn{0.5 * RSE^2}, where RSE is the Residual Standard Error).
#'
#'
#' @return Returns a list with if the parameter model is not null:
#' \describe{
#' \item{input}{list of the data used to construct the model (list(H, D))}
#' \item{model}{outputs of the model (same outputs as given by \code{\link{lm}}, \code{\link{nls}})}
#' \item{RSE}{Residual Standard Error of the model}
#' \item{RSElog}{Residual Standard Error of the log model (\code{NULL} if other model)}
#' \item{residuals}{Residuals of the model}
#' \item{coefficients}{Coefficients of the model}
#' \item{R.squared}{\eqn{R^2} of the model}
#' \item{formula}{Formula of the model}
#' \item{method}{Name of the method used to construct the model}
#' \item{predicted}{Predicted height values}
#' }
#'
#' If the parameter model is null, the function return a graph with all the methods for
#' comparison, the function also return a data.frame with:
#' \describe{
#' \item{method}{The method that had been used to construct the graph}
#' \item{color}{The color of the curve in the graph}
#' \item{RSE}{Residual Standard Error of the model}
#' \item{RSElog}{Residual Standard Error of the log model (\code{NULL} if other model)}
#' \item{Average_bias}{The average bias for the model}
#' }
#'
#'
#' @author Maxime REJOU-MECHAIN, Arthur PERE, Ariane TANGUY
#' @seealso \code{\link{retrieveH}}, \code{\link{predictHeight}}
#'
#' @export
#'
#' @examples
#' 
#' # Load a data set
#' data(NouraguesHD)
#' 
#' # To model the height from a dataset
#' \dontrun{
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, drawGraph = TRUE)
#' }
#' 
#' # If the method needed is known
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "weibull", drawGraph = TRUE)
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log1", drawGraph = TRUE)
#' 
#' # Using weights
#' HDmodel <- modelHD(
#'   D = NouraguesHD$D, H = NouraguesHD$H, method = "weibull", useWeight = TRUE,
#'   drawGraph = TRUE
#' )
#' @importFrom graphics legend lines par plot grid axis
#' @importFrom stats SSmicmen lm median na.omit quantile rnorm sd predict coef
#' @importFrom utils data
#' @importFrom data.table data.table

modelHD <- function(D, H, method = NULL, useWeight = FALSE, drawGraph = FALSE) {

  # parameters verification -------------------------------------------------

  # Check if there is enough data to compute an accurate model
  nbNonNA <- sum(!is.na(H))
  if (nbNonNA < 15) {
    stop("The data has not enough height data (less than 15 non NA)")
  }

  if (length(H) != length(D)) {
    stop("Your vector D and H don't have the same length")
  }

  if (!is.null(method)) {
    method <- tolower(method)
  }

  methods <- c("log1", "log2", "log3", "weibull", "michaelis")
  if (!is.null(method) && !(method %in% methods)) {
    stop("Chose your method among those ones : ", paste(methods, collapse = ", "))
  }

  if (!is.logical(useWeight)) {
    stop("UseWeight argument must be a boolean")
  }

  if (!is.logical(drawGraph)) {
    stop("drawGraph argument must be a boolean")
  }



  # functions ----------------------------------------------------------------

  # fonction to choose the fonction
  modSelect <- function(Hdata, method, useGraph = FALSE) {
    output <- list()


    ################## Log-log model
    if (grepl("log", method)) {
      mod <- loglogFunction(Hdata, method)
      output$RSElog <- summary(mod)$sigma


      # Baskerville correction 1972
      output$Hpredict <- exp(predict(mod) + 0.5 * output$RSElog^2)

      if (useGraph) {
        output$Hpredict_plot <- exp(predict(mod, newdata = data.frame(logD = log(D_Plot))) + 0.5 * output$RSElog^2)
      }
    } else {
      mod <- switch(method,
        michaelis = michaelisFunction(Hdata, weight), # Michaelis-Menten function
        weibull = weibullFunction(Hdata, weight) # Weibull 3 parameters
      )

      output$Hpredict <- predict(mod)
      output$RSElog <- NA_real_

      if (useGraph) {
        output$Hpredict_plot <- predict(mod, newdata = data.frame(D = D_Plot))
      }
    }

    names(output$Hpredict) <- NULL
    res <- Hdata$H - output$Hpredict


    output$method <- method
    output$RSE <- sqrt(sum(res^2) / summary(mod)$df[2]) # Residual standard error
    output$Average_bias <- (mean(output$Hpredict) - mean(Hdata$H)) / mean(Hdata$H)
    output$residuals <- res
    output$mod <- mod


    return(output)
  }



  # function to draw the beining of the graph
  drawPlotBegin <- function(givenMethod = FALSE) {
    par(mar = c(5, 5, 3, 3))
    plot(Hdata$D, Hdata$H,
      pch = 20, cex = 0.5, col = "grey50", log = "xy", las = 1,
      xlab = "D (cm)", ylab = "H (m)", cex.lab = 1.8, cex.axis = 1.5,
      main = ifelse(givenMethod == FALSE, "Model comparison", paste("Selected model : ", givenMethod)),
      cex.main = 2, axes = F, frame.plot = F
    )
    grid(col = "grey80", lty = 1, equilogs = F)
    axis(side = 1, lty = "blank", las = 1)
    axis(side = 2, lty = "blank", las = 1)
  }


  # Data processing ---------------------------------------------------------

  Hdata <- data.table(H = H, D = D)
  Hdata <- na.omit(Hdata) # Remove NA values
  weight <- NULL

  # Vector of diameter used only for visualisation purpose
  D_Plot <- seq(from = Hdata[, floor(min(D))], to = Hdata[, ceiling(max(D))], 0.5)

  # If the measures need to be weighted
  if (useWeight == TRUE) {
    weight <- Hdata[, D^2 * H]
  } # weight is proportional to tree volume



  # If we gave the function a method ----------------------------------------

  if (!is.null(method)) {
    output <- modSelect(Hdata, method, drawGraph)

    ####### if drawGraph is true
    if (drawGraph) {
      drawPlotBegin(method)

      lines(D_Plot, output$Hpredict_plot, lwd = 2, col = "blue")
      legend("bottomright", c("Data", "Model selected"),
        lty = c(3, 1), lwd = c(3, 3),
        col = c("grey", "blue"), cex = 1.5
      )
    }

    ################## Return the model chosen

    # Results (RSE, model coefficient, residuals, R?)

    out <- list(
      input = list(H = Hdata$H, D = Hdata$D),
      model = output$mod,
      residuals = output$residuals,
      coefficients = summary(output$mod)$coefficients,
      R.squared = summary(output$mod)$r.squared,
      formula = summary(output$mod)$call,
      method = method,
      predicted = output$Hpredict,
      RSE = output$RSE
    )

    if (grepl("log", method)) {
      out$RSElog <- output$RSElog
    }

    return(out)
  } else {
    # Compare Models ----------------------------------------------------------

    drawPlotBegin()
    color <- c("blue", "green", "red", "orange", "purple")


    result <- rbindlist(lapply(1:length(methods), function(i) {
      method <- methods[i]

      out <- modSelect(Hdata, method, useGraph = T)

      lines(D_Plot, out$Hpredict_plot, lwd = 2, col = color[i], lty = i)

      output <- list(
        method = method, color = color[i],
        RSE = out$RSE, # Residual standard error
        RSElog = out$RSElog,
        Average_bias = out$Average_bias
      )

      return(output)
    }), fill = T)

    legend("bottomright", methods,
      lty = 1:5, lwd = 2, cex = 1,
      col = color
    )

    message("If you want to use a particular model, use the parameter 'method' in this function.")
    return(data.frame(result))
  }
}
