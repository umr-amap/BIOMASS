#' Fitting height-diameter models
#'
#' This function fits and compares (optional) height-diameter models.
#'
#' @param D Vector with diameter measurements (in cm). NA values are accepted but a
#' minimum of 10 valid entries (i.e. having a corresponding height in H) is required.
#' @param H Vector with total height measurements (in m). NA values are accepted but a minimum of 10 valid entries
#' (i.e. having a corresponding diameter in D) is required.
#' @param method Method used to fit the relationship.
#' To be chosen between:
#'   - log1, log2
#'     + log 1: \eqn{(log(H) = a+ b*log(D))} (equivalent to a power model)
#'     + log 2: \eqn{(log(H) = a+ b*log(D) + c*log(D)^2)}
#'   - weibull: \eqn{H = a*(1-exp(-(D/b)^c))}
#'   - michaelis: \eqn{H = (A * D)/(B + D)}
#'
#' If `NULL`, all the methods will be compared.
#' @param useWeight If weight is `TRUE`, model weights will be \eqn{(D^2)*H} (i.e. weights are proportional to tree
#' volume, so that larger trees have a stronger influence during the construction of the model).
#' @param drawGraph If `TRUE`, a graphic will illustrate the relationship between H and D. Only if argument `plot` is null.
#' @param plot (optional) Plot ID, must be either one value, or a vector of the same length as D. This argument is used to build
#' stand-specific HD models.
#'
#' @details All the back transformations for log-log models are done using the Baskerville correction (\eqn{0.5 * RSE^2},
#' where RSE is the Residual Standard Error).
#'
#'
#' @return
#' If plot is NULL or has a single value, a single list is returned. If there is more than one plot,
#' multiple embedded lists are returned with plots as the list names.
#' Returns a list if the parameter model is not null:
#'   - `input`: list of the data used to construct the model (list(H, D))
#'   - `model`: outputs of the model (same outputs as given by [stats::lm()], [stats::nls()])
#'   - `RSE`: Residual Standard Error of the model
#'   - `RSElog`: Residual Standard Error of the log model (\code{NULL} if other model)
#'   - `residuals`: Residuals of the model
#'   - `coefficients`: Coefficients of the model
#'   - `R.squared`: \eqn{R^2} of the model
#'   - `formula`: Formula of the model
#'   - `method`: Name of the method used to construct the model
#'   - `predicted`: Predicted height values
#'
#'
#' If the parameter model is null, the function return a graph with all the methods for
#' comparison, the function also returns a data.frame with:
#'   - `method`: The method that had been used to construct the graph
#'   - `color`: The color of the curve in the graph
#'   - `RSE`: Residual Standard Error of the model
#'   - `RSElog`: Residual Standard Error of the log model (`NULL` if other model)
#'   - `Average_bias`: The average bias for the model
#'
#'
#'
#' @author Maxime REJOU-MECHAIN, Arthur PERE, Ariane TANGUY
#' @seealso [retrieveH()]
#'
#' @export
#'
#' @examples
#'
#' # Load a data set
#' data(NouraguesHD)
#'
#' # To model the height from a dataset
#' \donttest{
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

modelHD <- function(D, H, method = NULL, useWeight = FALSE, drawGraph = FALSE, plot = NULL) {

  # # To maintain user's original options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # parameters verification -------------------------------------------------

  # Check if there is enough data to compute an accurate model
  nbNonNA <- sum(!is.na(H))
  if (nbNonNA < 15) {
    stop("The data has not enough height data (less than 15 non NA)")
  }

  if (length(H) != length(D)) {
    stop("Your vector D and H do not have the same length")
  }

  if (!is.null(method)) {
    method <- tolower(method)
  }

  methods <- c("log1", "log2", "weibull", "michaelis")
  if (!is.null(method) && !(method %in% methods)) {
    stop("Chose your method among those ones : ", paste(methods, collapse = ", "))
  }

  if (!is.logical(useWeight)) {
    stop("UseWeight argument must be a boolean")
  }

  if (!is.logical(drawGraph)) {
    stop("drawGraph argument must be a boolean")
  }

  if (!is.null(plot) && !length(plot) %in% c(1, length(D))) {
    stop("The length of the 'plot' vector must be either 1 or the length of D")
  }


  if (!is.null(plot)) {
    drawGraph <- FALSE
  }



  # If there is a plot ID ---------------------------------------------------

  # if there is multiple plots in the plot vector
  if (!is.null(plot) && length(unique(plot)) != 1) {
    Hdata <- data.table(H = H, D = D, plot = plot)

    output <- lapply(split(Hdata, by = "plot", keep.by = TRUE), function(subData) {
      suppressMessages(modelHD(
        subData$D, subData$H, method, useWeight,
        drawGraph, unique(subData$plot)
      ))
    })

    if (is.null(method)) {
      message("To build a HD model you must use the parameter 'method' in this function")
    }

    return(output)
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
        output$Hpredict_plot <- exp(predict(mod, newdata = D_Plot) + 0.5 * output$RSElog^2)
      }
    } else {
      ######### The others HD models
      mod <- switch(method,
        michaelis = michaelisFunction(Hdata, weight), # Michaelis-Menten function
        weibull = weibullFunction(Hdata, weight) # Weibull 3 parameters
      )

      output$Hpredict <- predict(mod)
      output$RSElog <- NA_real_

      if (useGraph) {
        output$Hpredict_plot <- predict(mod, newdata = D_Plot)
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
  drawPlotBegin <- function(givenMethod = FALSE, plotId) {
    main_title <- ifelse(givenMethod == FALSE, "Model comparison", paste("Selected model : ", givenMethod))

    par(mar = c(5, 5, 3, 3))
    plot(Hdata$D, Hdata$H,
      pch = 20, cex = 0.5, col = "grey50", log = "xy", las = 1,
      xlab = "D (cm)", ylab = "H (m)", cex.lab = 1.8, cex.axis = 1.5,
      main = main_title,
      cex.main = 2, axes = FALSE, frame.plot = FALSE
    )
    grid(col = "grey80", lty = 1, equilogs = FALSE)
    axis(side = 1, lty = "blank", las = 1)
    axis(side = 2, lty = "blank", las = 1)
  }


  # Data processing ---------------------------------------------------------

  Hdata <- data.table(H = H, D = D)
  Hdata <- na.omit(Hdata) # Remove NA values
  weight <- NULL

  # Vector of diameter used only for visualisation purpose
  D_Plot <- data.frame(D = Hdata[, seq(floor(min(D)), ceiling(max(D)), 0.5)])

  # If the measures need to be weighted
  if (useWeight == TRUE) {
    weight <- Hdata[, D^2 * H]
  } # weight is proportional to tree volume



  # If we gave the function a method ----------------------------------------

  if (!is.null(method)) {
    output <- modSelect(Hdata, method, drawGraph)

    ####### if drawGraph is true
    if (drawGraph) {
      drawPlotBegin(method, plot)

      lines(D_Plot$D, output$Hpredict_plot, lwd = 2, col = "blue")
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
    if (is.null(plot)) {
      drawPlotBegin(plotId = plot)
    }
    color <- c("blue", "green", "orange", "purple")

    result <- rbindlist(lapply(1:length(methods), function(i) {
      method <- methods[i]

      out <- modSelect(Hdata, method, useGraph = is.null(plot))

      output <- list(
        method = method, color = color[i],
        RSE = out$RSE, # Residual standard error
        RSElog = out$RSElog,
        Average_bias = out$Average_bias
      )

      if (is.null(plot)) {
        lines(D_Plot$D, out$Hpredict_plot, lwd = 2, col = color[i], lty = i)
      }
      if (!is.null(plot)) {
        output[["color"]] <- NULL
      }

      return(output)
    }), fill = TRUE)

    if (is.null(plot)) {
      legend("bottomright", methods,
        lty = 1:5, lwd = 2, cex = 1,
        col = color
      )
    }

    message("To build a HD model you must use the parameter 'method' in this function")
    return(data.frame(result))
  }
}
