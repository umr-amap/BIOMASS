#' Retrieving tree height from models
#'
#' @description 
#' From the diameter and either i) a model, ii) the coordinates of the plot or iii) the region, this function gives an estimate of the total tree height.
#'
#' @param D Vector of diameters.
#' @param model A model output by the function [modelHD()].
#' @param coord Coordinates of the site(s), either a vector (e.g. c(longitude, latitude)) or a
#' matrix/dataframe with two columns (e.g. cbind(longitude, latitude)).
#' @param region Area of your dataset to estimate tree height thanks to Weibull-H region-, continent-specific
#' and pantropical models proposed by Feldpausch et al. (2012). To be chosen between:
#'   - `Africa`: Africa
#'   - `CAfrica`: Central Africa
#'   - `EAfrica`: Eastern Africa
#'   - `WAfrica`: Western Africa
#'   - `SAmerica`: Southern America
#'   - `BrazilianShield`: Brazilian Shield
#'   - `ECAmazonia`: East-Central Amazonia
#'   - `GuianaShield`: Guiana Shield
#'   - `WAmazonia`: Western Amazonia
#'   - `SEAsia`: South-Eastern Asia
#'   - `NAustralia`: Northern Australia
#'   - `Pantropical`: Pantropical
#'
#' @param plot (optional) Plot ID, must be either one value, or a vector of the same length as D. This argument is used to build
#' stand-specific HD models.
#' 
#' @return Returns a list with:
#'   - `H`: Height predicted by the model
#'   - `RSE` Residual Standard Error of the model, or a vector of those for each plot
#'   
#' @references
#' Feldpausch et al. _Tree height integrated into pantropical forest biomass estimates_. Biogeosciences (2012): 3381-3403.
#' 
#' Chave et al. _Improved allometric models to estimate the aboveground biomass of tropical trees_. Global change biology 20.10 (2014): 3177-3190.
#' 
#' @author Ariane TANGUY, Maxime REJOU-MECHAIN, Arthur PERE
#' @seealso [modelHD()]
#' @export
#'
#' @examples
#' # Load a database
#' data(NouraguesHD)
#' model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
#'
#' # If any height model is available
#' H <- retrieveH(D = NouraguesHD$D, model = model)
#'
#' # If the only data available are the coordinates of your spot
#' n <- length(NouraguesHD$D)
#' coord <- cbind(long = rep(-52.68, n), lat = rep(4.08, n))
#' \donttest{
#' H <- retrieveH(D = NouraguesHD$D, coord = coord)
#' }
#'
#' # If the only data available is the region of your spot
#' H <- retrieveH(D = NouraguesHD$D, region = "GuianaShield")
retrieveH <- function(D, model = NULL, coord = NULL, region = NULL, plot = NULL) {


  # parameters verification -------------------------------------------------

  if (is.null(model) & is.null(region) & is.null(coord)) {
    stop("Either model, region, coord should be given")
  }

  if (!is.null(region) && length(region) != 1 && length(region) != length(D)) {
    stop("The number of region does not match the number of trees")
  }

  if (!is.null(coord) && ((is.vector(coord) && length(coord) != 2) || (is.matrix(coord) && nrow(coord) != length(D)))) {
    stop("coord should be either
             - a vector (e.g. c(longitude, latitude))
             - a matrix with two columns (longitude and latitude)
             having the same number of rows as the number of trees (length(D))")
  }

  if ((!is.null(model) && !is.null(coord)) || (!is.null(model) && !is.null(region)) || (!is.null(coord) && !is.null(region))) {
    stop("Too many input, choose one input among those arguments:
              - H and Herr
              - HDmodel
              - coord")
  }

  # the length of the plot is tested in predictHeight
  # the names of the plot and the names of the model is tested in predictHeight
  if (!is.null(plot) && is.null(model)) {
    stop("The 'plot' vector must be with 'model' argument")
  }


  # First case : with a model fitted with HDfunction
  if (!is.null(model)) {
    H <- predictHeight(D, model, plot = plot)
    RSE <- if (!is.null(plot)) {
      sapply(model, function(x) x$RSE)
    } else if (length(model[[1]]) != 2) {
      model[[1]]$RSE
    } else {
      model$RSE
    }
  }
  else {
    # Second case : with the coordinates of your site, find the E index and estimate the H following Chave et al. 2014 Global Change Biology
    if (!is.null(coord)) {
      if (is.null(dim(coord))) {
        coord <- as.matrix(t(coord))
      }

      E <- computeE(coord) # E = environmental index in Chave et al. 2014

      if (length(E) == 1) {
        E <- rep(E, length(D))
      }

      logD <- log(D)

      # eq 6a Chave et al. 2014
      logH <- 0.893 - E + 0.760 * logD - 0.0340 * I(logD^2)
      RSE <- 0.243
      H <- exp(logH + 0.5 * RSE^2)
    }
    else {
      # Third case : with the region, use the weibull parameters from Feldpaush et al. 2012 Biogeosciences

      feldCoef <- BIOMASS::feldCoef
      a <- feldCoef[region, "a"]
      b <- feldCoef[region, "b"]
      c <- feldCoef[region, "c"]
      RSE <- feldCoef[region, "RSE"]
      # eq 5 in Feldpaush et al. 2012
      H <- a * (1 - exp(-b * D^c))
    }
  }
  output <- list(H = as.numeric(H), RSE = RSE)
  return(output)
}
