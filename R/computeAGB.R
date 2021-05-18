#' Computing tree above-ground biomass (AGB)
#'
#' This function uses Chave et al. 2014's pantropical models to estimate the above-ground biomass of tropical trees.
#'
#' @param D Tree diameter (in cm), either a vector or a single value.
#' @param WD Wood density (in g/cm3), either a vector or a single value. If not available, see [getWoodDensity()].
#' @param H (optional) Tree height (H in m), either a vector or a single value. If not available, see [retrieveH()]
#' and [modelHD()]. Compulsory if the coordinates `coord` are not given.
#' @param coord (optional) Coordinates of the site(s), either a vector giving a single site
#' (e.g. c(longitude, latitude)) or a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)).
#' The coordinates are used to account for variation in height-diameter relationship thanks to an environmental
#' proxy (parameter E in Chave et al. 2014). Compulsory if tree heights `H` are not given.
#' @param Dlim (optional) Minimum diameter (in cm) for which aboveground biomass should be calculated
#' (all diameter below `Dlim` will have a 0 value in the output).
#' @param useCache logical. Whether or not use a cache to avoid downloading multiple time the same files.
#' Strongly recommended to reduce computing time (but FALSE by default due to CRAN policy).
#'
#' @details
#' This function uses two different ways of computing the above-ground biomass of a tree:
#'
#' 1) If tree height data are available, the AGB is computed thanks to the following equation (Eq. 4 in Chave et al., 2014):
#'  \deqn{AGB = 0.0673 * (WD * H * D^2)^0.976}
#'
#' 2) If no tree height data is available, the AGB is computed thanks to the site coordinates with the following equation, slightly modified from Eq. 7 in Chave et al., 2014 (see Réjou-Méchain et al. 2017):
#'  \deqn{AGB = exp(-2.024- 0.896*E + 0.920*log(WD) + 2.795*log(D) - 0.0461*(log(D)^2))} where `E` is a measure of environmental stress estimated from the site coordinates (`coord`).
#'
#' @return The function returns the AGB in Mg (or ton) as a single value or a vector.
#' @export
#' @references
#' Chave et al. (2014) _Improved allometric models to estimate the aboveground biomass of tropical trees_,
#' Global Change Biology, 20 (10), 3177-3190
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY, Arthur PERE
#' @seealso [computeE()]
#' @examples
#' # Create variables
#' D <- 10:99
#' WD <- runif(length(D), min = 0.1, max = 1)
#' H <- D^(2 / 3)
#'
#' # If you have height data
#' AGB <- computeAGB(D, WD, H)
#'
#' # If you do not have height data and a single site
#' lat <- 4.08
#' long <- -52.68
#' coord <- c(long, lat)
#' \donttest{
#' AGB <- computeAGB(D, WD, coord = coord)
#' }
#'
#' # If you do not have height data and several sites (here three)
#' lat <- c(rep(4.08, 30), rep(3.98, 30), rep(4.12, 30))
#' long <- c(rep(-52.68, 30), rep(-53.12, 30), rep(-53.29, 30))
#' coord <- cbind(long, lat)
#' \donttest{
#' AGB <- computeAGB(D, WD, coord = coord)
#' }
#'
#' @keywords AGB above-ground biomass forest carbon allometry

computeAGB <- function(D, WD, H = NULL, coord = NULL, Dlim = NULL, useCache=FALSE) {

  # Parameters verification -------------------------------------------------

  if (length(D) != length(WD)) {
    stop("D and WD have different lenghts")
  }

  if (!is.null(H)) {
    if (length(D) != length(H)) {
      stop("H and WD have different length")
    }
    if (anyNA(D)) {
      warning("NA values in D")
    }
    if (anyNA(H) & !anyNA(D)) {
      warning("There is some NA values in given heights. For those trees the function will return NA AGB,
               you may construct a height-diameter model to overcome that issue (see ?HDFunction and ?retrieveH)")
    }
  }

  if (!is.null(coord) && ((is.vector(coord) && length(coord) != 2) || (is.matrix(coord) && nrow(coord) != length(D)))) {
    stop("coord should be either
             - a vector (e.g. c(longitude, latitude))
             - a matrix with two columns (longitude and latitude)
             having the same number of rows as the number of trees (length(D))")
  }

  if (is.null(H) && is.null(coord)) {
    stop("You need to provide either H or coord")
  }



  # Compute the AGB ---------------------------------------------------------

  # If there is height data for all the trees
  if (!is.null(H)) {
    AGB <- (0.0673 * (WD * H * D^2)^0.976) / 1000 # Eq 4 from Chave et al. 2014 Global change biology
  } else {

    # If there is no heigth, but the coordinates :
    if (is.null(dim(coord))) {
      coord <- as.matrix(t(coord))
    }

    E <- computeE(coord, useCache) # environmental index in Chave et al. 2014

    # Modified Eq 7 from Chave et al. 2014 Global change biology
    AGB <- exp(-2.023977 - 0.89563505 * E + 0.92023559 * log(WD) + 2.79495823 * log(D) - 0.04606298 * (log(D)^2)) / 1000
  }

  if (!is.null(Dlim)) AGB[D < Dlim] <- 0
  return(AGB)
}
