#' Retrieving the Feldpausch Regions TO BE IMPROVED
#'
#' Extract the Feldpausch et al. (2012)'s regions thanks to local coordinates.
#'
#' @inheritParams computeE
#'
#' @return The function returns a vector with the Feldpausch et al. (2012)'s regions that can be
#' incorporated in the \code{retrieveH} function.
#' @export
#'
#' @examples
#' #' # One study site
#' lat <- 4.08
#' long <- -52.68
#' coord <- cbind(long, lat)
#' \dontrun{FeldRegion <- computeFeldRegion(coord)}
#'
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \dontrun{FeldRegion <- computeFeldRegion(coord)}
#'
#'
#'
#'
#' @references
#' Feldpausch, T.R., et al. (2012). \emph{Tree height integrated into pantropical forest
#' biomass estimates.} Biogeosciences, 9, 3381–3403.
#'
#' @author Arthur PERE, Ted FELDPAUSCH
#'
#' @importFrom raster raster extract factorValues
computeFeldRegion <- function(coord) {
  feldRegion <- raster(system.file("external/feldRegion.grd", package = "BIOMASS"))
  # Extract the raster value
  RASTval <- extract(feldRegion, coord)
  FeldRegion <- as.vector(factorValues(feldRegion, RASTval, att = "Region"))

  return(FeldRegion)
}
