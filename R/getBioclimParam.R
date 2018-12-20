#' Retrieving bioclimatic parameters
#'
#' This function extracts three bioclimatic parameters thanks to the coordinates of the data:
#' the Climatic Water Deficit (CWD), the Temperature Seasonality (TS) and the Precipitation Seasonality (PS).
#'
#' The function is time-consuming at its first use as it downloads three raster files (one for each of
#' the parameter) which are then stored in forders named wc2-5 and CWD (see Localisation).
#'
#' However, as soon as the raster is downloaded once, the function then runs fast.
#'
#'
#' @inheritSection cacheManager Localisation
#'
#' @inheritParams computeE
#'
#' @return The function returns a data.frame with `tempSeas` (temperature seasonality,
#'  i.e. bioclimatic variable 4 from the Worldclim dataset; Hijmans et al. 2005), `precSeas`
#'  (precipitation seasonality, i.e. bioclimatic variable 15 from the Worldclim dataset; Hijmans
#'  et al. 2005) and `CWD` (climatic water deficit; Chave et al. 2014).
#'
#' @references
#' Hijmans et al. (2005) _Very high resolution interpolated climate surfaces for global land areas_,
#' International journal of climatology, 25(15), 1965-1978.
#' Chave et al. (2014) _Improved allometric models to estimate the above-ground biomass of tropical trees_,
#' Global Change Biology, 20 (10), 3177-3190
#'
#' @author Ariane TANGUY, Arthur PERE
#' @keywords bioclim param internal
#' @export
#'
#' @examples
#' # One study site
#' lat <- 4.08
#' long <- -52.68
#' coord <- cbind(long, lat)
#' \dontrun{
#' bioclim <- getBioclimParam(coord)
#' }
#' 
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \dontrun{
#' bioclim <- getBioclimParam(coord)
#' }
#' 
#' @importFrom rappdirs user_data_dir
#' @importFrom raster raster extract getData

getBioclimParam <- function(coord) {
  coord <- apply(coord, 1:2, as.numeric)

  pathwc <- cacheManager("wc2-5")

  ### Load rasters
  tempSeas_rast <- raster(pathwc[1])
  precSeas_rast <- raster(pathwc[2])
  CWD_rast <- raster(cacheManager("CWD"))

  ### Extract the raster value
  tempSeas <- extract(tempSeas_rast, coord, "bilinear") * 10^-3
  precSeas <- extract(precSeas_rast, coord, "bilinear") * 10^-3
  CWD <- extract(CWD_rast, coord, "bilinear") * 10^-3

  out <- data.frame(tempSeas = tempSeas, precSeas = precSeas, CWD = CWD)
  return(out)
}
