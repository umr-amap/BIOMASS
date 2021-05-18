#' Retrieving bioclimatic parameters
#'
#' This function extracts three bioclimatic parameters thanks to the coordinates of the data:
#' the Climatic Water Deficit (CWD), the Temperature Seasonality (TS) and the Precipitation Seasonality (PS).
#'
#' The function is time-consuming at its first use as it downloads three raster files (one for each of
#' the parameter) which are then stored in folders named wc2-5 and CWD (see Localisation).
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
#' \donttest{
#' bioclim <- getBioclimParam(coord, useCache = FALSE)
#' }
#'
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \donttest{
#' bioclim <- getBioclimParam(coord, useCache = FALSE)
#' }
#'
#' @importFrom rappdirs user_data_dir
#' @importFrom raster raster extract getData

getBioclimParam <- function(coord,useCache) {
  coord <- apply(coord, 1:2, as.numeric)

  if(is.logical(useCache) && !useCache){
    #tempSeas and precSeas
    tmp <- tempfile(fileext = ".zip")
    DEMzip <- download.file("https://github.com/AMAP-dev/BIOMASS/raw/master/data-raw/climate_variable/wc2-5.zip", destfile = tmp)
    unzip(tmp, exdir = tempdir())
    tempSeas_rast <- raster(file.path(tempdir(),"bio4.bil"))
    precSeas_rast <- raster(file.path(tempdir(),"bio15.bil"))
    #CWD
    tmp <- tempfile(fileext = ".zip")
    DEMzip <- download.file("https://github.com/AMAP-dev/BIOMASS/raw/master/data-raw/climate_variable/CWD.zip", destfile = tmp)
    unzip(tmp, exdir = tempdir())
    CWD_rast <- raster(file.path(tempdir(),"CWD.bil"))
    
    message("The Bioclim raster files have been downloaded in a temporary file. Using useCache=TRUE is recommended to avoid download time for the next research")
  }else{
  
  pathwc <- cacheManager("wc2-5")

  ### Load rasters
  tempSeas_rast <- raster(pathwc[1])
  precSeas_rast <- raster(pathwc[2])
  CWD_rast <- raster(cacheManager("CWD"))
  }
  
  ### Extract the raster value
  tempSeas <- extract(tempSeas_rast, coord, "bilinear") * 10^-3
  precSeas <- extract(precSeas_rast, coord, "bilinear") * 10^-3
  CWD <- extract(CWD_rast, coord, "bilinear") * 10^-3

  out <- data.frame(tempSeas = tempSeas, precSeas = precSeas, CWD = CWD)
  return(out)
}
