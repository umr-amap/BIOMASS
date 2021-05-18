if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "RASTval", "long", "lat", "slice", "i.RASTval"
  ))
}

#' Retrieving Chave's environmental index
#'
#' Extract the Chave et al. 2014's environmental index thanks to the coordinates of the data.
#' The function is time-consuming at its first use as it downloads a raster in a folder (see Details).
#' However, as soon as the raster is downloaded once, the function then runs fast.
#'
#'
#'
#' @param coord Coordinates of the site(s), a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)) (see examples).
#'
#'
#' @inheritSection cacheManager Localisation
#'
#' @details
#' The Chave's environmental index, `E`, has been shown to be an important covariable in
#' the diameter-height relationship for tropical trees. It is calculated as:
#' \deqn{E = 1.e-3 * (0.178 * TS - 0.938 * CWD - 6.61 * PS)}
#' where `TS` is temperature seasonality as defined in the Worldclim dataset (bioclimatic variable 4),
#' `CWD` is the climatic water deficit (in mm/yr, see Chave et al. 2014) and `PS` is the
#' precipitation seasonality as defined in the Worldclim dataset (bioclimatic variable 15).
#'
#'
#' The E index is extracted from a raster file (2.5 arc-second resolution, or ca. 5 km) available
#' at http://chave.ups-tlse.fr/pantropical_allometry.htm
#'
#' @return The function returns `E`, the environmental index computed thanks to the Chave et al 2014's formula as a single value or a vector.
#' @references
#' Chave et al. (2014) _Improved allometric models to estimate the aboveground biomass of tropical trees_,
#' Global Change Biology, 20 (10), 3177-3190
#' @author Jerome CHAVE, Maxime REJOU-MECHAIN, Ariane TANGUY, Arthur PERE
#'
#' @export
#' @keywords environmental index internal
#' @examples
#' # One study site
#' lat <- 4.08
#' long <- -52.68
#' coord <- cbind(long, lat)
#' \donttest{
#' E <- computeE(coord, useCache=FALSE)
#' }
#' 
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \donttest{
#' E <- computeE(coord, useCache=FALSE)
#' }
#' 
#' @importFrom raster raster extract
#' @importFrom data.table as.data.table

computeE <- function(coord,useCache) {

  if(is.logical(useCache) && !useCache){
    tmp <- tempfile(fileext = ".zip")
    DEMzip <- download.file("https://github.com/AMAP-dev/BIOMASS/raw/master/data-raw/climate_variable/E.zip", destfile = tmp)
    unzip(tmp, exdir = tempdir())
    RAST <- raster(file.path(tempdir(),"E.bil"))
    message("The E raster file has been downloaded in a temporary file. Using useCache=TRUE is recommended to avoid download time for the next research")
  }else{
    # find the raster
    RAST <- raster(cacheManager("E"))
  }
  
  if (is.vector(coord)) {
    return(extract(RAST, matrix(coord, ncol = 2),"bilinear"))
  }

  # set the coord in a data.table
  coord <- as.data.table(coord)
  setnames(coord, colnames(coord), c("long", "lat"))

  #
  coord_unique <- unique(coord)
  coord_unique <- na.omit(coord_unique)

  # Extract the raster value
  coord_unique[, RASTval := extract(RAST, coord_unique, "bilinear")]

  # search around the point if there is an NA in the RASTval
  r <- 0
  i <- 1
  while (anyNA(coord_unique$RASTval)) {
    r <- r + 5000
    coord_unique[is.na(RASTval), RASTval := sapply(extract(RAST, cbind(long, lat), buffer = r), mean, na.rm = TRUE)]

    if (i > 8) {
      coord[coord_unique, on = c("long", "lat"), RASTval := i.RASTval]
      stop(
        "The coordinate n ", paste(which(is.na(coord$RASTval)), collapse = " "),
        " are too far for first non-NA value in the raster"
      )
    }

    i <- i + 1
  }

  return(coord[coord_unique, on = c("long", "lat"), RASTval])
}
