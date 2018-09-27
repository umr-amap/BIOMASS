#' Retrieving bioclimatic parameters
#'
#' This function extracts three bioclimatic parameters thanks to the coordinates of the data: 
#' the Climatic Water Deficit (CWD), the Temperature Seasonality (TS) and the Precipitation Seasonality (PS).
#' The function is time-consuming at its first use as it downloads three raster files (one for each of 
#' the parameter) which are then stored in the working directory. However, as soon as the raster is 
#' downloaded once, the function then runs fast (if the working directory is not changed or if the rasters
#' are then moved to the new working directory).
#'
#' @param coord Coordinates of the site(s), a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)). 
#'
#' @return The function returns a data.frame with \code{tempSeas} (temperature seasonality,
#'  i.e. bioclimatic variable 4 from the Worldclim dataset; Hijmans et al. 2005), \code{precSeas}
#'  (precipitation seasonality, i.e. bioclimatic variable 15 from the Worldclim dataset; Hijmans 
#'  et al. 2005) and \code{CWD} (climatic water deficit; Chave et al. 2014). 
#'  
#' @references 
#' Hijmans et al. (2005) \emph{Very high resolution interpolated climate surfaces for global land areas}, International journal of climatology, 25(15), 1965-1978.
#' Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, Global Change Biology, 20 (10), 3177-3190
#' @author Ariane TANGUY
#' @keywords bioclim param internal
#' @export
#'
#' @examples
#' # One study site
#' lat <- 4.08
#' long <- -52.68
#' coord <- cbind(long, lat)
#' \dontrun{bioclim <- getBioclimParam(coord)}
#' 
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \dontrun{bioclim <- getBioclimParam(coord)}
#' 
#' @importFrom rappdirs user_data_dir
#' @importFrom raster raster extract getData
 
getBioclimParam <- function(coord)
{  
  
  sep = ifelse(length(grep( "win", Sys.info()["sysname"], ignore.case = T )) != 0, "\\", "/")
  path = paste0( user_data_dir("BIOMASS"), sep )
  
  if( !dir.exists( user_data_dir("BIOMASS")) ){
    dir.create( user_data_dir("BIOMASS") )
  }
  
  ### If the file needed are in the working directory
  if (dir.exists("wc2-5") & !dir.exists(paste0(path, "wc2-5"))){
    file.rename("wc2-5", paste0(path, "wc2-5"))
    message("Your repertory \"wc2-5\" has been moved in this repertory : ", path)
  }
  
  if (dir.exists("wc2-5") & dir.exists(paste0(path, "wc2-5"))){
    message("Your repertory \"wc2-5\" already exists in this path : ", path, " and in working directory. ",
            "You can delete those repertory if there is nothing more than the World Climate.")
  }
  
  ### Get the BioClim param from the http://www.worldclim.org website
  if(!dir.exists(paste0(path, "wc2-5")))
  {
    bioData <- getData('worldclim', var='bio', res=2.5, path = path)
    unzip(paste0(path, "wc2-5", sep, "bio_2-5m_bil.zip"), exdir = paste0(path, "wc2-5"), files = c("bio4.bil", "bio15.bil"))
  }
  

  
  
  ### If the file needed are in the working directory
  if (dir.exists("CWD") & !dir.exists(paste0(path, "CWD"))){
    file.rename("CWD", paste0(path, "CWD"))
    message("Your repertory \"CWD\" has been moved in this repertory : ", path)
  }
  
  if (file.exists("CWD_zip") & !file.exists(paste0(path, "CWD_zip"))){
    file.rename("CWD_zip", paste0(path, "CWD_zip"))
    message("Your repertory \"CWD_zip\" has been moved in this repertory : ", path)
  }
  
  if (dir.exists("CWD") & dir.exists(paste0(path, "CWD"))){
    message("Your repertory \"CWD\" and/or \"CWD_zip\" already exists in this path : ", path, " and in working directory. ",
            "You can delete those repertory if there is nothing more than the Climatological Water Deficit.")
  }
  
  
  ### Get the CWD from the Jerome Chave's website
  if(!dir.exists(paste0(path, "CWD")))
  {
    zipurl <- "http://chave.ups-tlse.fr/pantropical_allometry/CWD.bil.zip"
    
    if(!file.exists(paste0(path, "CWD")))
      DEMzip <- download.file(zipurl, destfile = paste0(path, "CWD_zip"))
    unzip(paste0(path, "CWD_zip"), exdir = paste0(path, "CWD"))
  }
  
  ### Load rasters
  tempSeas_rast <- raster(paste0(path, "wc2-5", sep, "bio4.bil"))
  precSeas_rast <- raster(paste0(path, "wc2-5", sep, "bio15.bil"))
  CWD_rast <- raster(paste0(path, "CWD", sep, "CWD.bil"))
  
  ### Extract the raster value
  tempSeas <- extract(tempSeas_rast, coord, "bilinear") * 10^-3
  precSeas <- extract(precSeas_rast, coord, "bilinear") * 10^-3
  CWD <- extract(CWD_rast, coord, "bilinear") * 10^-3
  
  out = data.frame(tempSeas = tempSeas, precSeas = precSeas, CWD = CWD)
  return(out)
}
