#' Retrieving Chave's environmental index
#'
#'Extract the Chave et al. 2014 environmental index thanks to the coordinates of the data. 
#'The function is time-consuming at its first use as it downloads a raster in the working directory. 
#'However, as soon as the raster is downloaded once, the function then runs fast (if the working directory 
#'is not changed or if the raster is copied in the new working directory).
#'
#' @param coord Coordinates of the site(s), a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)) (see examples). 
#'
#' @details 
#' The Chave's environmental index, \code{E}, has been shown to be an important covariable of 
#' the diameter-height relationship of trees. It is calculated as:
#' \deqn{E = 1.e-3 * (0.178 * TS - 0.938 * CWD - 6.61 * PS)}
#' where \eqn{TS} is temperature seasonality as defined in the Worldclim dataset (bioclimatic variable 4), 
#' \eqn{CWD} is the climatic water deficit (in mm/yr, see Chave et al. 2014) and \eqn{PS} is the 
#' precipitation seasonality as defined in the Worldclim dataset (bioclimatic variable 15). 
#' 
#' 
#' The E index is extracted from a raster file (2.5 arc-second resolution, or ca. 5 km) downloadable 
#' at http://chave.ups-tlse.fr/pantropical_allometry.htm
#'
#' @return The function returns \code{E}, the environmental index computed thanks to the Chave et al 2014 formula.
#' @references 
#' Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, Global Change Biology, 20 (10), 3177-3190
#' @author Jerome CHAVE, Maxime REJOU-MECHAIN, Ariane TANGUY
#' 
#' @export
#' @keywords environmental index internal
#' @examples
#' # One study site 
#' lat <- 4.08 
#' long <- -52.68 
#' coord <- cbind(long, lat)
#' \dontrun{E <- computeE(coord)}
#' 
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \dontrun{E <- computeE(coord)}
#' 
#' @importFrom rappdirs user_data_dir
#' @importFrom raster raster extract

computeE <- function(coord)
{  
  ### Compute the Environmental Index (Chave et al. 2014)
  
  sep = ifelse(length(grep( "win", Sys.info()["sysname"], ignore.case = T )) != 0, "\\", "/")
  path = paste0( user_data_dir("BIOMASS"), sep )
  
  if( !dir.exists( user_data_dir("BIOMASS")) ){
    dir.create( user_data_dir("BIOMASS") )
  }
  
  ### If the file needed are in the working directory
  if (dir.exists("E") & !dir.exists(paste0(path, "E"))){
    file.rename("E", paste0(path, "E"))
    message("Your repertory \"E\" has been moved in this repertory : ", path)
  }
  
  if (file.exists("E_zip") & !file.exists(paste0(path, "E_zip"))){
    file.rename("E_zip", paste0(path, "E_zip"))
    message("Your repertory \"E_zip\" has been moved in this repertory : ", path)
  }
  
  if (dir.exists("E") & dir.exists(paste0(path, "E"))){
    message("Your repertory \"E\" and/or \"E_zip\" already exists in this path : ", path, " and in working directory. ",
            "You can delete those repertory if there is nothing more than the Environmental Index.")
  }
  
  ### If the E file doesn't exist anywhere
  if(!dir.exists(paste0(path, "E")))
  {
    zipurl <- "http://chave.ups-tlse.fr/pantropical_allometry/E.bil.zip"
    
    if(!file.exists(paste0(path, "E_zip")) | file.info(paste0(path, "E_zip"))$size < 31202482)
      DEMzip <- download.file(zipurl, destfile = paste0(path, "E_zip"))
    unzip(paste0(path, "E_zip"), exdir = paste0(path, "E"))
  }
  
  nam <- paste0(path, "E", sep, "E.bil")
  RAST <- raster(nam)
  
  # Extract the raster value
  RASTval <- extract(RAST, coord, "bilinear")
  return(RASTval)
}