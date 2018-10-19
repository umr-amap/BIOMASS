#' Retrieving the Feldpausch Region
#' 
#' Extract the Feldpausch et al. (2012) region thanks to the coordinates of the data. 
#' The function is time-consuming at its first use as it downloads a raster in the folder FeldRegion (see Localisation).
#' However, as soon as the raster is downloaded once, the function then runs fast.
#' 
#' @inheritSection folderControl Localisation
#'
#' @inheritParams computeE
#'
#' @return The function return a vector with The Feldpausch Region prepared for the \code{retrieveH} function
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
#' @references 
#' Feldpausch et al. \emph{Tree height integrated into pantropical forest biomass estimates.} Biogeosciences (2012): 3381-3403.
#' 
#' @author Maxime REJOU-MECHAIN, Arthur PERE
#' 
#' @importFrom raster raster extract factorValues
computeFeldRegion = function(coord){
  
  # path = folderControl("FeldRegion")
  path = "~/Bureau/document arthur/feldRegion/"
  
  # nam <- paste(path$path, "feldRegion.grd", sep = path$sep)
  nam = paste0(path, "feldRegion.grd")
  RAST <- raster(nam)
  
  # Extract the raster value
  RASTval <- extract(RAST, coord)
  FeldRegion = as.vector( factorValues(RAST, RASTval, att = "Region") )
  
  return(FeldRegion)

}