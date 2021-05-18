#' Retrieving Feldpausch regions
#'
#' Extract the Feldpausch et al. (2012)'s regions using local coordinates.
#'
#' @inheritParams computeE
#' @param level a string or a vector of string, the length must match the number of rows of the parameter coord.
#' This parameter gives the scale at which Feldpausch regions should be assigned. There are tree levels:
#'    - `region`: Models assign at sub-continent levels, value by default
#'    - `continent`: Models assign at the Africa, South America, Asia and Australia levels
#'    - `world`: Pantropical model
#'
#' @return The function returns a vector with the Feldpausch et al. (2012)'s regions that can be
#' incorporated in the `retrieveH` function.
#' @export
#'
#' @examples
#' #' # One study site
#' lat <- 4.08
#' long <- -52.68
#' coord <- cbind(long, lat)
#' \donttest{
#' FeldRegion <- computeFeldRegion(coord)
#' }
#' 
#' # Several study sites (here three sites)
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \donttest{
#' FeldRegion <- computeFeldRegion(coord)
#' }
#' 
#' @references
#' Feldpausch, T.R., et al. (2012). _Tree height integrated into pantropical forest biomass estimates._
#' Biogeosciences, 9, 3381–3403.
#'
#' @author Arthur PERE
#'
#' @importFrom raster raster extract factorValues
computeFeldRegion <- function(coord, level = "region") {


  # Parameter verification --------------------------------------------------


  if (!(length(level) %in% c(1, nrow(coord)))) {
    stop("The vector region must be a length of 1 or the number of rows of your coord parameter")
  }
  if (!all(grepl("(^world$)|(^region$)|(^continent$)", tolower(level)))) {
    stop("The level parameter must be one of this tree levels: 'region', 'continent' or 'world'")
  }


  # if the user have the level set on world
  if (all(level == "world")) {
    return(rep("Pantropical", nrow(coord)))
  }


  # raster ------------------------------------------------------------------

  RAST <- raster(cacheManager("FELD"))
  # Extract the raster value
  RASTval <- extract(RAST, coord)
  FeldRegion <- as.vector(factorValues(RAST, RASTval, att = "Region"))[, 1]

  level <- tolower(level)

  if (all(level == "region")) {
    return(FeldRegion)
  }


  # level different from world and region -------------------------------------



  # if the user choose to take a level
  if (length(level) == 1) {
    level <- rep(level, length(FeldRegion))
  }

  # Replace the world level by Pantropical
  FeldRegion[ level == "world" ] <- "Pantropical"

  # Replace the continent level by different value:

  FeldRegion[ level == "continent" ] <- sub(
    pattern = ".+(Africa)", replacement = "Africa",
    FeldRegion[ level == "continent" ]
  )

  FeldRegion[ level == "continent" ] <- sub(
    pattern = ".+(Amazonia|Shield)", replacement = "SAmerica",
    FeldRegion[ level == "continent" ]
  )

  if (anyNA(FeldRegion)) {
    warning("There is NA in your final vector, those NA will be replaced by 'Pantropical'")
  }

  FeldRegion[is.na(FeldRegion)] <- "Pantropical"

  return(FeldRegion)
}
