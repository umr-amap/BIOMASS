#' Retrieving the Feldpausch regions
#'
#' Extract the Feldpausch et al. (2012)'s regions thanks to local coordinates.
#'
#' @inheritParams computeE
#' @param level a string or a vector of string, the length must be matched the number of row of the parameter coord.
#' This parameter will be used for grow the scale you which to look at for the Feldpausch regions. There are tree levels :
#' \describe{
#'    \item{region}{The smaller scale and the value by default}
#'    \item{continent}{The medium scale, only the value which who are in Africa and America will be changed}
#'    \item{world}{The larger scale, all the value will be replaced by "Pantropical"}
#' }
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
computeFeldRegion <- function(coord, level = "region") {


  # Parameter verification --------------------------------------------------


  if (!(length(level) %in% c(1, nrow(coord)))) {
    stop("The vector region must be a length of 1 or the number of row of your coord parameter")
  }
  if (!all(grepl("(^world$)|(^region$)|(^continent$)", tolower(level)))) {
    stop("The level parameter must have this tree level : 'region', 'continent' or 'world'")
  }


  # if the user have the level set on world
  if (all(level == "world")) {
    return(rep("Pantropical", nrow(coord)))
  }


  # raster ------------------------------------------------------------------


  RAST <- raster(system.file("external/feldRegion.grd", package = "BIOMASS", mustWork = T))
  # Extract the raster value
  RASTval <- extract(RAST, coord)
  FeldRegion <- as.vector(factorValues(RAST, RASTval, att = "Region"))[, 1]

  level <- tolower(level)

  if (all(level == "region")) {
    return(FeldRegion)
  }


  # level different of world and region -------------------------------------



  # if the user choose to take a level
  if (length(level) == 1) {
    level <- rep(level, length(FeldRegion))
  }

  # Replace the world level by Pantropical
  FeldRegion[ level == "world" ] <- "Pantropical"

  # Replace the continent level by different value:
  # The african region will be at the same level Africa
  FeldRegion[ level == "continent" ] <- sub(
    pattern = ".+(Africa)", replacement = "Africa",
    FeldRegion[ level == "continent" ]
  )

  # The south america region will be at the same level SAmerica
  FeldRegion[ level == "continent" ] <- sub(
    pattern = ".+(Amazonia|Shield)", replacement = "SAmerica",
    FeldRegion[ level == "continent" ]
  )

  return(FeldRegion)
}
