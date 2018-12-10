if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "codeUTM", "long", "lat", ".BY"
  ))
}

#' Translate the long lat coordinate in UTM coordinate
#'
#' @inheritParams computeE
#'
#' @return a data frame whith :
#' \describe{
#'    \item{long}{The longitude of the entry}
#'    \item{lat}{The latitude of the entry}
#'    \item{codeUTM}{The code proj4 for UTM}
#'    \item{X}{The X UTM coordinate}
#'    \item{Y}{The Y UTM coordinate}
#' }
#' @export
#' @importFrom data.table as.data.table :=
#' @examples
#' 
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \dontrun{
#' UTMcoord <- latlong2UTM(coord)
#' }
#' 
latlong2UTM <- function(coord) {
  coord <- as.data.table(coord)
  setnames(coord, colnames(coord), c("long", "lat"))

  if (!requireNamespace("proj4")) {
    stop("Please install the package 'proj4'\n
         \t\tinstall.packages('proj4').")
  }

  # Function to find UTM zone: assumes that data longitudes to the west of the
  # Prime Meridian are encoded as running from -180 to 0 degrees
  codelatlong2UTM <- function(long, lat) {
    Nzone <- (floor((long + 180) / 6) %% 60) + 1
    Nzone <- paste0(Nzone, ifelse(lat >= 0, " +north ", " +south "))
    Nzone <- paste0("+proj=utm +zone=", Nzone, "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    return(Nzone)
  }

  # Convert into UTM
  coord[, codeUTM := codelatlong2UTM(long, lat)]
  coord[, c("X", "Y") := proj4::project(.(long, lat), proj = unique(.BY)), by = codeUTM]

  setDF(coord)

  return(coord)
}
