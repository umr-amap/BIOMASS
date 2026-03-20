#' Translate the lon lat coordinate in UTM coordinate
#'
#' @inheritParams computeE
#'
#' @return a data frame with :
#'    - `codeUTM`: The code `proj` for UTM
#'
#' @export
#' @importFrom data.table as.data.table :=
#' @examples
#'
#' long <- c(-52.68, -51.12, -53.11)
#' lat <- c(4.08, 3.98, 4.12)
#' coord <- cbind(long, lat)
#' \donttest{
#' UTMcoord <- latlong2UTM(coord)
#' }
#'
getUTM <- function(coord) {
  coord <- data.table(coord, check.names = TRUE)
  setnames(coord, colnames(coord), c("lon", "lat"))

  if (!requireNamespace("proj4")) {
    stop("Please install the package 'proj4'\n
         \t\tinstall.packages('proj4').")
  }

  # Function to find UTM zone: assumes that data longitudes to the west of the
  # Prime Meridian are encoded as running from -180 to 0 degrees
  codelatlong2UTM <- function(lon, lat) {
    Nzone <- (floor((lon + 180) / 6) %% 60) + 1
    Nzone <- paste0(Nzone, ifelse(lat >= 0, " +north ", " +south "))
    Nzone <- paste0("+proj=utm +zone=", Nzone, "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    return(Nzone)
  }

  # Convert into UTM
  coord[, codeUTM := codelatlong2UTM(lon, lat)]
  coord[, c("X", "Y") := proj4::project(.(lon, lat), proj = unique(.BY)), by = codeUTM]

  # Convert to dataframe
  setDF(coord)

  return(coord)
}
