\name{getBioclimParam}
\alias{getBioclimParam}
\title{
Retrieving bioclimatic parameters
}
\description{
This function extracts three bioclimatic parameters thanks to the coordinates of the data: the Climatic Water Deficit (CWD), the Temperature Seasonality (TS) and the Precipitation Seasonality (PS). The function is time-consuming at its first use as it downloads three raster files (one for each of the parameter) which are then stored in the working directory. However, as soon as the raster is downloaded once, the function then runs fast (if the working directory is not changed or if the rasters are then moved to the new working directory).
}
\usage{
getBioclimParam(coord)
}
\arguments{
  \item{coord}{
  Coordinates of the site(s), a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)). 
}
}
\value{
The function returns a data.frame with \code{tempSeas} (temperature seasonality, i.e. bioclimatic variable 4 from the Worldclim dataset; Hijmans et al. 2005), \code{precSeas} (precipitation seasonality, i.e. bioclimatic variable 15 from the Worldclim dataset; Hijmans et al. 2005) and \code{CWD} (climatic water deficit; Chave et al. 2014). 
}
\references{
Hijmans et al. (2005) \emph{Very high resolution interpolated climate surfaces for global land areas}, International journal of climatology, 25(15), 1965-1978.

Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, Global Change Biology, 20 (10), 3177-3190
}
\author{
Ariane TANGUY
}

\examples{
# One study site
lat <- 4.08
long <- -52.68
coord <- cbind(long, lat)
\dontrun{bioclim <- getBioclimParam(coord)}

# Several study sites (here three sites)
long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)
\dontrun{bioclim <- getBioclimParam(coord)}

}
\keyword{ bioclim param }
\keyword{internal}
