\name{NouraguesHD}
\alias{NouraguesHD}
\docType{data}
\title{
Height-Diameter data
}
\description{
Dataset from two 1-ha plots from the Nouragues forest (French Guiana)
}
\usage{data("NouraguesHD")}
\format{
  A data frame with 1051 observations on the following variables :
  \describe{
    \item{\code{plotId}}{Names of the plots}
	\item{\code{genus}}{Genus}
    \item{\code{species}}{Species}
    \item{\code{D}}{Diameter}
	\item{\code{H}}{Height}
	\item{\code{lat}}{Latitude}
    \item{\code{long}}{Longitude}
  }
}
\examples{
data(NouraguesHD)
str(NouraguesHD)
}
\keyword{datasets}
