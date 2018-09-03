\name{wdData}
\alias{wdData}
\docType{data}
\title{
The global wood density database
}
\description{
The global wood density database (Chave et al. 2009, Zanne et al. 2009).
}
\usage{data("wdData")}
\format{
  A data frame with 16467 observations on the following 7 variables.
  \describe{
    \item{\code{family}}{a character vector indicating the family}
    \item{\code{genus}}{a character vector indicating the genus}
    \item{\code{species}}{a character vector indicating the species}
    \item{\code{wd}}{a numeric vector of wood densities (g/cm-3)}
    \item{\code{region}}{a character vector of regions (see \code{\link{getWoodDensity}})}
    \item{\code{referenceNumber}}{a numeric vector of reference numbers (bibliography)}
    \item{\code{regionId}}{a character vector of region ids}
  }
}
\details{
This dataset is used in the function \code{\link{getWoodDensity}}, to estimate a taxon-average wood density value.
}
\references{
Chave et al. \emph{Towards a worldwide wood economics spectrum.} Ecology letters 12.4 (2009): 351-366.
}

\source{
Zanne et al. \emph{Global wood density database.} Dryad. Identifier: http://datadryad.org/handle/10255/dryad.235 (2009).
}
\examples{
data(wdData)
str(wdData)
}
\keyword{datasets, wood density, getWoodDensity}
\keyword{internal}