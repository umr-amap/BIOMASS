\name{sd_10}
\alias{sd_10}
\docType{data}
\title{
Mean standard deviation of wood density estimates at different taxonomic levels
}
\description{
This dataset gives the mean standard deviation of wood density values of the \code{\link{wdData}} dataset at different taxonomical levels only considering taxa having more than 10 different values. 
This dataset is used in the function \code{\link{getWoodDensity}} to associate at the appropriate taxonomic level a mean error to wood density estimate.
}
\usage{data("sd_10")}
\format{
  A data frame with 3 observations on the following 2 variables:
  \describe{
    \item{\code{taxo}}{Character vector with the different taxonomical levels (family, genus, species)}
    \item{\code{sd}}{Numeric vector giving the mean standard deviation of wood density values}
  }
}
\details{
This dataset is used in the function \code{\link{getWoodDensity}}.
}
\examples{
data(sd_10)
str(sd_10)
}
\keyword{datasets, wdData, getWoodDensity}
\keyword{internal}
