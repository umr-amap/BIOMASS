\name{feldCoef}
\alias{feldCoef}
\docType{data}
\title{
Feldpausch et al. 2012 coefficients for generalized height-diameter models
}
\description{
Weibull coefficients from the Height-Diameter model \eqn{H = a(1-exp(-b*D^c))} by Feldpausch et al. 2012., with the associated RSE
}
\usage{data("feldCoef")}
\format{
  A data frame with 12 observations on the following 4 variables: 
  \describe{ 
    \item{\code{a}}{Coefficient a}
    \item{\code{b}}{Coefficient b}
    \item{\code{c}}{Coefficient c}
    \item{\code{RSE}}{Vector of RSE}
  }
}
\details{
This dataset is used in the function \code{\link{retrieveH}}, to predict height from diameter depending on the region.}
\references{
Feldpausch, Ted R., et al. \emph{Tree height integrated into pantropical forest biomass estimates.} Biogeosciences (2012): 3381-3403.
}
\examples{
data(feldCoef)
str(feldCoef)
}
\keyword{datasets}
\keyword{internal}
