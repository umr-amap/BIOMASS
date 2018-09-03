\name{param_4}
\alias{param_4}
\docType{data}
\title{
Posterior distribution of Chave et al. 2014 equation 4 parameters
}
\description{
This matrix contains the posterior distribution of the parameters of Equation 4 of Chave et al., (2014), obtained in a Bayesian framework with uninformative
priors through a Metropolis algorithm.
}
\usage{data("param_4")}
\format{
  A data frame with 1001 observations on the following 3 variables.
  \describe{
    \item{\code{intercept}}{Vector of intercept values}
    \item{\code{logagbt}}{Vector of the model coefficients associated with the product wood density * diameter^2 * height}
	\item{\code{sd}}{Vector of model residual standard error (RSE) values}
  }
}
\details{
This dataset is used in the function \code{\link{AGBmonteCarlo}}.
}
\references{
Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, Global Change Biology, 20 (10), 3177-3190}
\examples{
data(param_4)
str(param_4)
}
\keyword{datasets, AGBmonteCarlo}
\keyword{internal}
