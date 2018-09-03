\name{param_7}
\alias{param_7}
\docType{data}
\title{
Posterior distribution of parameters associated with the equation 7 by Chave et al. 2014.
}
\description{
This matrix contains the posterior distribution of the parameters of the Equation 7 of Chave et al., (2014), obtained in a Bayesian framework with uninformative
priors through a Metropolis algorithm.
}
\usage{data("param_7")}
\format{
  A data frame with 1001 observations on the following 9 variables.
  \describe{
	\item{\code{intercept}}{Vector of intercept values}
	\item{\code{logwsg}}{Vector of the model coefficients associated with log(wood density)}
	\item{\code{logdbh}}{Vector of the model coefficients associated with log(diameter)}
	\item{\code{logdbh2}}{Vector of the model coefficients associated with log(diameter)^2}
	\item{\code{E}}{Vector of the model coefficients associated with the environmental index E}
	\item{\code{sd}}{Vector of model residual standard error (RSE) values}
	\item{\code{temp}}{Vector of the model coefficients associated with temperature seasonality}
	\item{\code{cwd}}{Vector of the model coefficients associated with climatic water deficit}
	\item{\code{prec}}{Vector of the model coefficients associated with precipitation seasonality}
	}
  }
\details{
This dataset is used in the function \code{\link{AGBmonteCarlo}}.
}
\references{
Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, Global Change Biology, 20 (10), 3177-3190}
\examples{
data(param_7)
str(param_7)
}
\keyword{datasets, AGBmonteCarlo}
\keyword{internal}
