\name{HDmethods}

\alias{loglogFunction}
\alias{michaelisFunction}
\alias{weibullFunction}

\title{
Modeling height-diameter relationship
}
\description{
These functions model the relationship between tree height (H) and diameter (D).

\bold{loglogFunction} 
Compute three types of log model (log, log2 and log3) to model H from D.
The model can be:
\itemize{
 	\item log 1: \eqn{log(H) = a+ b*log(D)} (equivalent to a power model)
	\item log 2: \eqn{log(H) = a+ b*log(D) + c*log(D)^2}
	\item log 3: \eqn{log(H) = a+ b*log(D) + c*log(D)^2 + d*log(D)^3}
	}
	
\bold{michaelisFunction} 
Construct a Michaelis Menten model of the form: \deqn{H = (A * D) / (B + D)} (A and B are the model parameters to be estimated)

\bold{weibullFunction}
Construct a three parameter Weibull model of the form: \deqn{H = a*(1-exp(-(D/b)^c))} (a, b, c are the model parameters to be estimated)
}
\usage{
loglogFunction(data, method)
michaelisFunction(data, weight = NULL)
weibullFunction(data, weight = NULL)
}

\arguments{
  \item{data}{
  Dataset with the informations of height (H) and diameter (D)
}
  \item{method}{
  In the case of the loglogFunction, the model is to be chosen between log1, log2 or log3.
}
  \item{weight}{
  (optional) Vector indicating observation weights in the model.
}
}

\value{
All the functions give an output similar to the one given by \code{\link{lm}}, obtained for \code{michaelisFunction} and \code{weibullFunction} from \code{\link[minpack.lm]{nlsLM}}).
}
\references{
Michaelis, L., & Menten, M. L. (1913). \emph{Die kinetik der invertinwirkung}. Biochem. z, 49(333-369), 352.

Weibull, W. (1951). \emph{Wide applicability}. Journal of applied mechanics, 103.

Baskerville, G. L. (1972). \emph{Use of logarithmic regression in the estimation of plant biomass}. Canadian Journal of Forest Research, 2(1), 49-53.
}
\author{
Maxime REJOU-MECHAIN, Ariane TANGUY
}
\seealso{
\code{\link{modelHD}}, \code{\link[lmfor]{HDmodels}}
}
\examples{
# Load the data set
data("NouraguesHD")
weight <- (NouraguesHD$D^2) * NouraguesHD$H

# ----------- Log
mod <- loglogFunction(NouraguesHD, method = "log3")

# ----------- Michaelis Menten
mod_nonWeighted <- michaelisFunction(NouraguesHD)
mod_weighted <- michaelisFunction(NouraguesHD, weight)

# ----------- Weibull
mod_nonWeighted <- weibullFunction(NouraguesHD)
mod_weighted <- weibullFunction(NouraguesHD, weight)
}
\keyword{internal}