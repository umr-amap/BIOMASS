\name{predictHeight}
\alias{predictHeight}

\title{
Predicting tree height
}
\description{
The function predicts height from diameter based on a fitted model. 
}
\usage{
predictHeight(D, model, err = FALSE)
}

\arguments{
  \item{D}{
  Vector of diameter (in cm).
}
  \item{model}{
  A height-diameter model output by the function \code{link{modelHD}}
}
  \item{err}{
  If \code{TRUE}, An error is taken randomly from a normal distribution with a mean of zero and a standard deviation equals to the residual standard error of the model (RSE). We only use this argument for the Monte Carlo approach (see \code{\link{AGBmonteCarlo}}), otherwise it should be let as \code{FALSE}, the default case.
}
}
\details{
In the case where the error is \code{FALSE} and the model is a loglog model, we use the Baskerville correction, a bias correction factor used to get unbiased backtransformation values.
}
\value{
Returns a vector of total tree height (in m).
}
\author{
Maxime REJOU-MECHAIN, Ariane TANGUY
}
\seealso{
\code{\link[minpack.lm]{nlsLM}}
}
\examples{
# Load a database
data(NouraguesHD)

# Create a model
model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
# Predict the height
H <- predictHeight(NouraguesHD$D, model)

}
\keyword{internal}