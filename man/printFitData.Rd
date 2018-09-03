\name{printFitData}
\alias{printFitData}
\title{
Printing model's information
}
\description{
This function prints a set of statistics (RSE, Average Bias) to assess the quality of the fit. 
}
\usage{
printFitData(H, Hpredict, mod)
}
\arguments{
  \item{H}{
	Vector of the observed heights
}
  \item{Hpredict}{
	Vector of the predicted heights
}
  \item{mod}{
	A model obtained by functions of type \code{\link{lm}}. 
}
}
\author{
Ariane TANGUY, Maxime REJOU-MECHAIN
}
\examples{
# Load a database
data(NouraguesHD)

# Create a model
model <- lm(log(NouraguesHD$H)~log(NouraguesHD$D))

plot(log(na.omit(NouraguesHD$H)), model$fitted.values)
printFitData(H = NouraguesHD$H, Hpredict = model$fitted.values, mod = model)
}
\keyword{internal}