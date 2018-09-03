\name{KarnatakaForest}
\alias{KarnatakaForest}
\docType{data}
\title{
Karnataka forest dataset
}
\description{
Dataset from 96 forest plots (1 ha) established in the central Western Ghats of India by Ramesh et al. (2010).
}
\usage{data("KarnatakaForest")}
\format{
  A data frame with 65965 observations on the following 8 variables : 
  \describe{
    \item{\code{plotId}}{Names of the plots}
	\item{\code{treeId}}{Tree Id, contains a letter (A, B, C...) when an individual has multiple stems}
    \item{\code{family}}{Family}
    \item{\code{genus}}{Genus}
    \item{\code{species}}{Species}
    \item{\code{D}}{Diameter}
	\item{\code{lat}}{Latitude}
    \item{\code{long}}{Longitude}
  }
}
\examples{
data(KarnatakaForest)
str(KarnatakaForest)
}

\references{
Ramesh, B. R. et al. (2010). \emph{Forest stand structure and composition in 96 sites along environmental gradients in the central Western Ghats of India} Ecological Archives E091-216. Ecology, 91(10), 3118-3118.
}
\keyword{datasets}
