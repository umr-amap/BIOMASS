\name{apgFamilies}
\alias{apgFamilies}
\docType{data}
\title{
Angiosperm Phylogeny Group (APG III) dataset
}
\description{
APGIII Families taken from the Angiosperm Phylogeny Website (http://www.mobot.org/MOBOT/research/APweb/)
}
\usage{data("apgFamilies")}
\format{
  A data frame with 502 observations on the following 2 variables: 
  \describe{
    \item{\code{order}}{Vector of order}
    \item{\code{famAPG}}{Vector of APGII families}
  }
}
\source{
Stevens, P. F. (2001 onwards). \emph{Angiosperm Phylogeny Website}. Version 12, July 2012. Retrieved on 2016-07-25 http://www.mobot.org/MOBOT/research/APweb/
}
\examples{
data(apgFamilies)
str(apgFamilies)
}
\keyword{datasets}
\keyword{internal}