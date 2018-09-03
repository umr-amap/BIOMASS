\name{getTaxonomy}
\alias{getTaxonomy}

\title{
Retrieving the taxonomy
}
\description{
From a genus, the function \code{getTaxonomy} finds the APG III family, and optionally the order, from the Genus Family database (see \code{\link{genusFamily}}) and the APGIII dataset (see \code{\link{apgFamilies}})
}
\usage{
getTaxonomy(genus, findOrder = FALSE)
}
\arguments{
  \item{genus}{
  Vector of genus names
}
  \item{findOrder}{
  Boolean. If \code{TRUE}, the output will contain the taxonomical orders of the families.
}
}
\value{
Data frame with the order (if \code{findOrder} is \code{TRUE}), family and genus.
}
\author{
Ariane TANGUY, Maxime REJOU-MECHAIN
}

\examples{
# Find the Family of the Aphelandra genus 
getTaxonomy("Aphelandra")
# ... and the order 
getTaxonomy("Aphelandra", findOrder = TRUE)
}
\keyword{ taxonomy }
