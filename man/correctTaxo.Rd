\name{correctTaxo}
\alias{correctTaxo}
\title{
Checking typos in names
}
\description{
This function corrects typos for a given taxonomic name using the Taxonomic Name Resolution Service (TNRS) via the Taxosaurus interface. This function has been adapted from the \code{tnrs} function from the taxize package (\code{\link[taxize]{tnrs}}).
}
\usage{
correctTaxo(genus, species, score = 0.5)
}
\arguments{
  \item{genus}{
  Vector of genus to be checked. Alternatively, the whole species name (genus + species) or (genus + species + author) may be given (see example).
}
  \item{species}{
  Vector of species to be checked (same size as the genus vector).
}
  \item{score}{
  Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
}
}
\value{
The function returns a dataframe with the corrected (or not) genera and species.
}
\references{
Boyle, B. et al. (2013). \emph{The taxonomic name resolution service: An online tool for automated standardization of plant names}. BMC bioinformatics, 14, 1.

Chamberlain, S. A. and Szocs, E. (2013). \emph{taxize: taxonomic search and retrieval in R}. F1000Research, 2.
}
\author{
Ariane TANGUY, Maxime REJOU-MECHAIN
}

\examples{
\dontrun{correctTaxo(genus = "Astrocarium", species="standleanum")}
\dontrun{correctTaxo(genus = "Astrocarium standleanum")}
}
