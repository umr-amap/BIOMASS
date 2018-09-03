\name{getWoodDensity}
\alias{getWoodDensity}
\title{
Estimating wood density
}
\description{
The function estimates the wood density (WD) of the trees from their taxonomy or from their congeners using the global wood density database (Chave et al. 2009, Zanne et al. 2009) or any additional dataset. The WD can either be attributed to an individual at a species, genus, family or stand level.
}
\usage{
getWoodDensity(genus, species, stand = NULL, family = NULL, region = "World", 
				addWoodDensityData = NULL)
}
\arguments{
  \item{genus}{
  Vector of genus names
}
  \item{species}{
  Vector of species names
}
  \item{stand}{
  (optional) Vector with the corresponding stands of your data. If set, the missing wood densities at the genus level will be attributed at stand level. If not, the value attributed will be the mean of the whole tree dataset.
}
  \item{family}{
  (optional) Vector of families. If set, the missing wood densities at the genus level will be attributed at family level if available.
}
  \item{region}{
  Region of interest of your sample. By default, Region is set to 'World', but you can restrict the WD estimates to a single region : 
  \itemize{
  \item \code{AfricaExtraTrop}: Africa (extra tropical)
  \item \code{AfricaTrop}: Africa (tropical)
  \item \code{Australia}: Australia
  \item \code{AustraliaTrop}: Australia (tropical)
  \item \code{CentralAmericaTrop}: Central America (tropical)
  \item \code{China}: China
  \item \code{Europe}: Europe
  \item \code{India}: India
  \item \code{Madagascar}: Madagascar
  \item \code{Mexico}: Mexico
  \item \code{NorthAmerica}: North America
  \item \code{Oceania}: Oceania
  \item \code{SouthEastAsia}: South-East Asia
  \item \code{SouthEastAsiaTrop}: South-East Asia (tropical)
  \item \code{SouthAmericaExtraTrop}: South America (extra tropical)
  \item \code{SouthAmericaTrop}: South America (tropical)
  \item \code{World}: World
  }
  }
  \item{addWoodDensityData}{
  A dataframe containing additional wood density data to be combined with the global wood density database. The dataframe should be organized in a dataframe with three columns: "genus","species","wd" (column order and names should be respected).
  }
}
\details{
The function assigns to each taxon a species- or genus- level average if at least one wood density value at the genus level is available for that taxon in the reference database. If not, the mean wood density of the family (if set) or of the stand (if set) is given.

The function also provides an estimate of the error associated with the wood density estimate (i.e. a standard deviation): a mean standard deviation value is given to the tree at the appropriate taxonomic level using the sd_10 dataset (see \code{\link{sd_10}}).
}
\value{
Returns a dataframe containing the following information: 
\item{family}{(if set) Family}
\item{genus}{Genus}
\item{species}{Species}
\item{meanWD}{Mean wood density}
\item{sdWD}{Standard deviation of the wood density that can be used in error propagation (see \code{\link{sd_10}} and \code{\link{AGBmonteCarlo}})}
\item{levelWD}{Level at which wood density has been calculated. Can be species, genus, family, Dataset (mean of the entire dataset) or, if stand is set, the name of the stand (mean of the current stand)}
\item{nInd}{Number of individuals taken into account to compute the mean wood density}
}
\references{
Chave, Jerome, et al. \emph{Towards a worldwide wood economics spectrum.} Ecology letters 12.4 (2009): 351-366.

Zanne, A. E., et al. \emph{Global wood density database.} Dryad. Identifier: http://hdl. handle. net/10255/dryad 235 (2009).
}
\author{
Maxime REJOU-MECHAIN, Ariane TANGUY
}
\seealso{
\code{\link{wdData}}, \code{\link{sd_10}}
}
\examples{
# Load a data set
data(KarnatakaForest)

# Compute the Wood Density up to the genus level and give the mean wood density of the dataset
WD <- getWoodDensity(genus = KarnatakaForest$genus, 
					species = KarnatakaForest$species)

# Compute the Wood Density up to the genus level and then give the mean wood density per stand
WD <- getWoodDensity(genus = KarnatakaForest$genus, 
					species = KarnatakaForest$species, 
					stand = KarnatakaForest$plotId)

# Compute the Wood Density up to the family level and then give the mean wood density per stand
WD <- getWoodDensity(family = KarnatakaForest$family, 
					genus = KarnatakaForest$genus, 
					species = KarnatakaForest$species, 
					stand = KarnatakaForest$plotId)
str(WD)
}
\keyword{Wood density}
