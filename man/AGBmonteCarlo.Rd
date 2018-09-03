\name{AGBmonteCarlo}
\alias{AGBmonteCarlo}

\title{
Propagating above-ground biomass (AGB) or carbon (AGC) errors to the stand level
}
\description{
Propagation of the errors throughought the steps needed to compute AGB or AGC.
}
\usage{
AGBmonteCarlo(D, WD = NULL, errWD = NULL, H = NULL, errH = NULL, HDmodel = NULL, 
coord = NULL, Dpropag = NULL, n = 1000, Carbon = FALSE, Dlim = NULL)
}
\arguments{
  \item{D}{
  Vector of tree diameters (in cm)
}
  \item{WD}{
  Vector of wood density estimates (in g/cm3)
}
  \item{errWD}{
	Vector of error associated to the wood density estimates (should be of the same size as \code{WD})
}
\item{H}{
  (option 1) Vector of tree heights (in m). If set, \code{errH} must be set too.
}
  \item{errH}{
   (if \code{H}) Residual standard error (RSE) of a model or vector of errors (sd values) associated to tree height values (in the latter case the vector should be of the same lenght as \code{H}).
}
 \item{HDmodel}{
  (option 2) Model used to estimate tree height from tree diameter (output from \code{\link{modelHD}}, see example)
}
  \item{coord}{
  (option 3) Coordinates of the site(s), either a vector giving a single site (e.g. c(longitude, latitude)) or a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)). The coordinates are used to predict height-diameter allometry with bioclimatic variables.
  
}
  \item{Dpropag}{
  This variable can take three kind of values, indicating how to propagate the errors on diameter measurements : a single numerical value or a vector of the same size as \code{D}, both representing the standard deviation associated with the diameter measurements or \code{"chave2004"} (an important error on 5 percent of the measures, a smaller error on 95 percent of the trees).
}
  \item{n}{
  Number of iteration. Cannot be smaller than 50 or larger than 1000. By default \code{n = 1000}
}
  \item{Carbon}{
  (logical) Wether or not the propagation should be done up to the carbon value (FALSE by default).
}
  \item{Dlim}{
  (optional) Minimum diameter (in cm) for which aboveground biomass should be calculated (all diameter below \code{Dlim} will have a 0 value in the output).
}
}
\details{
See Rejou-Mechain et al. (in prep) for all details on the error propagation procedure.
}
\value{
Returns a list  with (if Carbon is FALSE): 
\item{meanAGB}{Mean stand AGB value following the error propagation}
\item{medAGB}{Median stand AGB value following the error propagation}
\item{sdAGB}{Standard deviation of the stand AGB value following the error propagation}
\item{credibilityAGB}{Credibility interval at 95\% of the stand AGB value following the error propagation}
\item{AGB_simu}{Matrix with the AGB of the trees (rows) times the n iterations (columns)}

Returns a list  with (if Carbon is TRUE): 
\item{meanAGC}{Mean stand AGC value following the error propagation}
\item{medAGC}{Median stand AGC value following the error propagation}
\item{sdAGC}{Standard deviation of the stand AGC value following the error propagation}
\item{credibilityAGC}{Credibility interval at 95\% of the stand AGC value following the error propagation}
\item{AGC_simu}{Matrix with the AGC of the trees (rows) times the n iterations (columns)}

}
\references{
Chave, J. et al. (2004). \emph{Error propagation and scaling for tropical forest biomass estimates}. Philosophical Transactions of the Royal Society B: Biological Sciences, 359(1443), 409-420.

Rejou-Mechain et al. (in prep). \emph{BIOMASS: An R Package for estimating above-ground biomass and its uncertainty in tropical forests}.
}
\author{
Bruno HERAULT, Camille PIPONIOT,  Ariane TANGUY, Maxime REJOU-MECHAIN
}
\examples{
# Load a database
data(NouraguesHD)
data(KarnatakaForest)

# Modelling height-diameter relationship
HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")

# Retrieving wood density values
KarnatakaWD <- getWoodDensity(KarnatakaForest$genus, KarnatakaForest$species, 
stand = KarnatakaForest$plotId)

# Propagating errors with a standard error in wood density in one plot
filt <- KarnatakaForest$plotId=="BSP20"
resultMC <- AGBmonteCarlo(D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt], 
                             errWD = KarnatakaWD$sdWD[filt], HDmodel = HDmodel)
str(resultMC)

# If only the coordinates are available
lat <- KarnatakaForest$lat[filt]
long <- KarnatakaForest$long[filt]
coord <- cbind(long, lat)
\dontrun{
resultMC <- AGBmonteCarlo(D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt], 
                             errWD = KarnatakaWD$sdWD[filt], coord = coord)
str(resultMC)}

# Propagating errors with a standard error in wood density in all plots at once
KarnatakaForest$meanWD = KarnatakaWD$meanWD
KarnatakaForest$sdWD = KarnatakaWD$sdWD

\dontrun{
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId, 
                 function(x) AGBmonteCarlo(D = x$D, WD = x$meanWD, errWD = x$sdWD, 
                                           HDmodel = HDmodel, Dpropag = "chave2004"))
meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
credperplot <- sapply(resultMC, "[", 4)}

}
\keyword{ monte carlo}
