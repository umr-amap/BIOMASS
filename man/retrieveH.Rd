\name{retrieveH}
\alias{retrieveH}
\title{
Retrieving tree height from models
}
\description{
From the diameter and either i) a model, ii) the coordinates of the plot or iii) the region, this function gives an estimation of the total tree height. 
}
\usage{
retrieveH(D, model = NULL, coord = NULL, region = NULL)
}
\arguments{
  \item{D}{
	Vector of diameters.
}
 \item{model}{
	A model output by the function \code{\link{modelHD}}.
}
 \item{coord}{
  Coordinates of the site(s), either a vector (e.g. c(longitude, latitude)) or a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)).
}
  \item{region}{
  Area of your dataset to estimate tree height thanks to Weibull-H region-, continent-specific and pantropical models proposed by Feldpausch et al. (2012). To be chosen between: 
	\itemize{
	\item \code{Africa}: Africa
	\item \code{CAfrica}: Central Africa
	\item \code{EAfrica}: Eastern Africa
	\item \code{WAfrica}: Western Africa
	\item \code{SAmerica}: Southern America
	\item \code{BrazilianShield}: Brazilian Shield
	\item \code{ECAmazonia}: East-Central Amazonia
	\item \code{GuianaShield}: Guiana Shield
	\item \code{WAmazonia}: Western Amazonia
	\item \code{SEAsia}: South-Eastern Asia
	\item \code{NAustralia}: Northern Australia
	\item \code{Pantropical}: Pantropical
	}
}
}
\value{
Returns a list  with: 
  \item{H}{H predicted by the model}
  \item{RSE}{Residual Standard Error of the model}
}
\references{
Feldpausch et al. \emph{Tree height integrated into pantropical forest biomass estimates.} Biogeosciences (2012): 3381-3403.

Chave et al. \emph{Improved allometric models to estimate the aboveground biomass of tropical trees.} Global change biology 20.10 (2014): 3177-3190.
}
\author{
Ariane TANGUY, Maxime REJOU-MECHAIN
}
\seealso{
\code{\link{modelHD}}
}
\examples{
# Load a database
data(NouraguesHD)
model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")

# If any height model is available
H <- retrieveH(D = NouraguesHD$D, model = model)

# If the only data available are the coordinates of your spot
lat <- 4.08
long <- -52.68
coord <- cbind(long, lat)
\dontrun{H <- retrieveH(D = NouraguesHD$D, coord = coord)}

# If the only data available is the region of your spot
H <- retrieveH(D = NouraguesHD$D, region = "GuianaShield")
}
