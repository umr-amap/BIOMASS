#' Angiosperm Phylogeny Group (APG III) dataset
#' 
#' APGIII Families taken from the Angiosperm Phylogeny Website (http://www.mobot.org/MOBOT/research/APweb/)
#' 
#' @docType data
#' @format 
#' A data frame with 502 observations on the following 2 variables: 
#' \describe{
#'   \item{\code{order}}{Vector of order}
#'   \item{\code{famAPG}}{Vector of APGII families}
#' }
#' @usage data("apgFamilies")
#' @source Stevens, P. F. (2001 onwards). \emph{Angiosperm Phylogeny Website}. 
#' Version 12, July 2012. Retrieved on 2016-07-25 http://www.mobot.org/MOBOT/research/APweb/
#' 
#' @examples 
#' data(apgFamilies)
#' str(apgFamilies)
#' @keywords datasets internal
#' 
"apgFamilies"


#' Feldpausch et al. 2012 coefficients for generalized height-diameter models
#' 
#' Weibull coefficients from the Height-Diameter model \eqn{H = a(1-exp(-b*D^c))} by Feldpausch
#' et al. 2012., with the associated RSE.
#' 
#' @docType data
#' @usage data("feldCoef")
#' @format 
#' A data frame with 12 observations on the following 4 variables: 
#' \describe{ 
#'   \item{\code{a}}{Coefficient a}
#'   \item{\code{b}}{Coefficient b}
#'   \item{\code{c}}{Coefficient c}
#'   \item{\code{RSE}}{Vector of RSE}
#' }
#' @details This dataset is used in the function \code{\link{retrieveH}}, 
#' to predict height from diameter depending on the region.
#' @references Feldpausch, Ted R., et al. \emph{Tree height integrated into pantropical forest biomass estimates.} Biogeosciences (2012): 3381-3403.
#' @examples 
#' data(feldCoef)
#' str(feldCoef)
#' @keywords datasets internal
"feldCoef"


#' Genus Family database
#' 
#' To create this database, we combined the genera from The Plant List (http://www.theplantlist.org/1.1/browse/-/-/)
#' and the Vascular Plant Families and Genera from Kew (http://data.kew.org/vpfg1992/genlist.html). 
#' Families were checked against the APGIII families.
#'  
#' @docType data
#' @usage data("genusFamily")
#' @format A data frame with 28107 observations on the following 2 variables: 
#' \describe{
#'   \item{\code{family}}{Vector of families APGIII corrected}
#'   \item{\code{genus}}{Vector of genus}
#' }
#' @source 
#' WCSP (2015). \emph{World Checklist of Selected Plant Families}. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://apps.kew.org/wcsp/ Retrieved 2015-12-17.
#' 
#' The Plant List (2013). Version 1.1. Published on the Internet; http://www.theplantlist.org/ Retrieved 2016-08-25. 
#' @examples
#' data(genusFamily)
#' str(genusFamily)
#' @keywords datasets internal
"genusFamily"


#' Karnataka forest dataset
#' 
#' Dataset from 96 forest plots (1 ha) established in the central Western Ghats of India by Ramesh et al. (2010).
#'  
#' @docType data
#' @usage data("KarnatakaForest")
#' @format 
#' A data frame with 65965 observations on the following 8 variables : 
#' \describe{
#'   \item{\code{plotId}}{Names of the plots}
#'   \item{\code{treeId}}{Tree Id, contains a letter (A, B, C...) when an individual has multiple stems}
#'   \item{\code{family}}{Family}
#'   \item{\code{genus}}{Genus}
#'   \item{\code{species}}{Species}
#'   \item{\code{D}}{Diameter}
#'   \item{\code{lat}}{Latitude}
#'   \item{\code{long}}{Longitude}
#' }
#' @references 
#' Ramesh, B. R. et al. (2010). \emph{Forest stand structure and composition in 96 sites along environmental gradients in the central Western Ghats of India} Ecological Archives E091-216. Ecology, 91(10), 3118-3118. 
#' @examples
#' data(KarnatakaForest)
#' str(KarnatakaForest)
#' @keywords datasets
"KarnatakaForest"


#' Height-Diameter data
#' 
#' Dataset from two 1-ha plots from the Nouragues forest (French Guiana)
#'  
#' @docType data
#' @usage data("NouraguesHD")
#' @format 
#' A data frame with 1051 observations on the following variables :
#' \describe{
#'   \item{\code{plotId}}{Names of the plots}
#'   \item{\code{genus}}{Genus}
#'   \item{\code{species}}{Species}
#'   \item{\code{D}}{Diameter}
#'   \item{\code{H}}{Height}
#'   \item{\code{lat}}{Latitude}
#'   \item{\code{long}}{Longitude}
#' }
#' @references 
#' Ramesh, B. R. et al. (2010). \emph{Forest stand structure and composition in 96 sites along environmental 
#' gradients in the central Western Ghats of India} Ecological Archives E091-216. Ecology, 91(10), 3118-3118. 
#' @examples
#' data(NouraguesHD)
#' str(NouraguesHD)
#' @keywords datasets
"NouraguesHD"




#' Posterior distribution of Chave et al. 2014 equation 4 parameters
#' 
#' This matrix contains the posterior distribution of the parameters of Equation 4 of Chave et al., (2014), 
#' obtained in a Bayesian framework with uninformative priors through a Metropolis algorithm.
#'  
#' @docType data
#' @usage data("param_4")
#' @format 
#' A data frame with 1001 observations on the following 3 variables.
#' \describe{
#'   \item{\code{intercept}}{Vector of intercept values}
#'   \item{\code{logagbt}}{Vector of the model coefficients associated with the product wood density * diameter^2 * height}
#'   \item{\code{sd}}{Vector of model residual standard error (RSE) values}
#' }
#' @details This dataset is used in the function \code{\link{AGBmonteCarlo}}.
#' @references 
#' Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees},
#' Global Change Biology, 20 (10), 3177-3190
#' @examples
#' data(param_4)
#' str(param_4)
#' @keywords datasets AGBmonteCarlo internal
"param_4"


#' Posterior distribution of parameters associated with the equation 7 by Chave et al. 2014.
#' 
#' This matrix contains the posterior distribution of the parameters of the Equation 7 of Chave et al., (2014), 
#' obtained in a Bayesian framework with uninformativepriors through a Metropolis algorithm.
#'  
#' @docType data
#' @usage data("param_7")
#' @format 
#' A data frame with 1001 observations on the following 9 variables.
#' \describe{
#'   \item{\code{intercept}}{Vector of intercept values}
#'   \item{\code{logwsg}}{Vector of the model coefficients associated with log(wood density)}
#'   \item{\code{logdbh}}{Vector of the model coefficients associated with log(diameter)}
#'   \item{\code{logdbh2}}{Vector of the model coefficients associated with log(diameter)^2}
#'   \item{\code{E}}{Vector of the model coefficients associated with the environmental index E}
#'   \item{\code{sd}}{Vector of model residual standard error (RSE) values}
#'   \item{\code{temp}}{Vector of the model coefficients associated with temperature seasonality}
#'   \item{\code{cwd}}{Vector of the model coefficients associated with climatic water deficit}
#'   \item{\code{prec}}{Vector of the model coefficients associated with precipitation seasonality}
#' }
#' @details This dataset is used in the function \code{\link{AGBmonteCarlo}}.
#' @references 
#' Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, 
#' Global Change Biology, 20 (10), 3177-3190
#' @examples
#' data(param_7)
#' str(param_7)
#' @keywords datasets AGBmonteCarlo internal
"param_7"



#' Mean standard deviation of wood density estimates at different taxonomic levels
#' 
#' This dataset gives the mean standard deviation of wood density values of the \code{\link{wdData}} dataset 
#' at different taxonomical levels only considering taxa having more than 10 different values. 
#' This dataset is used in the function \code{\link{getWoodDensity}} to associate at the appropriate taxonomic 
#' level a mean error to wood density estimate.
#'  
#' @docType data
#' @usage data("sd_10")
#' @format 
#' A data frame with 3 observations on the following 2 variables:
#' \describe{
#'   \item{\code{taxo}}{Character vector with the different taxonomical levels (family, genus, species)}
#'   \item{\code{sd}}{Numeric vector giving the mean standard deviation of wood density values}
#' }
#' @details This dataset is used in the function \code{\link{getWoodDensity}}.
#' @references 
#' Chave et al. (2014) \emph{Improved allometric models to estimate the aboveground biomass of tropical trees}, 
#' Global Change Biology, 20 (10), 3177-3190
#' @examples
#' data(sd_10)
#' str(sd_10)
#' @keywords datasets wdData getWoodDensity internal
"sd_10"


#' The global wood density database
#' 
#' The global wood density database (Chave et al. 2009, Zanne et al. 2009).
#'  
#' @docType data
#' @usage data("wdData")
#' @format 
#'  A data frame with 16467 observations on the following 7 variables.
#' \describe{
#'   \item{\code{family}}{a character vector indicating the family}
#'   \item{\code{genus}}{a character vector indicating the genus}
#'   \item{\code{species}}{a character vector indicating the species}
#'   \item{\code{wd}}{a numeric vector of wood densities (g/cm-3)}
#'   \item{\code{region}}{a character vector of regions (see \code{\link{getWoodDensity}})}
#'   \item{\code{referenceNumber}}{a numeric vector of reference numbers (bibliography)}
#'   \item{\code{regionId}}{a character vector of region ids}
#' }
#' @details This dataset is used in the function \code{\link{getWoodDensity}}, to estimate a taxon-average wood density value.
#' @references 
#' Chave et al. \emph{Towards a worldwide wood economics spectrum.} Ecology letters 12.4 (2009): 351-366.
#' @source Zanne et al. \emph{Global wood density database.} Dryad. Identifier: http://datadryad.org/handle/10255/dryad.235 (2009).
#' @examples
#' data(wdData)
#' str(wdData)
#' @keywords datasets wood density getWoodDensity internal
"wdData"


#' @name HDmethods
#' 
#' @title HDmethods
#' 
#' @description  Modeling height-diameter relationship
#' 
#' @details 
#' These functions model the relationship between tree height (H) and diameter (D).
#' \bold{loglogFunction} 
#' Compute three types of log model (log, log2 and log3) to model H from D.
#' The model can be:
#' \itemize{
#'     \item log 1: \eqn{log(H) = a+ b*log(D)} (equivalent to a power model)
#'     \item log 2: \eqn{log(H) = a+ b*log(D) + c*log(D)^2}
#'     \item log 3: \eqn{log(H) = a+ b*log(D) + c*log(D)^2 + d*log(D)^3}
#' }
#' 
#' \bold{michaelisFunction} 
#' Construct a Michaelis Menten model of the form: \deqn{H = (A * D) / (B + D)} (A and B are the model parameters to be estimated)
#' 
#' \bold{weibullFunction}
#' Construct a three parameter Weibull model of the form: \deqn{H = a*(1-exp(-(D/b)^c))} (a, b, c are the model parameters to be estimated)
#' 
#' 
#' @param data Dataset with the informations of height (H) and diameter (D)
#' @param method In the case of the loglogFunction, the model is to be chosen between log1, log2 or log3.
#' @param (optional) Vector indicating observation weights in the model.
#' 
#' @return All the functions give an output similar to the one given by \code{\link{lm}}, obtained for 
#' \code{michaelisFunction} and \code{weibullFunction} from \code{\link[minpack.lm]{nlsLM}}).
#' 
#' @references 
#' Michaelis, L., & Menten, M. L. (1913). \emph{Die kinetik der invertinwirkung}. Biochem. z, 49(333-369), 352.
#' Weibull, W. (1951). \emph{Wide applicability}. Journal of applied mechanics, 103.
#' Baskerville, G. L. (1972). \emph{Use of logarithmic regression in the estimation of plant biomass}. 
#' Canadian Journal of Forest Research, 2(1), 49-53.
#' 
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY
#' 
#' @seealso \code{\link{modelHD}}, \code{\link[lmfor]{HDmodels}}
#' 
#' @examples 
#' # Load the data set
#' data("NouraguesHD")
#' weight <- (NouraguesHD$D^2) * NouraguesHD$H
#' 
#' # ----------- Log
#' mod <- loglogFunction(NouraguesHD, method = "log3")
#' 
#' # ----------- Michaelis Menten
#' mod_nonWeighted <- michaelisFunction(NouraguesHD)
#' mod_weighted <- michaelisFunction(NouraguesHD, weight)
#' 
#' # ----------- Weibull
#' mod_nonWeighted <- weibullFunction(NouraguesHD)
#' mod_weighted <- weibullFunction(NouraguesHD, weight)
#' 
#' @keywords Internal
NULL






