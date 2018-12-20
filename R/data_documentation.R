#' Angiosperm Phylogeny Group (APG III) dataset UPDATE WITH APG IV?!
#'
#' APGIII Families taken from the Angiosperm Phylogeny Website (http://www.mobot.org/MOBOT/research/APweb/)
#'
#' @docType data
#' @format
#' A data frame with 502 observations on the following 2 variables:
#'   - `order`: Vector of order
#'   - `famAPG`: Vector of APGIII families
#'
#' @usage data("apgFamilies")
#' @source Stevens, P. F. (2001 onwards). _Angiosperm Phylogeny Website_.
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
#' Weibull coefficients from a height-diameter model of the form \eqn{H = a(1-exp(-b*D^c))} given by Feldpausch
#' et al. 2012. in the table 3, with the associated RSE.
#'
#' @docType data
#' @usage data("feldCoef")
#' @format
#' A data frame with 12 observations on the following 4 variables:
#'   - `a`: Coefficient a
#'   - `b`: Coefficient b
#'   - `c`: Coefficient c
#'   - `RSE`: Vector of RSE
#'
#' @details This dataset is used in the function [retrieveH()]
#' to predict height from diameter depending on the region.
#' @references
#' Feldpausch, T.R., et al. (2012). _Tree height integrated into pantropical forest biomass estimates_.
#' Biogeosciences, 9, 3381–3403.
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
#'   - `family`: Vector of families APGIII corrected
#'   - `genus`: Vector of genus
#'
#' @source
#' WCSP (2015). _World Checklist of Selected Plant Families_.
#' Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet; http://apps.kew.org/wcsp/ Retrieved 2015-12-17.
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
#' A data frame with 65889 observations on the following 8 variables :
#'   - `plotId`: Names of the plots
#'   - `treeId`: Tree Id, contains a letter (A, B, C...) when an individual has multiple stems
#'   - `family`: Family
#'   - `genus`: Genus
#'   - `species`: Species
#'   - `D`: Diameter (cm)
#'   - `lat`: Latitude
#'   - `long`: Longitude
#'
#' @references
#' Ramesh, B. R. et al. (2010).
#' _Forest stand structure and composition in 96 sites along environmental gradients in the central Western Ghats of India_
#' Ecological Archives E091-216. Ecology, 91(10), 3118-3118.
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
#'   - `plotId`: Names of the plots
#'   - `genus`: Genus
#'   - `species`: Species
#'   - `D`: Diameter (cm)
#'   - `H`: Height (m)
#'   - `lat`: Latitude
#'   - `long`: Longitude
#'
#' @references
#' Réjou-Méchain, M. et al. (2015).
#' _Using repeated small-footprint LiDAR acquisitions to infer spatial and temporal variations of a high-biomass Neotropical forest_
#' Remote Sensing of Environment, 169, 93-101.
#' @examples
#' data(NouraguesHD)
#' str(NouraguesHD)
#' @keywords datasets
"NouraguesHD"




#' Posterior distribution of Chave et al.'s 2014 equation 4 parameters
#'
#' This matrix contains the posterior distribution of the parameters of Equation 4 of Chave et al. (2014),
#' obtained in a Bayesian framework with uninformative priors through a Metropolis algorithm.
#'
#' @docType data
#' @usage data("param_4")
#' @format
#' A data frame with 1001 observations on the following 3 variables.
#'   - `intercept`: Vector of intercept values
#'   - `logagbt`: Vector of the model coefficients associated with the product wood density * diameter^2 * height
#'   - `sd`: Vector of model residual standard error (RSE) values
#'
#' @details This dataset is used in the function [AGBmonteCarlo()].
#' @references
#' Chave et al. (2014) _Improved allometric models to estimate the aboveground biomass of tropical trees_,
#' Global Change Biology, 20 (10), 3177-3190
#' @examples
#' data(param_4)
#' str(param_4)
#' @keywords datasets AGBmonteCarlo internal
"param_4"


#' Posterior distribution of parameters associated with the equation 7 by Chave et al. 2014.
#'
#' This matrix contains the posterior distribution of the parameters of the Equation 7 of Chave et al., (2014),
#' obtained in a Bayesian framework with uninformative priors through a Metropolis algorithm.
#'
#' @docType data
#' @usage data("param_7")
#' @format
#' A data frame with 1001 observations on the following 9 variables.
#'   - `intercept`: Vector of intercept values
#'   - `logwsg`: Vector of the model coefficients associated with log(wood density)
#'   - `logdbh`: Vector of the model coefficients associated with log(diameter)
#'   - `logdbh2`: Vector of the model coefficients associated with log(diameter)^2
#'   - `E`: Vector of the model coefficients associated with the environmental index E
#'   - `sd`: Vector of model residual standard error (RSE) values
#'   - `temp`: Vector of the model coefficients associated with temperature seasonality
#'   - `cwd`: Vector of the model coefficients associated with climatic water deficit
#'   - `prec`: Vector of the model coefficients associated with precipitation seasonality
#'
#' @details This dataset is used in the function [AGBmonteCarlo()].
#' @references
#' Chave et al. (2014) _Improved allometric models to estimate the aboveground biomass of tropical trees_,
#' Global Change Biology, 20 (10), 3177-3190
#' @examples
#' data(param_7)
#' str(param_7)
#' @keywords datasets AGBmonteCarlo internal
"param_7"



#' Mean standard deviation of wood density estimates at different taxonomic levels
#'
#' This dataset gives the mean standard deviation of wood density values of the [wdData] dataset
#' at different taxonomical levels only considering taxa having more than 10 different values.
#' This dataset is used in the function [getWoodDensity()] to associate at the appropriate taxonomic
#' level a mean error to wood density estimate.
#'
#' @docType data
#' @usage data("sd_10")
#' @format
#' A data frame with 3 observations on the following 2 variables:
#'   - `taxo`: Character vector with the different taxonomical levels (family, genus, species)
#'   - `sd`: Numeric vector giving the mean standard deviation of wood density values
#'
#' @details This dataset is used in the function [getWoodDensity()].
#' @references
#' Rejou-Mechain et al. (2017).
#'  _BIOMASS: An R Package for estimating above-ground biomass and its uncertainty in tropical forests_.
#'  Methods in Ecology and Evolution, 8 (9), 1163-1167.
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
#'   - `family`: a character vector indicating the family
#'   - `genus`: a character vector indicating the genus
#'   - `species`: a character vector indicating the species
#'   - `wd`: a numeric vector of wood densities (g/cm-3)
#'   - `region`: a character vector of regions (see [getWoodDensity()])
#'   - `referenceNumber`: a numeric vector of reference numbers (bibliography)
#'   - `regionId`: a character vector of region ids
#'
#' @details This dataset is used in the function [getWoodDensity()], to estimate a taxon-average wood density value.
#' @references
#' Chave et al. (2009) _Towards a worldwide wood economics spectrum._ Ecology letters 12:4, 351-366.
#' @source Zanne et al. _Global wood density database._ Dryad. Identifier: http://datadryad.org/handle/10255/dryad.235 (2009).
#' @examples
#' data(wdData)
#' str(wdData)
#' @keywords datasets wood density getWoodDensity internal
"wdData"


#' @name HDmethods
#'
#' @title HDmethods
#'
#' @description  Methods used for modeling height-diameter relationship
#'
#' @details
#' These functions model the relationship between tree height (H) and diameter (D).
#' __loglogFunction__
#' Compute three types of log model (log, log2 and log3) to predict H from D.
#' The model can be:
#'   - log 1: \eqn{log(H) = a+ b*log(D)} (equivalent to a power model)
#'   - log 2: \eqn{log(H) = a+ b*log(D) + c*log(D)^2}
#'   - log 3: \eqn{log(H) = a+ b*log(D) + c*log(D)^2 + d*log(D)^3}
#'
#' __michaelisFunction__
#' Construct a Michaelis Menten model of the form: \deqn{H = (A * D) / (B + D)} (A and B are the model parameters to be estimated)
#'
#' __weibullFunction__
#' Construct a three parameter Weibull model of the form: \deqn{H = a*(1-exp(-(D/b)^c))} (a, b, c are the model parameters to be estimated)
#'
#'
#' @param data Dataset with the informations of height (H) and diameter (D)
#' @param method In the case of the loglogFunction, the model is to be chosen between log1, log2 or log3.
#' @param weight (optional) Vector indicating observation weights in the model.
#'
#' @return All the functions give an output similar to the one given by [stats::lm()], obtained for
#' `michaelisFunction` and `weibullFunction` from [minpack.lm::nlsLM]).
#'
#' @references
#' Michaelis, L., & Menten, M. L. (1913). _Die kinetik der invertinwirkung_. Biochem. z, 49(333-369), 352.
#' Weibull, W. (1951). _Wide applicability_. Journal of applied mechanics, 103.
#' Baskerville, G. L. (1972). _Use of logarithmic regression in the estimation of plant biomass_.
#' Canadian Journal of Forest Research, 2(1), 49-53.
#'
#' @author Maxime REJOU-MECHAIN, Ariane TANGUY
#'
#' @seealso [modelHD()], [lmfor::HDmodels()]
#'
#'
#' @keywords Internal
NULL
