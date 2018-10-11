#' Retrieving tree height from models
#' 
#' From the diameter and either i) a model, ii) the coordinates of the plot or iii) the region, this function gives an estimation of the total tree height. 
#'
#' @param D Vector of diameters.
#' @param model A model output by the function \code{\link{modelHD}}.
#' @param coord Coordinates of the site(s), either a vector (e.g. c(longitude, latitude)) or a matrix/dataframe with two columns (e.g. cbind(longitude, latitude)).
#' @param region Area of your dataset to estimate tree height thanks to Weibull-H region-, continent-specific and pantropical models proposed by Feldpausch et al. (2012). To be chosen between: 
#' \itemize{
#'   \item \code{Africa}: Africa
#'   \item \code{CAfrica}: Central Africa
#'   \item \code{EAfrica}: Eastern Africa
#'   \item \code{WAfrica}: Western Africa
#'   \item \code{SAmerica}: Southern America
#'   \item \code{BrazilianShield}: Brazilian Shield
#'   \item \code{ECAmazonia}: East-Central Amazonia
#'   \item \code{GuianaShield}: Guiana Shield
#'   \item \code{WAmazonia}: Western Amazonia
#'   \item \code{SEAsia}: South-Eastern Asia
#'   \item \code{NAustralia}: Northern Australia
#'   \item \code{Pantropical}: Pantropical
#' }
#'
#' @return Returns a list  with: 
#' \item{H}{H predicted by the model}
#' \item{RSE}{Residual Standard Error of the model}
#' @references 
#' Feldpausch et al. \emph{Tree height integrated into pantropical forest biomass estimates.} Biogeosciences (2012): 3381-3403.
#' Chave et al. \emph{Improved allometric models to estimate the aboveground biomass of tropical trees.} 
#' Global change biology 20.10 (2014): 3177-3190.
#' @author Ariane TANGUY, Maxime REJOU-MECHAIN, Arthur PERE
#' @seealso \code{\link{modelHD}}
#' @export
#'
#' @examples
#' # Load a database
#' data(NouraguesHD)
#' model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
#' 
#' # If any height model is available
#' H <- retrieveH(D = NouraguesHD$D, model = model)
#' 
#' # If the only data available are the coordinates of your spot
#' lat <- 4.08
#' long <- -52.68
#' coord <- cbind(long, lat)
#' \dontrun{H <- retrieveH(D = NouraguesHD$D, coord = coord)}
#' 
#' # If the only data available is the region of your spot
#' H <- retrieveH(D = NouraguesHD$D, region = "GuianaShield")

retrieveH <- function(D, model = NULL, coord = NULL, region = NULL)
{  
  

  # parameters verification -------------------------------------------------

  if(is.null(model) & is.null(region) & is.null(coord))
    stop("Either model, region, coord should be given")
  
  if(!is.null(region) && length(region) != 1 && length(region) != length(D)) 
    stop("The number of region does not match the number of trees")
  
  if ( !is.null(coord) && ( (is.vector(coord) && length(coord) != 2) || (is.matrix(coord) && nrow(coord) != length(D)) ) )
    stop("coord should be either
             - a vector (e.g. c(longitude, latitude))
             - a matrix with two columns (longitude and latitude) 
             having the same number of rows as the number of trees (length(D))")
  
  if( (!is.null(model) && !is.null(coord)) || (!is.null(model) && !is.null(region)) || (!is.null(coord) && !is.null(region)) )
    stop("Too many input, choose one input among those arguments:
              - H and Herr
              - HDmodel
              - coord")
  
  
  
  # First case : with a model fitted with HDfunction
  if(!is.null(model))
  {
    H <- predictHeight(D, model)
    RSE <- model$RSE
  }
  else
  {
    # Second case : with the coordinates of your site, find the E index and estimate the H following Chave et al. 2014 Global Change Biology
    if(!is.null(coord))
    {
      if(is.null(dim(coord))) 
        coord <- as.matrix(t(coord))
      
      if(nrow(coord) == 1)
        coord <- cbind(rep(coord[1], length(D)), rep(coord[2], length(D)))
      
      
      logD <- log(D)
      E <- computeE(coord) # E = environmental index in Chave et al. 2014
      
      # eq 6a Chave et al. 2014
      logH <- 0.893 - E + 0.760 * logD - 0.0340 * I(logD^2)
      RSE <- 0.243
      H <- exp(logH + 0.5*RSE^2)
    }
    else
    {
      # Third case : with the region, use the weibull parameters from Feldpaush et al. 2012 Biogeosciences
      
      feldCoef <- BIOMASS::feldCoef
      a <- feldCoef[region, "a"]
      b <- feldCoef[region, "b"]
      c <- feldCoef[region, "c"]
      RSE <- feldCoef[region, "RSE"]
      # eq 5 in Feldpaush et al. 2012
      H <- a * (1 - exp(-b * D^c))
    }
  }
  output <- list(H = H, RSE = RSE)  
  return(output)
}
