#' Reset all data stored in the cache
#'
#' @description
#' This function removes all the files that are automatically stored in a local folder following the use of 
#' the correctTaxo, getBioclim or computeE functions.
#'
#' @details
#' This function first identifies the path where the cache is stored and then remove it
#' @author Maxime REJOU-MECHAIN
#' @examples
#' removeCache()
#' @export
 

removeCache <- function() {
  # First identify the path of the cache
  basePath <- cachePath()
  # Remove all files contained in the folder
  if (dir.exists(basePath)) unlink("~/.local/share/R/BIOMASS",recursive = T)
}
