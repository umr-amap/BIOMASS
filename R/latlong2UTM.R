#' Translate the long lat coordinate in UTM coordinate
#'
#' @inheritParams computeE
#'
#' @return a data frame whith :
#' \describe{
#'    \item[long] The longitude of the entry
#'    \item[lat] The latitude of the entry
#'    \item[codeUTM] The code proj4 for UTM
#'    \item[X] The X UTM coordinate
#'    \item[Y] The Y UTM coordinate
#' }
#' @export
#'
#' @examples
latlong2UTM = function(Coord){
  
  setDT( Coord )
  setnames(Coord, colnames(Coord), c("long", "lat"))
  
  if( !("proj4" %in% rownames(installed.packages())) ){
    
    stop("Please install the package 'proj4'")
    
    # input = "a"
    # while (!( input == "y" || input == "n" )){
    #   message("To use this function, you must install the package proj4, do you want to install it now ? (y/n)")
    #   input = tolower( scan(n = 1, what = character()) )
    # }
    # if( input == "n" )
    #   stop()
    # 
    # if( tolower(Sys.info()[["sysname"]]) == "linux" ){
    #   message("If the instalation fail, please install the package 'rgdal' by using this command in the terminal : \n\n",
    #           "\t\tsudo apt install libproj-dev")
    # }
    # 
    # install.packages("proj4")
  }
  
  # Function to find UTM zone: assumes that data longitudes to the west of the 
  # Prime Meridian are encoded as running from -180 to 0 degrees
  codelatlong2UTM <- function(long,lat){
    Nzone=(floor((long + 180)/6) %% 60) + 1
    Nzone = paste0(Nzone, ifelse(lat >= 0, " +north ", " +south "))
    Nzone = paste0("+proj=utm +zone=", Nzone,"+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    return(Nzone)
  }
  
  # Convert into UTM
  Coord[, codeUTM := codelatlong2UTM(long, lat)]
  Coord[, c("X", "Y") := split( t(project(cbind( long, lat ), proj = unique(codeUTM))), 1:2), by = codeUTM]
  
  setDF(Coord)
  
  return(Coord)
}