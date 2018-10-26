#' Controling files and folders
#' 
#' The function creates a folder (only once) and then controls files that needed to be downloaded and 
#' placed in this folder.
#' 
#' @section Localisation:
#' The localisation of the folder is :
#' \itemize{
#' \item On Linux : \code{~/.local/share/BIOMASS}
#' \item On Mac OS X : \code{~/Library/Application Support/BIOMASS}
#' \item On Windows 7 up to 10 : \code{C:\\Users\\<username>\\AppData\\Local\\BIOMASS\\BIOMASS}
#' \item On Windows XP : \code{C:\\Documents and Settings\\<username>\\Data\\BIOMASS\\BIOMASS}
#' }
#' See this function for more information : \code{\link[rappdirs]{user_data_dir}}
#'
#' @param nameFile the name of the file or folder
#' @param correctTaxo (binary) if we are in the correctTaxo function
#'
#' @return the path to the folder, and the separator if correctTaxo is False
#' @author Arthur PERE
#' @seealso \code{\link[rappdirs]{user_data_dir}}
#' 
#' @keywords Internal
#' @importFrom rappdirs user_data_dir
#' @importFrom utils download.file unzip

folderControl = function(nameFile = "", correctTaxo = FALSE){
  
  sep = ifelse( grepl("win", Sys.info()["sysname"], ignore.case = T), "\\", "/" )
  path = user_data_dir("BIOMASS")
  
  if( !dir.exists( path ) )
    dir.create( path, recursive = T )
  
  if( correctTaxo )
    return(paste(path, "correctTaxo.log", sep = sep))
  
  
  
  path1 = paste(path, nameFile, sep = sep)
  file_exists = F
  ############# if the folder exists in the working directory
  if (file.exists(nameFile)){
    
    if (!file.exists(path1)){
      file.rename(nameFile, path1)
      message("Your folder \"", nameFile, "\" has been moved in this folder : ", path)
    } else {
      message("Your folder \"", nameFile,"\" already exists in this path : ", path, " and in working directory. ",
              "You can delete the folder ", nameFile)
    }
    file_exists = T
    
  }
  
  
  ############# if the folder exists in the designed folder
  if (file.exists(path1))
    file_exists = T
  
  
  if (file_exists){
    ## If the file isn't a zip but exist
    if(!grepl("_zip$", nameFile, ignore.case=TRUE))
      return(list("path" = path1, "sep" = sep))
    
    
    ## If the file is a zip, exists and its size is ok, we do not download again the file
    size = switch (nameFile,
      "E_zip" = 31202482,
      "CWD_zip" = 15765207
    )
    if ( file.info(path1)$size >= size ){
      unzip(path1, exdir = paste(path, strsplit(nameFile, "_")[[1]][1], sep = sep))
      return()
    }
  }
    
  
  
  ############# if the folder does not exist anywhere
  ###### if the folder asked is the World Climate
  if (nameFile == "wc2-5"){
    ### Get the BioClim param from the http://www.worldclim.org website
    bioData <- getData('worldclim', var='bio', res=2.5, path = path)
    unzip(paste(path1, "bio_2-5m_bil.zip", sep = sep), exdir = paste(path, "wc2-5", sep = sep), files = c("bio4.bil", "bio15.bil"))
    
    message("Your file ", nameFile, " has been download and deziped in this folder : ", path)
    return(list("path" = path1, "sep" = sep))
  }
  
  
  
  ###### If nameFile isn't a zip file
  if ( any(grepl("zip", nameFile)) ){
    folderControl(nameFile = paste(nameFile, "zip", sep = "_"))
    return(list("path" = path1, "sep" = sep))
  }
  
  
  zip_url = switch (nameFile,
                    "CWD_zip" = "http://chave.ups-tlse.fr/pantropical_allometry/CWD.bil.zip",
                    "E_zip" = "http://chave.ups-tlse.fr/pantropical_allometry/E.bil.zip"
  )
  
  DEMzip <- download.file(zip_url, destfile = path1)
  unzip(path1, exdir = paste(path, strsplit(nameFile, "_")[[1]][1], sep = sep))
  message("Your file ", nameFile, " has been download and deziped in this folder : ", path)
  
  return()
}