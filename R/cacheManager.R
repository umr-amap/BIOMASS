#' Manage the cache
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
#'
#' @return the path to the file we need
#'
#' @author Arthur PERE
#' @seealso \code{\link[rappdirs]{user_data_dir}}
#'
#' @keywords Internal
#' @importFrom utils download.file unzip

cacheManager <- function(nameFile) {
  
  basePath <- cachePath()

  if (!dir.exists(basePath)) dir.create(basePath, recursive = T, showWarnings = F)

  if (nameFile == "correctTaxo") {
    return(cachePath("correctTaxo.log"))
  }

  if (nameFile == "FELD") {
    return(system.file("external", "feldRegion.grd", package = "BIOMASS", mustWork = T))
  }

  path <- cachePath(nameFile)

  ############# if the folder exists in the designed folder
  file_exists <- file.exists(path)

  ############# if the folder exists in the working directory
  if (file.exists(nameFile) && !file_exists) {
    file.copy(nameFile, basePath, recursive = T)
    file.remove(dir(nameFile, recursive = T, full.names = T), nameFile)
    message("Your folder '", nameFile, "' has been moved in this folder : ", basePath)

    file_exists <- T
  }

  # if the file is empty
  if (file_exists && length(dir(path)) == 0) {
    file.remove(path)
    file_exists <- F
  }




  ############# if the folder does not exist anywhere
  if (!file_exists) {
    tryCatch({
      nameFileZip <- paste0(nameFile, ".zip")

      if (file.exists(nameFileZip)) {
        unzip(nameFileZip, exdir = path)
      } else {
        unzip(paste0(path, ".zip"), exdir = path)
      }
    },
    warning = function(cond) {
      tmp <- tempfile(pattern = nameFile, fileext = ".zip")
      zip_url <- switch(nameFile,
        "wc2-5" = "http://amap-dev.cirad.fr/attachments/download/1525/wc2-5.zip",
        "CWD" = "http://amap-dev.cirad.fr/attachments/download/1520/CWD.zip",
        "E" = "http://amap-dev.cirad.fr/attachments/download/1521/E.zip",
        stop("You demand something we don't understand.")
      )

      DEMzip <- download.file(zip_url, destfile = tmp)
      unzip(tmp, exdir = path)
    },
    finally = {
      message("Your file ", nameFile, " has been download and/or deziped in this folder : ", basePath)
      
      # update the flag
      writeLines(Sys.Date(), cachePath(".last_check"))
    }
    )
  }
  
  
  
  # give the full path expect for the wc2-5 file who have two file we need.
  path = switch (nameFile,
    "wc2-5" = file.path(path, c("bio4.bil", "bio15.bil")),
    file.path(path, paste0(nameFile, ".bil"))
  )
  
  return(path)
}


#' @importFrom rappdirs user_data_dir
#' @keywords Internal
cachePath <- function(path = NULL) {
  basePath <- user_data_dir("BIOMASS")
  if (!is.null(path)) {
    basePath <- file.path(basePath, path)
  }
  basePath
}