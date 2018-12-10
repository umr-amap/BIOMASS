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

  if (!getOption("BIOMASS.ignore_update", TRUE)) {
    checkTime()
  }


  ############# if the folder does not exist anywhere
  if (!file_exists) {
    updateCache(nameFile)
  }



  # give the full path expect for the wc2-5 file who have two file we need.
  path <- switch(nameFile,
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


#' @keywords Internal
checkTime <- function() {
  if (!file.exists(cachePath(".last_check"))) {
    writeLines(as.character(Sys.Date()), cachePath(".last_check"))
  } else {
    
    check_interval <- 1830 # this message will appear every 5 years 
    
    last_check <- as.Date(readLines(cachePath(".last_check")))
    if ((Sys.Date() - last_check) > check_interval) {
      message(
        "You can verify if the cache is updated by using this function\n",
        "\t\tupdateCache()\n",
        "Be careful, all the environement variable will be deleted and updated.\n",
        "You can ignore this message, and if you want to you can prevent this message to appear by using\n",
        "\t\toptions(BIOMASS.ignore_update=TRUE)"
      )

      # update the flag
      writeLines(as.character(Sys.Date()), cachePath(".last_check"))
    }
  }

  return()
}





#' Update the cache for the different function
#' 
#' This function update the cache for the environemental variable, meaning the file :
#' \itemize{
#'   \item wc2-5
#'   \item CWD
#'   \item E
#' }
#'
#' @param nameFile The name of the file you want to update. If it's NULL the function will update all the files.
#'
#' @return NULL
#' @export
#'
#' @author Arthur PERE
#' 
#' @importFrom utils download.file unzip
#' @examples
#' \dontrun{
#'    updateCache()
#' }
updateCache <- function(nameFile = NULL) {

  # the url of the differents zip to download
  zip_urls <- c(
    "http://amap-dev.cirad.fr/attachments/download/1525/wc2-5.zip",
    "http://amap-dev.cirad.fr/attachments/download/1520/CWD.zip",
    "http://amap-dev.cirad.fr/attachments/download/1521/E.zip"
  )


  # dowload and unzip the files
  downloadZip <- function(zip_url, path) {
    tmp <- tempfile(fileext = ".zip")

    DEMzip <- download.file(zip_url, destfile = tmp)
    unzip(tmp, exdir = path)

    # update the flag
    writeLines(as.character(Sys.Date()), cachePath(".last_check"))

    message("Your file ", nameFile, " has been download and/or deziped in this folder : ", path)
    return()
  }


  if (is.null(nameFile)) {
    lapply(zip_urls, function(url_zip) {
      nameFile <- sub(".zip$", "", basename(url_zip))
      path <- cachePath(nameFile)
      downloadZip(url_zip, path)
    })
    message("Your cache had been updated!")

    return()
  }


  nameFileZip <- paste0(nameFile, ".zip")
  path <- cachePath(nameFile)

  tryCatch({
    if (!is.null(nameFile)) {
      ifelse(file.exists(nameFileZip),
        unzip(nameFileZip, exdir = path),
        unzip(paste0(path, ".zip"), exdir = path)
      )
    } else {
      warning()
    }
  },
  warning = function(cond) {
    zip_url <- switch(nameFile,
      "wc2-5" = zip_urls[1],
      "CWD" = zip_urls[2],
      "E" = zip_urls[3],
      stop("You demand something we don't understand.")
    )
    downloadZip(zip_url, path)
    return()
  },
  finally = {
    return()
  }
  )
  return()
}
