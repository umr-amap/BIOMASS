#' Function that return a possibly cached file, transparently downloading it if missing
#' 
#' @section Localisation:
#' Cache path discovery protocol
#' 1. BIOMASS.cache option set to an **existing** folder
#' 2. **existing** user data folder [rappdirs::user_data_dir()]
#'     - On Linux : `~/.local/share/R/BIOMASS`
#'     - On Mac OS X : `~/Library/Application Support/R/BIOMASS`
#'     - On Windows 7 up to 10 : `C:\\Users\\<username>\\AppData\\Local\\R\\BIOMASS`
#'     - On Windows XP : `C:\\Documents and Settings\\<username>\\Data\\R\\BIOMASS`
#' 3. fallback to R session tempdir
#'   
#' @param nameFile character. file to resolve cached path.
#' @return file path of the resolved cached file.
#' @importFrom utils download.file unzip
#' @export 
cacheManager <- function(nameFile) {
  
  if(length(nameFile)>1) {
    stop("Only one file at a time please!")
  }
  
  if (nameFile == "correctTaxo.log") {
    return(cachePath("correctTaxo.log"))
  }
  
  if (nameFile == "feldRegion.grd") {
    return(system.file("external", "feldRegion.grd", package = "BIOMASS", mustWork = TRUE))
  }
  
  url <- list(
    E.bil     = "https://github.com/umr-amap/BIOMASS/raw/master/data-raw/climate_variable/E.zip",
    CWD.bil   = "https://github.com/umr-amap/BIOMASS/raw/master/data-raw/climate_variable/CWD.zip",
    bio4.bil  = "https://github.com/umr-amap/BIOMASS/raw/master/data-raw/climate_variable/wc2-5.zip",
    bio15.bil = "https://github.com/umr-amap/BIOMASS/raw/master/data-raw/climate_variable/wc2-5.zip"
  )
  url <- url[[nameFile]]
  
  if(is.null(url)) {
    stop(
      "I don't know how to get this file!",
      "\n  ", nameFile
    )
  }
  
  if(!file.exists(cachePath(nameFile))) {
    dest <- tempfile(fileext = ".zip")
    on.exit(unlink(dest))
    
    download.file(url, dest)
    
    unzip(dest, exdir = cachePath())
  }
  
  if(!file.exists(cachePath(nameFile))) {
    stop("Error while retrieving file ", nameFile)
  }
  
  cachePath(nameFile)
}

#' Function used to build a file path based on a cache folder
#' 
#' Parameters are similar to that of file.path function
#'   
#' @inheritSection cacheManager Localisation
#' 
#' @param ... character vectors. Elements of the subpath of cache path
#' @return A character vector of normalized file path with a source attribute 
#' holding a hint to cache path source ("option", "data", "temp")
#' @export
cachePath <- function(...) {
  
  # user defined path
  basePath <- getOption("BIOMASS.cache")
  src <- "option"
  
  # if no user defined path, use subdir of user data dir
  if(is.null(basePath)) {
    # appauthor defaults to appname on windows resulting in duplicated sub path
    basePath <- rappdirs::user_data_dir("R/BIOMASS", appauthor=NULL) 
    src <- "data"
  }
  
  # if user defined or user data path does not exist
  # fallback to R session temporary folder
  if(!dir.exists(basePath)) {
    basePath <- file.path(tempdir(check=TRUE), "BIOMASS")
    src <- "temp"
  }
  
  # return path built from base cache path and given subdir (...)
  # and how it was built
  structure(
    normalizePath(do.call(file.path, c(basePath, list(...))), mustWork = FALSE),
    source = src
  )
}

#' Function used to create or activate a permanent cache.
#' 
#' Permanent cache is located by default in user data dir.
#' 
#' You can provide a custom path (that will be defined as a BIOMASS.cache option)
#' but clearCache function will refuse to operate on it for security reasons.
#' @param path Use a custom path to host cache
#' @return No return value, called for side effects
#' @export
createCache <- function(path=NULL) {
  if(is.null(path)) {
    path <- rappdirs::user_data_dir("R/BIOMASS", NULL)
  } else {
    options(BIOMASS.cache=path)
    message("options(BIOMASS.cache=\"", path, "\")")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(NULL)
}

#' Function to clear cache content and possibly remove it
#' 
#' It will refuse to clear or remove a custom cache folder set using BIOMASS.cache
#' option as we don't know whether this folder contains other possibly valuable
#' files apart from our cached files.
#' 
#' @param remove logical. If TRUE cache folder will be removed too (not only content)
#' resulting in deactivating cache as a side effect
#' @return No return value, called for side effects
#' @importFrom utils askYesNo
clearCache <- function(remove=FALSE) {
  basePath <- cachePath()
  
  # temporary folder will be removed automatically at the end of the session
  if(attr(basePath, "source")=="temp") {
    return()
  }
  
  # prevent clearing custom path
  if(attr(basePath, "source")=="option") {
    if(remove) {
      options(BIOMASS.cache=NULL)
    }
    stop(
      "Custom cache path defined using BIOMASS.cache option must be removed manually!",
      "\n  ", basePath,
      if(remove)
        "\n  Resetting option BIOMASS.cache to simulate deactivation"
    )
  }
  
  # Ask for confirmation
  if(askYesNo(paste(
    "Warning! This will permanently remove files from\n", 
    basePath,
    "\n\n Continue?"), FALSE)) {
    
    if(remove) {
      unlink(basePath, recursive = TRUE)
    } else {
      unlink(list.files(basePath, include.dirs = TRUE, full.names = TRUE), recursive = TRUE)
    }
  }
  
  invisible(NULL)
}

