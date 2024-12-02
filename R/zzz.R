.onLoad <- function(libname, pkgname) {
  
}

.onAttach <- function(libname, pkgname) {
  basePath <- cachePath()
  
  if(attr(basePath, "source")=="temp") {
    packageStartupMessage(
      "Using temporary cache",
      "\n  It is recommended to use a permanent cache to avoid to re-download files on each session.",
      "\n  See function createCache() or BIOMASS.cache option."
    )
  }
  
  if(attr(basePath, "source")=="data") {
    packageStartupMessage(
      "Using user data cache ", basePath,
      "\n  To clear or remove cache see function clearCache()."
    )
  }
}