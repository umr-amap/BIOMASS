computeE <- function(coord)
{  
  ### Compute the Environmental Index (Chave et al. 2014)
  
  sep = ifelse(length(grep( "win", Sys.info()["sysname"], ignore.case = T )) != 0, "\\", "/")
  path = paste0( rappdirs::user_data_dir("BIOMASS"), sep )
  
  if( !dir.exists( rappdirs::user_data_dir("BIOMASS")) ){
    dir.create(rappdirs::user_data_dir("BIOMASS"))
  }
  
  ### If the file needed are in the working directory
  if (dir.exists("E") & !dir.exists(paste0(path, "E"))){
    file.rename("E", paste0(path, "E"))
    message("Your repertory \"E\" has been moved in this repertory : ", path)
  }
  
  if (file.exists("E_zip") & !file.exists(paste0(path, "E_zip"))){
    file.rename("E_zip", paste0(path, "E_zip"))
    message("Your repertory \"E_zip\" has been moved in this repertory : ", path)
  }
  
  if (dir.exists("E") & dir.exists(paste0(path, "E"))){
    message("Your repertory \"E\" and/or \"E_zip\" already exists in this path : ", path, " and in working directory. ",
            "You can delete those repertory if there is nothing more than the Environmental Index.")
  }
  
  ### If the E file doesn't exist anywhere
  if(!dir.exists(paste0(path, "E")))
  {
    zipurl <- "http://chave.ups-tlse.fr/pantropical_allometry/E.bil.zip"
    
    if(!file.exists(paste0(path, "E_zip")) | file.info(paste0(path, "E_zip"))$size < 31202482)
      DEMzip <- download.file(zipurl, destfile = paste0(path, "E_zip"))
    unzip(paste0(path, "E_zip"), exdir = paste0(path, "E"))
  }
  
  nam <- paste0(path, "E", sep, "E.bil")
  RAST <- raster::raster(nam)
  
  # Extract the raster value
  RASTval <- raster::extract(RAST, coord, "bilinear")
  return(RASTval)
}