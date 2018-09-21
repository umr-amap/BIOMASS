getBioclimParam <- function(coord)
{  
  
  sep = ifelse(length(grep( "win", Sys.info()["sysname"], ignore.case = T )) != 0, "\\", "/")
  path = paste0( rappdirs::user_data_dir("BIOMASS"), sep )
  
  if( !dir.exists( rappdirs::user_data_dir("BIOMASS")) ){
    dir.create(rappdirs::user_data_dir("BIOMASS"))
  }
  
  ### If the file needed are in the working directory
  if (dir.exists("wc2-5") & !dir.exists(paste0(path, "wc2-5"))){
    file.rename("wc2-5", paste0(path, "wc2-5"))
    message("Your repertory \"wc2-5\" has been moved in this repertory : ", path)
  }
  
  if (dir.exists("wc2-5") & dir.exists(paste0(path, "wc2-5"))){
    message("Your repertory \"wc2-5\" already exists in this path : ", path, " and in working directory. ",
            "You can delete those repertory if there is nothing more than the World Climate.")
  }
  
  ### Get the BioClim param from the http://www.worldclim.org website
  if(!dir.exists(paste0(path, "wc2-5")))
  {
    bioData <- raster::getData('worldclim', var='bio', res=2.5, path = path)
    unzip(paste0(path, "wc2-5", sep, "bio_2-5m_bil.zip"), exdir = paste0(path, "wc2-5"), files = c("bio4.bil", "bio15.bil"))
  }
  

  
  
  ### If the file needed are in the working directory
  if (dir.exists("CWD") & !dir.exists(paste0(path, "CWD"))){
    file.rename("CWD", paste0(path, "CWD"))
    message("Your repertory \"CWD\" has been moved in this repertory : ", path)
  }
  
  if (file.exists("CWD_zip") & !file.exists(paste0(path, "CWD_zip"))){
    file.rename("CWD_zip", paste0(path, "CWD_zip"))
    message("Your repertory \"CWD_zip\" has been moved in this repertory : ", path)
  }
  
  if (dir.exists("CWD") & dir.exists(paste0(path, "CWD"))){
    message("Your repertory \"CWD\" and/or \"CWD_zip\" already exists in this path : ", path, " and in working directory. ",
            "You can delete those repertory if there is nothing more than the Climatological Water Deficit.")
  }
  
  
  ### Get the CWD from the Jerome Chave's website
  if(!dir.exists(paste0(path, "CWD")))
  {
    zipurl <- "http://chave.ups-tlse.fr/pantropical_allometry/CWD.bil.zip"
    
    if(!file.exists(paste0(path, "CWD")))
      DEMzip <- download.file(zipurl, destfile = paste0(path, "CWD_zip"))
    unzip(paste0(path, "CWD_zip"), exdir = paste0(path, "CWD"))
  }
  
  ### Load rasters
  tempSeas_rast <- raster::raster(paste0(path, "wc2-5", sep, "bio4.bil"))
  precSeas_rast <- raster::raster(paste0(path, "wc2-5", sep, "bio15.bil"))
  CWD_rast <- raster::raster(paste0(path, "CWD", sep, "CWD.bil"))
  
  ### Extract the raster value
  tempSeas <- raster::extract(tempSeas_rast, coord, "bilinear") * 10^-3
  precSeas <- raster::extract(precSeas_rast, coord, "bilinear") * 10^-3
  CWD <- raster::extract(CWD_rast, coord, "bilinear") * 10^-3
  
  out = data.frame(tempSeas = tempSeas, precSeas = precSeas, CWD = CWD)
  return(out)
}
