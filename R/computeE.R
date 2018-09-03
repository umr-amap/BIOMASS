computeE <- function(coord)
{  
  ### Compute the Environmental Index (Chave et al. 2014)
    
  if(!dir.exists("E"))
  {
    zipurl <- "http://chave.ups-tlse.fr/pantropical_allometry/E.bil.zip"
    
    if(!file.exists("E_zip") | file.info("E_zip")$size < 31202482)
      DEMzip <- download.file(zipurl, destfile = "E_zip")
    unzip("E_zip", exdir = "E")
  }
  
  nam <- "E/E.bil"
  RAST <- raster::raster(nam)
  
  # Extract the raster value
  RASTval <- raster::extract(RAST, coord, "bilinear")
  return(RASTval)
}
