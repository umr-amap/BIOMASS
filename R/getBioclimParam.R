getBioclimParam <- function(coord)
{  
  ### Get the BioClim param from the http://www.worldclim.org website
  if(!dir.exists("wc2-5"))
  {
    bioData <- raster::getData('worldclim', var='bio', res=2.5)
    unzip("wc2-5/bio_2-5m_bil.zip", exdir = "wc2-5", files = c("bio4.bil", "bio15.bil"))
  }

  ### Get the CWD from the Jerome Chave's website
  if(!dir.exists("CWD"))
  {
    zipurl <- "http://chave.ups-tlse.fr/pantropical_allometry/CWD.bil.zip"
    
    if(!file.exists("CWD_zip"))
      DEMzip <- download.file(zipurl, destfile = "CWD_zip")
    unzip("CWD_zip", exdir = "CWD")
  }
  
  ### Load rasters
  tempSeas_rast <- raster::raster("wc2-5/bio4.bil")
  precSeas_rast <- raster::raster("wc2-5/bio15.bil")
  CWD_rast <- raster::raster("CWD/CWD.bil")
  
  ### Extract the raster value
  tempSeas <- raster::extract(tempSeas_rast, coord, "bilinear") * 10^-3
  precSeas <- raster::extract(precSeas_rast, coord, "bilinear") * 10^-3
  CWD <- raster::extract(CWD_rast, coord, "bilinear") * 10^-3
  
  out = data.frame(tempSeas = tempSeas, precSeas = precSeas, CWD = CWD)
  return(out)
}
