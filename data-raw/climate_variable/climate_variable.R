require(raster)

##### wc2-5
# get the data from world climate
getData("worldclim", var = "bio", res = 2.5, path = "data-raw/climate_variable")

paste0("data-raw/climate_variable/wc2-5/", c("bio4\\.*", "bio15\\.*"))

files = unlist(sapply(c("bio4", "bio15"), function(x) dir("data-raw/climate_variable/wc2-5", x, full.names = T), 
              simplify = F), 
       use.names = F)

zip("data-raw/climate_variable/wc2-5.zip", files = files, flags = "-rj9X")


###### Not working now because the server is down
### get the data CWD
download.file("http://chave.ups-tlse.fr/pantropical_allometry/CWD.bil.zip", destfile = "data-raw/climate_variable/CWD.zip")

###### Not working now because the server is down
### get the data E
download.file("http://chave.ups-tlse.fr/pantropical_allometry/E.bil.zip", destfile = "data-raw/climate_variable/E.zip")