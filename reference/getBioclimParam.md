# Retrieving bioclimatic parameters

This function extracts three bioclimatic parameters thanks to the
coordinates of the data: the Climatic Water Deficit (CWD), the
Temperature Seasonality (TS) and the Precipitation Seasonality (PS).

## Usage

``` r
getBioclimParam(coord)
```

## Arguments

- coord:

  Coordinates of the site(s), a matrix/dataframe with two columns (e.g.
  cbind(longitude, latitude)) (see examples).

## Value

The function returns a data.frame with `tempSeas` (temperature
seasonality, i.e. bioclimatic variable 4 from the Worldclim dataset;
Hijmans et al. 2005), `precSeas` (precipitation seasonality, i.e.
bioclimatic variable 15 from the Worldclim dataset; Hijmans et al. 2005)
and `CWD` (climatic water deficit; Chave et al. 2014).

## Details

The function is time-consuming at its first use as it downloads three
raster files (one for each of the parameter) which are then stored in
folders named wc2-5 and CWD (see Localisation).

However, as soon as the raster is downloaded once, the function then
runs fast.

## Localisation

Cache path discovery protocol

1.  BIOMASS.cache option set to an **existing** folder

2.  **existing** user data folder
    [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html)

    - On Linux : `~/.local/share/R/BIOMASS`

    - On Mac OS X : `~/Library/Application Support/R/BIOMASS`

    - On Windows 7 up to 10 :
      `C:\\Users\\<username>\\AppData\\Local\\R\\BIOMASS`

    - On Windows XP :
      `C:\\Documents and Settings\\<username>\\Data\\R\\BIOMASS`

3.  fallback to R session tempdir

## References

Hijmans et al. (2005) *Very high resolution interpolated climate
surfaces for global land areas*, International journal of climatology,
25(15), 1965-1978. Chave et al. (2014) *Improved allometric models to
estimate the above-ground biomass of tropical trees*, Global Change
Biology, 20 (10), 3177-3190

## Author

Ariane TANGUY, Arthur PERE

## Examples

``` r
# One study site
lat <- 4.08
long <- -52.68
coord <- cbind(long, lat)
# \donttest{
bioclim <- getBioclimParam(coord)
# }

# Several study sites (here three sites)
long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)
# \donttest{
bioclim <- getBioclimParam(coord)
#> There appears to be a problem reaching the directory.
#> Error in methods::as(x, "SpatRaster"): no method or default for coercing “NULL” to “SpatRaster”
# }
closeAllConnections()
```
