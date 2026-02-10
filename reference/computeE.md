# Retrieving Chave's environmental index

Extract the Chave et al. 2014's environmental index thanks to the
coordinates of the data. The function is time-consuming at its first use
as it downloads a raster in a folder (see Details). However, as soon as
the raster is downloaded once, the function then runs fast.

## Usage

``` r
computeE(coord)
```

## Arguments

- coord:

  Coordinates of the site(s), a matrix/dataframe with two columns (e.g.
  cbind(longitude, latitude)) (see examples).

## Value

The function returns `E`, the environmental index computed thanks to the
Chave et al 2014's formula as a single value or a vector.

## Details

The Chave's environmental index, `E`, has been shown to be an important
covariable in the diameter-height relationship for tropical trees. It is
calculated as: \$\$E = 1.e-3 \* (0.178 \* TS - 0.938 \* CWD - 6.61 \*
PS)\$\$ where `TS` is temperature seasonality as defined in the
Worldclim dataset (bioclimatic variable 4), `CWD` is the climatic water
deficit (in mm/yr, see Chave et al. 2014) and `PS` is the precipitation
seasonality as defined in the Worldclim dataset (bioclimatic variable
15).

The E index is extracted from a raster file (2.5 arc-second resolution,
or ca. 5 km) available at
http://chave.ups-tlse.fr/pantropical_allometry.htm

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

Chave et al. (2014) *Improved allometric models to estimate the
aboveground biomass of tropical trees*, Global Change Biology, 20 (10),
3177-3190

## Author

Jerome CHAVE, Maxime REJOU-MECHAIN, Ariane TANGUY, Arthur PERE

## Examples

``` r
# One study site
lat <- 4.08
long <- -52.68
coord <- cbind(long, lat)
# \donttest{
E <- computeE(coord)
# }

# Several study sites (here three sites)
long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)
# \donttest{
E <- computeE(coord)
# }
closeAllConnections()
```
