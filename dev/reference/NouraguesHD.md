# Height-Diameter data

Dataset from two 1-ha plots from the Nouragues forest (French Guiana)

## Usage

``` r
data("NouraguesHD")
```

## Format

A data frame with 1051 observations on the following variables :

- `plotId`: Names of the plots

- `genus`: Genus

- `species`: Species

- `D`: Diameter (cm)

- `H`: Height (m)

- `lat`: Latitude

- `long`: Longitude

## References

Réjou-Méchain, M. et al. (2015). *Using repeated small-footprint LiDAR
acquisitions to infer spatial and temporal variations of a high-biomass
Neotropical forest* Remote Sensing of Environment, 169, 93-101.

## Examples

``` r
data(NouraguesHD)
str(NouraguesHD)
#> 'data.frame':    1051 obs. of  7 variables:
#>  $ plotId : chr  "Plot1" "Plot1" "Plot1" "Plot1" ...
#>  $ genus  : chr  "indet" "Qualea" "Dicorynia" "Protium" ...
#>  $ species: chr  "indet" "rosea" "guianensis" "cf_guianense" ...
#>  $ D      : num  11.5 11.6 83.9 15 36.8 13.5 17.8 17.8 15.9 17.8 ...
#>  $ H      : num  12 16 40 18 27 20 24 21 22 24 ...
#>  $ lat    : num  4.07 4.07 4.07 4.07 4.07 ...
#>  $ long   : num  -52.7 -52.7 -52.7 -52.7 -52.7 ...
```
