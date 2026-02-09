# The global wood density database

The Global Wood Density Database v.2 (GWDD v.2) (Fischer, F. J., et al.
2026).

## Usage

``` r
data("wsg_estimates")
```

## Format

A data frame with 20332 observations on the following 6 variables.

- `family`: a character vector indicating the family

- `genus`: a character vector indicating the genus

- `species`: a character vector indicating the species

- `wsg`: a numeric vector of mean wood density of a population (g/cm^3)
  at taxonomic level

- `sd`: a numeric vector of standard deviation of wood density of a
  popuation (g/cm^3) at taxonomic level

- `level_tax`: a character vector of taxonomic level (namely `family` or
  `genus` or `species`)

## Source

Fischer, F. J., et al. (2026). Global Wood Density Database v.2 (GWDD
v.2) (Data set). Zenodo. https://doi.org/10.5281/zenodo.18262736

## Details

This dataset is used in the function
[`getWoodDensity()`](https://umr-amap.github.io/BIOMASS/reference/getWoodDensity.md),
to estimate a taxon-average wood density value.

## References

Fischer, F. J., et al. (2026). Beyond species means - the intraspecific
contribution to global wood density variation. New Phytol.
https://doi.org/10.1111/nph.70860

## Examples

``` r
data(wsg_estimates)
str(wsg_estimates)
#> 'data.frame':    20332 obs. of  6 variables:
#>  $ family   : chr  "Acanthaceae" "Acanthaceae" "Acanthaceae" "Acanthaceae" ...
#>  $ genus    : chr  NA "Acanthus" "Acanthus" "Aphelandra" ...
#>  $ species  : chr  NA NA "Acanthus montanus" NA ...
#>  $ wsg      : num  0.494 0.443 0.443 0.563 0.563 ...
#>  $ sd       : num  0.1565 0.0813 0.0813 0.093 0.093 ...
#>  $ level_tax: chr  "family" "genus" "species" "genus" ...
```
