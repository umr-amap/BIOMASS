# The global wood density database

The global wood density database (Chave et al. 2009, Zanne et al. 2009).

## Usage

``` r
data("wdData")
```

## Format

A data frame with 16467 observations on the following 7 variables.

- `family`: a character vector indicating the family

- `genus`: a character vector indicating the genus

- `species`: a character vector indicating the species

- `wd`: a numeric vector of wood densities (g/cm^3)

- `region`: a character vector of regions (see
  [`getWoodDensity()`](https://umr-amap.github.io/BIOMASS/reference/getWoodDensity.md))

- `referenceNumber`: a numeric vector of reference numbers
  (bibliography)

- `regionId`: a character vector of region ids

## Source

Zanne et al. *Global wood density database.* Dryad. Identifier:
http://datadryad.org/handle/10255/dryad.235 (2009).

## Details

This dataset is used in the function
[`getWoodDensity()`](https://umr-amap.github.io/BIOMASS/reference/getWoodDensity.md),
to estimate a taxon-average wood density value.

## References

Chave et al. (2009) *Towards a worldwide wood economics spectrum.*
Ecology letters 12:4, 351-366.

## Examples

``` r
data(wdData)
#> Warning: data set ‘wdData’ not found
str(wdData)
#> Error: object 'wdData' not found
```
