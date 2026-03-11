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
str(wdData)
#> 'data.frame':    16467 obs. of  7 variables:
#>  $ family         : chr  "Fabaceae" "Fabaceae" "Fabaceae" "Fabaceae" ...
#>  $ genus          : chr  "Abarema" "Abarema" "Abarema" "Abarema" ...
#>  $ species        : chr  "jupunba" "jupunba" "jupunba" "jupunba" ...
#>  $ wd             : num  0.78 0.66 0.551 0.534 0.551 0.5 0.52 0.438 0.353 0.4 ...
#>  $ region         : chr  "South_America_(tropical)" "South_America_(tropical)" "South_America_(tropical)" "South_America_(tropical)" ...
#>  $ referenceNumber: int  114 198 52 65 189 204 45 167 28 7 ...
#>  $ regionId       : chr  "SouthAmericaTrop" "SouthAmericaTrop" "SouthAmericaTrop" "SouthAmericaTrop" ...
#>  - attr(*, "na.action")= 'omit' Named int 12150
#>   ..- attr(*, "names")= chr "12150"
```
