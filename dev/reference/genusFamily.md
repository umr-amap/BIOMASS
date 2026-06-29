# Genus Family database

To create this database, we combined the genera from The Plant List
(http://www.theplantlist.org/1.1/browse/-/-/) and the Vascular Plant
Families and Genera from Kew
(http://data.kew.org/vpfg1992/genlist.html). Families were checked
against the APGIII families.

## Usage

``` r
data("genusFamily")
```

## Format

A data frame with 28107 observations on the following 2 variables:

- `family`: Vector of families APGIII corrected

- `genus`: Vector of genus

## Source

WCSP (2015). *World Checklist of Selected Plant Families*. Facilitated
by the Royal Botanic Gardens, Kew. Published on the Internet;
http://apps.kew.org/wcsp/ Retrieved 2015-12-17.

The Plant List (2013). Version 1.1. Published on the Internet;
http://www.theplantlist.org/ Retrieved 2016-08-25.

## Examples

``` r
data(genusFamily)
str(genusFamily)
#> 'data.frame':    31355 obs. of  2 variables:
#>  $ family: chr  "Orchidaceae" "Euphorbiaceae" "Compositae" "Thelypteridaceae" ...
#>  $ genus : chr  "Aa" "Aalius" "Aaronsohnia" "Abacopteris" ...
```
