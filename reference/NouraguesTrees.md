# Nouragues forest dataset

This dataset contains 4 of the 12 plots of \`Petit Plateau' permanent
plots fifth census, 2012, Nouragues forestTree dataset (French Guiana).
For educational purposes, some virtual trees have been added in the
dataset. Dead trees have been removed.

## Usage

``` r
data(NouraguesTrees)
```

## Format

A data frame with 2050 observations (trees) of the 8 following variables
:

- `site`: Name of the site set up in the Nouragues forest

- `plot`: Plot ID

- `Xfield`: Tree location on the x-axis in the local coordinate system
  (defined by the 4 corners of the plot)

- `Yfield`: Tree location on the y-axis in the local coordinate system

- `family`: Tree family

- `genus`: Tree genus

- `species`: Tree species

- `D`: Tree diameter (in cm)

## References

\`Petit Plateau' permanent plots fifth census, 2012, Nouragues forest,
[https://doi.org/10.18167/DVN1/TZ1RL9](https://dataverse.cirad.fr/dataset.xhtml?persistentId=doi:10.18167/DVN1/TZ1RL9),
CIRAD Dataverse, V1

## Examples

``` r
data(NouraguesTrees)
str(NouraguesTrees)
#> 'data.frame':    2050 obs. of  8 variables:
#>  $ Site   : chr  "Petit_Plateau" "Petit_Plateau" "Petit_Plateau" "Petit_Plateau" ...
#>  $ Plot   : int  201 201 201 201 201 201 201 201 201 201 ...
#>  $ Xfield : num  0 0.1 0.2 -4 0.3 ...
#>  $ Yfield : num  31.5 75.2 27.6 67.5 39.9 ...
#>  $ Family : chr  "Burseraceae" "Anacardiaceae" "Lecythidaceae" "Euphorbiaceae" ...
#>  $ Genus  : chr  "Protium" "Tapirira" "Indet.Lecythidaceae" "Conceveiba" ...
#>  $ Species: chr  "surinamense" "guianensis" "Indet." "guyanensis" ...
#>  $ D      : num  11 74.4 25.4 10 18.9 10 15.9 10.1 35.9 57.4 ...
```
