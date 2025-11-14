# Nouragues plot coordinates

Dataset containing the corner coordinates of 4 plots of \`Petit Plateau'
in Nouragues forest (French Guiana).

## Usage

``` r
data(NouraguesCoords)
```

## Format

A data frame with 16 observations (GPS measurements) of the 8 following
variables :

- `Site`: Name of the site set up in the Nouragues forest

- `Plot`: Plot ID of the site

- `Xfield`: Corner location on the x-axis in the local coordinate system
  (defined by the 4 corners of the plot)

- `Yfield`: Corner location on the y-axis in the local coordinate system

- `Xutm`: Corner location on the x-axis in the UTM coordinate system

- `Yutm`: Corner location on the y-axis in the UTM coordinate system

- `Long`: Corner longitude coordinate

- `Lat`: Corner latitude coordinate

## References

Jaouen, Gaëlle, 2023, "Nouragues forest permanent plots details",
[doi:10.18167/DVN1/HXKS4E](https://doi.org/10.18167/DVN1/HXKS4E) , CIRAD
Dataverse, V2

## Examples

``` r
data(NouraguesCoords)
str(NouraguesCoords)
#> 'data.frame':    16 obs. of  8 variables:
#>  $ Site  : chr  "Petit_Plateau" "Petit_Plateau" "Petit_Plateau" "Petit_Plateau" ...
#>  $ Plot  : int  201 201 201 201 204 204 204 204 213 213 ...
#>  $ Xfield: int  0 0 100 100 0 0 100 100 100 100 ...
#>  $ Yfield: int  0 100 0 100 300 400 300 400 200 300 ...
#>  $ Xutm  : num  313008 313096 312960 313048 313272 ...
#>  $ Yutm  : num  451717 451669 451629 451582 451574 ...
#>  $ Long  : num  -52.7 -52.7 -52.7 -52.7 -52.7 ...
#>  $ Lat   : num  4.08 4.08 4.08 4.08 4.08 ...
```
