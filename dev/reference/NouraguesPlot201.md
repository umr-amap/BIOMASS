# Nouragues plot 201 coordinates

Simulated corner coordinates of Nouragues 'Petit plateau' plot 201. The
original coordinates have been modified to make the plot non-squared,
and 10 repeated measurements of each corner have been simulated adding a
random error to x and y coordinates.

## Usage

``` r
data(NouraguesPlot201)
```

## Format

A data frame with 40 (simulated GPS measurements) of the 8 following
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
data(NouraguesPlot201)
str(NouraguesPlot201)
#> 'data.frame':    40 obs. of  8 variables:
#>  $ Site  : chr  "Petit_Plateau" "Petit_Plateau" "Petit_Plateau" "Petit_Plateau" ...
#>  $ Plot  : int  201 201 201 201 201 201 201 201 201 201 ...
#>  $ Xfield: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Yfield: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Xutm  : num  313003 313008 313009 313003 313006 ...
#>  $ Yutm  : num  451723 451721 451723 451726 451722 ...
#>  $ Long  : num  -52.7 -52.7 -52.7 -52.7 -52.7 ...
#>  $ Lat   : num  4.09 4.09 4.09 4.09 4.09 ...
```
