# Feldpausch et al. 2012 coefficients for generalized height-diameter models

Weibull coefficients from a height-diameter model of the form \\H =
a(1-exp(-b\*D^c))\\ given by Feldpausch et al. 2012. in the table 3,
with the associated RSE.

## Usage

``` r
data("feldCoef")
```

## Format

A data frame with 12 observations on the following 4 variables:

- `a`: Coefficient a

- `b`: Coefficient b

- `c`: Coefficient c

- `RSE`: Vector of RSE

## Details

This dataset is used in the function
[`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
to predict height from diameter depending on the region.

## References

Feldpausch, T.R., et al. (2012). *Tree height integrated into
pantropical forest biomass estimates*. Biogeosciences, 9, 3381–3403.

## Examples

``` r
data(feldCoef)
str(feldCoef)
#> 'data.frame':    12 obs. of  4 variables:
#>  $ a  : num  50.1 50.5 44 53.1 42.6 ...
#>  $ b  : num  0.0371 0.0471 0.0334 0.0331 0.0482 ...
#>  $ c  : num  0.829 0.812 0.855 0.833 0.831 ...
#>  $ RSE: num  5.74 6.18 5.47 5.17 5.62 ...
```
