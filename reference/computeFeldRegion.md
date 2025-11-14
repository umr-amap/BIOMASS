# Retrieving Feldpausch regions

Extract the Feldpausch et al. (2012)'s regions using local coordinates.

## Usage

``` r
computeFeldRegion(coord, level = c("region"))
```

## Arguments

- coord:

  Coordinates of the site(s), a matrix/dataframe with two columns (e.g.
  cbind(longitude, latitude)) (see examples).

- level:

  a string or a vector of string, the length must match the number of
  rows of the parameter coord. This parameter gives the scale at which
  Feldpausch regions should be assigned. There are tree levels:

  - `region`: Models assign at sub-continent levels, value by default

  - `continent`: Models assign at the Africa, South America, Asia and
    Australia levels

  - `world`: Pantropical model

## Value

The function returns a vector with the Feldpausch et al. (2012)'s
regions that can be incorporated in the `retrieveH` function.

## References

Feldpausch, T.R., et al. (2012). *Tree height integrated into
pantropical forest biomass estimates.* Biogeosciences, 9, 3381–3403.

## Author

Arthur PERE

## Examples

``` r
#' # One study site
lat <- 4.08
long <- -52.68
coord <- cbind(long, lat)
# \donttest{
FeldRegion <- computeFeldRegion(coord)
# }

# Several study sites (here three sites)
long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)
# \donttest{
FeldRegion <- computeFeldRegion(coord)
#> Warning: There is NA in your final vector, those NA will be replaced by 'Pantropical'
# }
```
