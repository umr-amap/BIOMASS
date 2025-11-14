# Translate the long lat coordinate in UTM coordinate

Translate the long lat coordinate in UTM coordinate

## Usage

``` r
latlong2UTM(coord)
```

## Arguments

- coord:

  Coordinates of the site(s), a matrix/dataframe with two columns (e.g.
  cbind(longitude, latitude)) (see examples).

## Value

a data frame with :

- `long`: The longitude of the entry

- `lat`: The latitude of the entry

- `codeUTM`: The code `proj` for UTM

- `X`: The X UTM coordinate

- `Y`: The Y UTM coordinate

## Examples

``` r
long <- c(-52.68, -51.12, -53.11)
lat <- c(4.08, 3.98, 4.12)
coord <- cbind(long, lat)
# \donttest{
UTMcoord <- latlong2UTM(coord)
#> Loading required namespace: proj4
# }
```
