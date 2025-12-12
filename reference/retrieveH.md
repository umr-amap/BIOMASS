# Retrieving tree height from models

From the diameter and either i) a model, ii) the coordinates of the plot
or iii) the region, this function gives an estimate of the total tree
height.

## Usage

``` r
retrieveH(D, model = NULL, coord = NULL, region = NULL, plot = NULL)
```

## Arguments

- D:

  Vector of diameters.

- model:

  A model output by the function
  [`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md).

- coord:

  Coordinates of the site(s), either a vector (e.g. c(longitude,
  latitude)) or a matrix/dataframe with two columns (e.g.
  cbind(longitude, latitude)).

- region:

  Area of your dataset to estimate tree height thanks to Weibull-H
  region-, continent-specific and pantropical models proposed by
  Feldpausch et al. (2012). To be chosen between:

  - `Africa`: Africa

  - `CAfrica`: Central Africa

  - `EAfrica`: Eastern Africa

  - `WAfrica`: Western Africa

  - `SAmerica`: Southern America

  - `BrazilianShield`: Brazilian Shield

  - `ECAmazonia`: East-Central Amazonia

  - `GuianaShield`: Guiana Shield

  - `WAmazonia`: Western Amazonia

  - `SEAsia`: South-Eastern Asia

  - `NAustralia`: Northern Australia

  - `Pantropical`: Pantropical

- plot:

  (optional) Plot ID, must be either one value, or a vector of the same
  length as D. This argument is used to build stand-specific HD models.

## Value

Returns a list with:

- `H`: Height predicted by the model

- `RSE` Residual Standard Error of the model, or a vector of those for
  each plot

## References

Feldpausch et al. *Tree height integrated into pantropical forest
biomass estimates*. Biogeosciences (2012): 3381-3403.

Chave et al. *Improved allometric models to estimate the aboveground
biomass of tropical trees*. Global change biology 20.10 (2014):
3177-3190.

## See also

[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)

## Author

Ariane TANGUY, Maxime REJOU-MECHAIN, Arthur PERE

## Examples

``` r
# Load a database
data(NouraguesHD)
model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2", bayesian = FALSE)

# If any height model is available
H <- retrieveH(D = NouraguesHD$D, model = model)

# If the only data available are the coordinates of your spot
n <- length(NouraguesHD$D)
coord <- cbind(long = rep(-52.68, n), lat = rep(4.08, n))
# \donttest{
H <- retrieveH(D = NouraguesHD$D, coord = coord)
# }

# If the only data available is the region of your spot
H <- retrieveH(D = NouraguesHD$D, region = "GuianaShield")
```
