# Estimating wood density and associated uncertainties

The function estimates the wood density (WD) and the associated sd of
the trees from their taxonomy or from their congeners using the global
wood density database V2 (Fischer et al. 2025) or any additional dataset
if the sd is also provided. The WD can either be attributed to an
individual at a species, genus, family or stand level.

## Usage

``` r
getWoodDensity(
  genus,
  species,
  family = NULL,
  stand = NULL,
  addWoodDensityData = NULL,
  verbose = TRUE
)
```

## Arguments

- genus:

  Vector of genus names.

- species:

  Vector of species names.

- family:

  (optional) Vector of families. If set, the missing wood densities at
  the genus level will be attributed at family level if available.

- stand:

  (optional) Vector with the corresponding stands of your data. If set,
  the missing wood densities at the genus level will be attributed at
  stand level. If not, the value attributed will be the mean of the
  whole tree dataset.

- addWoodDensityData:

  A dataframe containing additional wood density data to be combined
  with the global wood density database (see Details).

- verbose:

  A logical, give some statistic with the database

## Value

Returns a dataframe containing the following information:

- `family`: Family

- `genus`: Genus

- `species`: Species

- `meanWD` (g/cm^3): Mean wood density estimates

- `sdWD` (g/cm^3): Standard deviation estimates of the wood density

- `levelWD`: Level at which wood density has been calculated. Can be
  species, genus, family, dataset (mean of the entire dataset) or, if
  stand is set, the name of the stand (mean of the current stand)

## Details

The function assigns wood density estimates (WD) and uncertainty (sigma)
at species, genus or family level to each taxon, using the results of
Bayesian hierarchical modelling on the Global Wood Density Database V2,
with the following brms formula: WD ~ 1 + (1 \| family / genus /
species) + (1 \| source_short) sigma ~ 1 + ind + (1 \| species) The
uncertainties related to the genus and family are then estimated by
simulating WD values for all the species.

If a taxon is unidentified or absent from the database, the estimated WD
and uncertainty of the stand (if set) is given.

When supplying addWoodDensityData, the dataframe should be organized as
follow:

- four (or five) columns: "genus","species","meanWD","sdWD" (the fifth
  column "family" is optional)

- one row per species (not per individual measurement) The taxa present
  in addWoodDensityData will replace the GWDD V2 estimates.

## References

Fischer, F. J., et al. 2025 A global map of wood density
https://doi.org/10.1101/2025.08.25.671920 Fischer, F. J., et al. 2025
Beyond species means - the intraspecific contribution to global wood
density variation https://doi.org/10.1101/2025.08.25.671896

## See also

wsg_estimates

## Author

Arthur BAILLY, Maxime REJOU-MECHAIN, Fabian FISCHER

## Examples

``` r
# Load a data set
data(NouraguesTrees)

# Compute the Wood Density up to the genus level and give the mean wood density of the dataset
# \donttest{
WD <- getWoodDensity(
  genus = NouraguesTrees$Genus,
  species = NouraguesTrees$Species
)
#> Your taxonomic table contains 409 taxa
#> Warning: 142 taxa don't match the Global Wood Density Database V2. You may provide 'family' to match wood density estimates at family level.
# }

# Compute the Wood Density up to the genus level and then give the mean wood density per stand
# \donttest{
WD <- getWoodDensity(
  genus = NouraguesTrees$Genus,
  species = NouraguesTrees$Species,
  stand = NouraguesTrees$plotId
)
#> Your taxonomic table contains 409 taxa
#> Warning: 142 taxa don't match the Global Wood Density Database V2. You may provide 'family' to match wood density estimates at family level.
# }
```
