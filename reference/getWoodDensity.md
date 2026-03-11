# Estimating wood density

The function estimates the wood density (WD) of the trees from their
taxonomy or from their congeners using the global wood density database
(Chave et al. 2009, Zanne et al. 2009) or any additional dataset. The WD
can either be attributed to an individual at a species, genus, family or
stand level.

## Usage

``` r
getWoodDensity(
  genus,
  species,
  stand = NULL,
  family = NULL,
  region = "World",
  addWoodDensityData = NULL,
  verbose = TRUE
)
```

## Arguments

- genus:

  Vector of genus names

- species:

  Vector of species names

- stand:

  (optional) Vector with the corresponding stands of your data. If set,
  the missing wood densities at the genus level will be attributed at
  stand level. If not, the value attributed will be the mean of the
  whole tree dataset.

- family:

  (optional) Vector of families. If set, the missing wood densities at
  the genus level will be attributed at family level if available.

- region:

  Region (or vector of region) of interest of your sample. By default,
  Region is set to 'World', but you can restrict the WD estimates to a
  single region :

  - `AfricaExtraTrop`: Africa (extra tropical)

  - `AfricaTrop`: Africa (tropical)

  - `Australia`: Australia

  - `AustraliaTrop`: Australia (tropical)

  - `CentralAmericaTrop`: Central America (tropical)

  - `China`: China

  - `Europe`: Europe

  - `India`: India

  - `Madagascar`: Madagascar

  - `Mexico`: Mexico

  - `NorthAmerica`: North America

  - `Oceania`: Oceania

  - `SouthEastAsia`: South-East Asia

  - `SouthEastAsiaTrop`: South-East Asia (tropical)

  - `SouthAmericaExtraTrop`: South America (extra tropical)

  - `SouthAmericaTrop`: South America (tropical)

  - `World`: World

- addWoodDensityData:

  A dataframe containing additional wood density data to be combined
  with the global wood density database. The dataframe should be
  organized in a dataframe with three (or four) columns:
  "genus","species","wd", the fourth column "family" is optional.

- verbose:

  A logical, give some statistic with the database

## Value

Returns a dataframe containing the following information:

- `family`: (if set) Family

- `genus`: Genus

- `species`: Species

- `meanWD` (g/cm^3): Mean wood density

- `sdWD` (g/cm^3): Standard deviation of the wood density that can be
  used in error propagation (see
  [sd_10](https://umr-amap.github.io/BIOMASS/reference/sd_10.md) and
  [`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md))

- `levelWD`: Level at which wood density has been calculated. Can be
  species, genus, family, dataset (mean of the entire dataset) or, if
  stand is set, the name of the stand (mean of the current stand)

- `nInd`: Number of individuals taken into account to compute the mean
  wood density

## Details

The function assigns to each taxon a species- or genus- level average if
at least one wood density value at the genus level is available for that
taxon in the reference database. If not, the mean wood density of the
family (if set) or of the stand (if set) is given.

The function also provides an estimate of the error associated with the
wood density estimate (i.e. a standard deviation): a mean standard
deviation value is given to the tree at the appropriate taxonomic level
using the [sd_10](https://umr-amap.github.io/BIOMASS/reference/sd_10.md)
dataset.

## References

Chave, J., et al. *Towards a worldwide wood economics spectrum*. Ecology
letters 12.4 (2009): 351-366. Zanne, A. E., et al. *Global wood density
database*. Dryad. Identifier: http://hdl. handle. net/10255/dryad 235
(2009).

## See also

[wdData](https://umr-amap.github.io/BIOMASS/reference/wdData.md),
[sd_10](https://umr-amap.github.io/BIOMASS/reference/sd_10.md)

## Author

Maxime REJOU-MECHAIN, Arthur PERE, Ariane TANGUY

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
#> The reference dataset contains 16467 wood density values
#> Your taxonomic table contains 409 taxa
# }

# Compute the Wood Density up to the genus level and then give the mean wood density per stand
# \donttest{
WD <- getWoodDensity(
  genus = NouraguesTrees$Genus,
  species = NouraguesTrees$Species,
  stand = NouraguesTrees$plotId
)
#> The reference dataset contains 16467 wood density values
#> Your taxonomic table contains 409 taxa
# }

# Compute the Wood Density up to the family level and then give the mean wood density per stand
# \donttest{
WD <- getWoodDensity(
  family = NouraguesTrees$family,
  genus = NouraguesTrees$Genus,
  species = NouraguesTrees$Species,
  stand = NouraguesTrees$plotId
)
#> The reference dataset contains 16467 wood density values
#> Your taxonomic table contains 409 taxa
str(WD)
#> 'data.frame':    2050 obs. of  7 variables:
#>  $ family : chr  "Burseraceae" "Anacardiaceae" NA "Euphorbiaceae" ...
#>  $ genus  : chr  "Protium" "Tapirira" "Indet.Lecythidaceae" "Conceveiba" ...
#>  $ species: chr  "surinamense" "guianensis" "Indet." "guyanensis" ...
#>  $ meanWD : num  0.568 0.457 0.675 0.413 0.568 ...
#>  $ sdWD   : num  0.0941 0.0708 0.1565 0.0941 0.0941 ...
#>  $ levelWD: chr  "genus" "species" "dataset" "genus" ...
#>  $ nInd   : int  31 8 1877 3 31 2 1877 2 3 1877 ...
# }
```
