# Propagating above ground biomass (AGB) or carbon (AGC) errors to the stand level

Propagation of the errors throughout the steps needed to compute AGB or
AGC.

## Usage

``` r
AGBmonteCarlo(
  D,
  WD = NULL,
  errWD = NULL,
  H = NULL,
  errH = NULL,
  HDmodel = NULL,
  coord = NULL,
  Dpropag = NULL,
  n = 1000,
  Carbon = FALSE,
  Dlim = NULL
)
```

## Arguments

- D:

  Vector of tree diameters (in cm)

- WD:

  Vector of wood density estimates (in g/cm3)

- errWD:

  Vector of error associated to the wood density estimates (should be of
  the same size as `WD`)

- H:

  (option 1) Vector of tree heights (in m). If set, `errH` must be set
  too.

- errH:

  (if `H`) Residual standard error (RSE) of a model or vector of errors
  (sd values) associated to tree height values (in the latter case the
  vector should be of the same length as `H`).

- HDmodel:

  (option 2) Model used to estimate tree height from tree diameter
  (output from
  [`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md),
  see example).

- coord:

  (option 3) Coordinates of the site(s), either a vector giving a single
  site (e.g. c(longitude, latitude)) or a matrix/dataframe with two
  columns (e.g. cbind(longitude, latitude)). The coordinates are used to
  predict height-diameter allometry with bioclimatic variables.

- Dpropag:

  This variable can take three kind of values, indicating how to
  propagate the errors on diameter measurements: a single numerical
  value or a vector of the same size as `D`, both representing the
  standard deviation associated with the diameter measurements or
  `"chave2004"` (an important error on 5 percent of the measures, a
  smaller error on 95 percent of the trees).

- n:

  Number of iterations. Cannot be smaller than 50 or larger than 1000.
  By default `n = 1000`

- Carbon:

  (logical) Whether or not the propagation should be done up to the
  carbon value (FALSE by default).

- Dlim:

  (optional) Minimum diameter (in cm) for which above ground biomass
  should be calculated (all diameter below `Dlim` will have a 0 value in
  the output).

## Value

Returns a list with (if Carbon is FALSE):

- `meanAGB`: Mean stand AGB value following the error propagation

- `medAGB`: Median stand AGB value following the error propagation

- `sdAGB`: Standard deviation of the stand AGB value following the error
  propagation

- `credibilityAGB`: Credibility interval at 95\\

- `AGB_simu`: Matrix with the AGB of the trees (rows) times the n
  iterations (columns)

## Details

See Rejou-Mechain et al. (2017) for all details on the error propagation
procedure.

## References

Chave, J. et al. (2004). *Error propagation and scaling for tropical
forest biomass estimates*. Philosophical Transactions of the Royal
Society B: Biological Sciences, 359(1443), 409-420.

Rejou-Mechain et al. (2017). *BIOMASS: An R Package for estimating
above-ground biomass and its uncertainty in tropical forests*. Methods
in Ecology and Evolution, 8 (9), 1163-1167.

## Author

Maxime REJOU-MECHAIN, Bruno HERAULT, Camille PIPONIOT, Ariane TANGUY,
Arthur PERE

## Examples

``` r
# Load a database
data(NouraguesHD)
data(NouraguesTrees)

# Modelling height-diameter relationship
HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2", bayesian = FALSE)

# Retrieving wood density values
# \donttest{
NouraguesWD <- getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species,
  stand = NouraguesTrees$Plot
)
#> Your taxonomic table contains 409 taxa
#> Warning: 142 taxa don't match the Global Wood Density Database V2. You may provide 'family' to match wood density estimates at family level.
# }

# Propagating errors with a standard error for Wood density
# \donttest{
resultMC <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
  errWD = NouraguesWD$sdWD, HDmodel = HDmodel
)
# }

# If only the coordinates are available
coord <- c(-52.683213,4.083024 )
# \donttest{
resultMC <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
  errWD = NouraguesWD$sdWD, coord = coord
)
# }

# Propagating errors with a standard error in wood density in all plots at once
# \donttest{
NouraguesTrees$meanWD <- NouraguesWD$meanWD
NouraguesTrees$sdWD <- NouraguesWD$sdWD
resultMC <- by(
  NouraguesTrees, NouraguesTrees$Plot,
  function(x) AGBmonteCarlo(
      D = x$D, WD = x$meanWD, errWD = x$sdWD,
      HDmodel = HDmodel, Dpropag = "chave2004"
    )
)
meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
credperplot <- sapply(resultMC, "[", 4)
# }
closeAllConnections()
```
