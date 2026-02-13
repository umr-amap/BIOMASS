# Summarise by plot the posterior distribution of AGB values

This function summarises the matrix `AGB_val` given by the function
[`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
by plot.

## Usage

``` r
summaryByPlot(AGB_val, plot, drawPlot = FALSE)
```

## Arguments

- AGB_val:

  Either the matrix resulting from the
  [`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
  function (AGB_simu element of the list), or simply the output of the
  [`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
  function itself.

- plot:

  Vector corresponding to the plots code (plots ID)

- drawPlot:

  A logical indicating whether the graphic should be displayed or not

## Value

a data frame where:

- `plot`: the code of the plot

- `AGB`: AGB value at the plot level

- `Cred_2.5`: the 2.5\\

- `Cred_97.5`: the 97.5\\

## Details

If some trees belong to an unknown plot (i.e. NA value in the plot
arguments), their AGB values are randomly assigned to a plot at each
iteration of the AGB monte Carlo approach.

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
                                stand = NouraguesTrees$plotId)
#> Your taxonomic table contains 409 taxa
#> Warning: 142 taxa don't match the Global Wood Density Database V2. You may provide 'family' to match wood density estimates at family level.
# }

# Propagating errors
# \donttest{
  resultMC <- AGBmonteCarlo(
    D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
    errWD = NouraguesWD$sdWD, HDmodel = HDmodel )
  
  # The summary by plot
  summaryByPlot(AGB_val = resultMC$AGB_simu, plot = NouraguesTrees$Plot)
#>   plot      AGB Cred_2.5 Cred_97.5
#> 1  201 438.7902 396.9703  490.4543
#> 2  204 500.2492 455.4215  551.6338
#> 3  213 399.3600 358.4331  448.6508
#> 4  223 271.5251 249.0423  300.4295
# }
```
