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

  A logic indicating whether the graphic should be displayed or not

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
HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")

# Retrieving wood density values
# \donttest{
  NouraguesWD <- getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species,
                                stand = NouraguesTrees$plotId)
#> The reference dataset contains 16467 wood density values
#> Your taxonomic table contains 409 taxa
# }

# Propagating errors
# \donttest{
  resultMC <- AGBmonteCarlo(
    D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
    errWD = NouraguesWD$sdWD, HDmodel = HDmodel )
  
  # The summary by plot
  summaryByPlot(AGB_val = resultMC$AGB_simu, plot = NouraguesTrees$Plot)
#>   plot      AGB Cred_2.5 Cred_97.5
#> 1  201 456.9527 414.5240  510.7609
#> 2  204 511.7294 464.0859  563.9798
#> 3  213 372.5745 334.5266  417.6881
#> 4  223 291.6578 265.0000  323.9551
# }
```
