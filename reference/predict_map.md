# Predict map of AGBD and associated uncertainty

As part of the FRM4BIOMASS project, this function enables to produce a
map of the AGBD and associated uncertainty, using a spatially varying
coefficient calibrated model created with the
[`calibrate_model()`](https://umr-amap.github.io/BIOMASS/reference/calibrate_model.md)
function.

## Usage

``` r
predict_map(
  fit_brms,
  pred_raster,
  grid_size,
  raster_fun = mean,
  n_cores = getOption("mc.cores", 1),
  n_post_draws = 50,
  alignment_raster = NULL,
  plot_maps = TRUE
)
```

## Arguments

- fit_brms:

  a brmsfit object, output of the
  [`calibrate_model()`](https://umr-amap.github.io/BIOMASS/reference/calibrate_model.md)
  function.

- pred_raster:

  filename (character) or a SpatRaster object from terra package: the
  raster to predict using fit_brms (typically a CHM raster created from
  LiDAR data)

- grid_size:

  a numeric indicating the dimension of grid cells. Must be identical to
  'grid_size' used in
  [`divide_plot()`](https://umr-amap.github.io/BIOMASS/reference/divide_plot.md)

- raster_fun:

  the function to apply to summarize the values of 'pred_raster'. Must
  be identical to 'raster_fun' used in
  [`subplot_summary()`](https://umr-amap.github.io/BIOMASS/reference/subplot_summary.md)

- n_cores:

  number of cores to use for predictions

- n_post_draws:

  positive integer indicating how many posterior draws should be used

- alignment_raster:

  filename (character) or a SpatRaster object from terra package: a
  raster whose coordinates will be used to align the coordinates of the
  predicted raster.

- plot_maps:

  A logical indicating whether the maps should be displayed (median, sd
  and CV of AGBD posterior distributions)

## Value

The data-table format of 'pred_raster', to which the following variables
have been added:

- post_median_AGBD: the median of the posterior distributions of the
  predicted AGBDs

- post_sd_AGBD: the sd of the posterior distributions of the predicted
  AGBDs

- post_cred_2.5_AGBD and post_cred_97.5_AGBD: the 2.5 and 97.5 quantiles
  of the posterior distributions of the predicted AGBDs

## Details

Speak about parallel computing ?

## Author

Arthur Bailly

## Examples

``` r
if (FALSE) { # \dontrun{

} # }
```
