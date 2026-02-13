# Calibrate a bayesian model to fit log(AGBD) ~ log(raster metric)

After applying the
[`subplot_summary()`](https://umr-amap.github.io/BIOMASS/reference/subplot_summary.md)
function, this function fits a log-log bayesian regression model with
spatially varying coefficient process, on AGBD and raster metric
simulated values (see Details).

## Usage

``` r
calibrate_model(
  long_AGB_simu,
  nb_rep = 30,
  useCache = FALSE,
  plot_model = TRUE,
  chains = 4,
  thin = 20,
  iter = 2300,
  warmup = 300,
  cores = 4,
  ...
)
```

## Arguments

- long_AGB_simu:

  The '\$long_AGB_simu' output of the
  [`subplot_summary()`](https://umr-amap.github.io/BIOMASS/reference/subplot_summary.md)
  function (see Details).

- nb_rep:

  Number of simulation to provide in the brms fit (nb_rep \> 50 will not
  improved significantly the model and will be much longer to fit).

- useCache:

  A logical that determines wether to use the cache when building a
  Bayesian model (see Details).

- plot_model:

  A logical indicating whether the model should be plot.

- chains:

  Number of Markov chains (defaults to 3), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- thin:

  Thinning rate, see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- iter:

  Number of total iterations per chain (including warmup; defaults to
  5000), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- warmup:

  Number of warmup (aka burnin) iterations (defaults to 1000), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- cores:

  Number of cores to use when executing the chains in parallel, see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- ...:

  Further arguments passed to `brm()`, e.g: prior, cores, etc. See
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

## Value

The function return a brmsfit object.

## Details

The 'long_AGB_simu' argument must be a data frame or data frame
extension containing the following variables:

- 'N_simu': a numeric indicating the simulation number.

- 'x_center' and 'y_center': the coordinates of the plots/subplots in
  the projected coordinate system.

- 'AGBD': the AGBD value of the simulation.

- 'raster_metric': the raster metric value of the simulation.

Speak about the model (fixed intercept), capturing the spatial
autocorrelation, and cite Gelfand et al. 2003 (Spatial modeling with
spatially varying coefficient processes)

If useCache = TRUE and this is the first time the model is being built,
the model will be saved as a .rds file in the defined cache path (see
[`createCache()`](https://umr-amap.github.io/BIOMASS/reference/createCache.md)).
If useCache = TRUE and the model has already been built using the user
cache, the model will be loaded and updated to avoid wasting time
re-compiling it. If useCache = NULL, the cache is first cleared before
building the model.

## Author

Arthur Bailly

## Examples

``` r
if (FALSE) { # \dontrun{

} # }
```
