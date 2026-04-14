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
  intercept = FALSE,
  chains = 3,
  thin = 20,
  iter = 3000,
  warmup = 1000,
  cores = 3,
  ...
)
```

## Arguments

- long_AGB_simu:

  The '\$long_AGB_simu' output of the
  [`subplot_summary()`](https://umr-amap.github.io/BIOMASS/reference/subplot_summary.md)
  function (see Details).

- nb_rep:

  Number of simulation to provide in the brms fit (defaults to 30;
  nb_rep \> 50 will not improved significantly the model and will be
  much longer to fit).

- useCache:

  A logical that determines wether to use the cache when building a
  Bayesian model (see Details).

- plot_model:

  A logical indicating whether the model should be plotted (defaults to
  TRUE).

- intercept:

  A logical indicating whether the regression model should include an
  intercept (defaults to FALSE).

- chains:

  Number of Markov chains (defaults to 3), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- thin:

  Thinning rate (defaults to 20), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- iter:

  Number of total iterations per chain (including warmup; defaults to
  3000), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- warmup:

  Number of warmup (aka burnin) iterations (defaults to 1000), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- cores:

  Number of cores to use when executing the chains in parallel (defaults
  to 3), see
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

The model describing the relationship between plot-level AGBD and LiDAR
metrics is a log-log regression, with a Gaussian error model. To capture
the spatial structure that may exists in the data, we use a Spatially
Varying Coefficient (SVC) regression with Gaussian random fields
(Gelfand et al. 2003, Spatial modeling with spatially varying
coefficient processes).

The general equation can be written as follow, for a subplot \\s_i\\:

\\Y_i \sim \mathrm{N}(\mu_i, \sigma)\\

\\\mu_i = \beta_0 + (\beta_1 + \eta_i) \times X_i\\

\\\eta_i \sim \mathrm{MVNormal}(0, \Sigma)\\

\\X_i\\ stands for the logarithm of plot-level AGBD, \\Y_i\\ is the
logarithm of a LiDAR metric measurement for the corresponding plot.

\\\Sigma\\, the covariance matrix, is defined by the \\\frac{3}{2}\\
Matern kernel between two locations \\s_i\\ and \\s_j\\:
\\k(\mathbf{s}\_i, \mathbf{s}\_j) = \psi^2 \left( 1 +
\frac{\sqrt{3}d\_{i,j}}{l} \right) \exp \left(
-\frac{\sqrt{3}d\_{i,j}}{l} \right)\\ \\d\_{i,j}\\ is the distance
between locations \\s_i\\ and \\s_j\\, parameter \\\psi\\ controls the
magnitude and parameter \\l\\ the range of the kernel.

If useCache = TRUE and this is the first time the model is being built,
the model will be saved as a .rds file in the defined cache path (see
[`createCache()`](https://umr-amap.github.io/BIOMASS/reference/createCache.md)).
If useCache = TRUE and the model has already been built using the user
cache, the model will be loaded and updated to avoid wasting time
re-compiling it. If useCache = NULL, the cache is first cleared before
building the model.

## Author

Arthur BAILLY, Dominique LAMONICA
