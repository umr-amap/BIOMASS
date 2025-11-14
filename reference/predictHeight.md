# Tree height predictions

The function predicts height from diameter based on a fitted model. As
the predict() function for brms models takes ~10 minutes to run,
predictions are calculated using the coefficients from the models
directly.

## Usage

``` r
predictHeight(D, model, err = FALSE, plot = NULL)
```

## Arguments

- D:

  a n x m matrix containing tree diameters (in cm), where n is the
  number of trees and m is the number of Monte Carlo simulations (m = 1
  if no error propagation).

- model:

  The output of the
  [`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)
  function.

- err:

  If `TRUE`, An error is taken randomly from a normal distribution with
  a mean of zero and a standard deviation equaled to the residual
  standard error of the model (RSE). Only used for the Monte Carlo
  approach (see
  [`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)),
  otherwise it should be let as `FALSE`, the default case.

- plot:

  (optional) Plot ID, must be either one value, or a vector of the same
  length as D. This argument is used to build stand-specific HD models.

## Value

Returns a vector of total tree height (in m).

## Details

In the case where the error is `FALSE` and the model is a log-log model,
we use the Baskerville correction, a bias correction factor used to get
unbiased backtransformation values.

## See also

[`minpack.lm::nlsLM()`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)

## Author

Arthur BAILLY
