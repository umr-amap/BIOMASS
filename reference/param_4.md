# Posterior distribution of Chave et al.'s 2014 equation 4 parameters

This matrix contains the posterior distribution of the parameters of
Equation 4 of Chave et al. (2014), obtained in a Bayesian framework with
uninformative priors through a Metropolis algorithm.

## Usage

``` r
data("param_4")
```

## Format

A data frame with 1001 observations on the following 3 variables.

- `intercept`: Vector of intercept values

- `logagbt`: Vector of the model coefficients associated with the
  product wood density \* diameter^2 \* height

- `sd`: Vector of model residual standard error (RSE) values

## Details

This dataset is used in the function
[`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md).

## References

Chave et al. (2014) *Improved allometric models to estimate the
aboveground biomass of tropical trees*, Global Change Biology, 20 (10),
3177-3190

## Examples

``` r
data(param_4)
str(param_4)
#> 'data.frame':    1001 obs. of  3 variables:
#>  $ intercept: num  -2.72 -2.76 -2.75 -2.77 -2.75 ...
#>  $ logagbt  : num  0.971 0.975 0.975 0.976 0.973 ...
#>  $ sd       : num  0.355 0.357 0.355 0.362 0.358 ...
```
