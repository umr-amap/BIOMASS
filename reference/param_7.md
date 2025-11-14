# Posterior distribution of parameters associated with the equation 7 by Chave et al. 2014.

This matrix contains the posterior distribution of the parameters of the
Equation 7 of Chave et al., (2014), obtained in a Bayesian framework
with uninformative priors through a Metropolis algorithm.

## Usage

``` r
data("param_7")
```

## Format

A data frame with 1001 observations on the following 9 variables.

- `intercept`: Vector of intercept values

- `logwsg`: Vector of the model coefficients associated with log(wood
  density)

- `logdbh`: Vector of the model coefficients associated with
  log(diameter)

- `logdbh2`: Vector of the model coefficients associated with
  log(diameter)^2

- `E`: Vector of the model coefficients associated with the
  environmental index E

- `sd`: Vector of model residual standard error (RSE) values

- `temp`: Vector of the model coefficients associated with temperature
  seasonality

- `cwd`: Vector of the model coefficients associated with climatic water
  deficit

- `prec`: Vector of the model coefficients associated with precipitation
  seasonality

## Details

This dataset is used in the function
[`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md).

## References

Chave et al. (2014) *Improved allometric models to estimate the
aboveground biomass of tropical trees*, Global Change Biology, 20 (10),
3177-3190

## Examples

``` r
data(param_7)
str(param_7)
#> 'data.frame':    1001 obs. of  9 variables:
#>  $ intercept: num  -2.08 -1.94 -1.98 -2 -2.08 ...
#>  $ logwsg   : num  0.919 0.898 0.941 0.904 0.914 ...
#>  $ logdbh   : num  2.75 2.67 2.69 2.69 2.77 ...
#>  $ logdbh2  : num  -0.0358 -0.0275 -0.0314 -0.0245 -0.0403 ...
#>  $ E        : num  -0.892 -0.838 -0.856 -0.878 -0.88 ...
#>  $ sd       : num  0.407 0.415 0.406 0.417 0.429 ...
#>  $ temp     : num  -0.18 -0.185 -0.184 -0.17 -0.185 ...
#>  $ cwd      : num  0.947 0.921 0.917 0.965 0.877 ...
#>  $ prec     : num  6.88 6.4 6.46 6.65 6.29 ...
```
