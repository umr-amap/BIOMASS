# HDmethods

Methods used for modeling height-diameter relationship

## Usage

``` r
loglogFunction(
  data,
  weight = NULL,
  method,
  bayesian,
  useCache,
  chains,
  thin,
  iter,
  warmup,
  ...
)

michaelisFunction(
  data,
  weight = NULL,
  bayesian,
  useCache,
  chains,
  thin,
  iter,
  warmup,
  ...
)

weibullFunction(
  data,
  weight = NULL,
  bayesian,
  useCache,
  chains,
  thin,
  iter,
  warmup,
  ...
)
```

## Arguments

- data:

  Dataset with the informations of height (H) and diameter (D)

- weight:

  (optional) Vector indicating observation weights in the model.

- method:

  In the case of the loglogFunction, the model is to be chosen between
  log1, log2 or log3.

- bayesian:

  a logical. If FALSE (by default) the model is estimated using a
  frequentist framework (lm or nls). If TRUE, the model is estimated in
  a Bayesian framework using the brms package.

- useCache:

  a logical. If bayesian = TRUE, determine wether to use the cache when
  building a Bayesian model (see Details).

- chains:

  (only relevant if bayesian = TRUE): Number of Markov chains (defaults
  to 3), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- thin:

  (only relevant if bayesian = TRUE): Thinning rate, see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- iter:

  (only relevant if bayesian = TRUE): number of total iterations per
  chain (including warmup; defaults to 5000), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- warmup:

  (only relevant if bayesian = TRUE): number of warmup (aka burnin)
  iterations (defaults to 1000), see
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

- ...:

  Further arguments passed to `brm()`, e.g: prior, cores, etc. See
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)

## Value

All the functions give an output similar to the one given by
[`stats::lm()`](https://rdrr.io/r/stats/lm.html), obtained for
`michaelisFunction` and `weibullFunction` from
[minpack.lm::nlsLM](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)).

Result of a model (lm object if bayesian = FALSE, brm object if bayesian
= TRUE)

Result of a model (nlsM object if bayesian = FALSE, brm object if
bayesian = TRUE)

Result of a model (nlsM object if bayesian = FALSE, brm object if
bayesian = TRUE)

## Details

These functions model the relationship between tree height (H) and
diameter (D). **loglogFunction** Compute two types of log model (log and
log2) to predict H from D. The model can be:

- log 1: \\log(H) = a+ b\*log(D)\\ (equivalent to a power model)

- log 2: \\log(H) = a+ b\*log(D) + c\*log(D)^2\\

**michaelisFunction** Construct a Michaelis Menten model of the form:
\$\$H = (A \* D) / (B + D)\$\$ (A and B are the model parameters to be
estimated)

**weibullFunction** Construct a three parameter Weibull model of the
form: \$\$H = a\*(1-exp(-(D/b)^c))\$\$ (a, b, c are the model parameters
to be estimated)

## References

Michaelis, L., & Menten, M. L. (1913). *Die kinetik der
invertinwirkung*. Biochem. z, 49(333-369), 352. Weibull, W. (1951).
*Wide applicability*. Journal of applied mechanics, 103. Baskerville, G.
L. (1972). *Use of logarithmic regression in the estimation of plant
biomass*. Canadian Journal of Forest Research, 2(1), 49-53.

## See also

[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)

## Author

Maxime REJOU-MECHAIN, Ariane TANGUY
