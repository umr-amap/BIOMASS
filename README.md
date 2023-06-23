BIOMASS
================

  - [The package](#the-package)
  - [Citation](#citation)
  - [Install BIOMASS](#install-biomass)

## The package

R package for estimating aboveground biomass and its uncertainty in
tropical forests.

Contains functions to estimate aboveground biomass/carbon and its
uncertainty in tropical forests. These functions allow to:

1.  retrieve and correct the taxonomy;
2.  estimate the wood density and its uncertainty;
3.  construct height-diameter models;
4.  manage tree and plot coordinates;
5.  estimate the aboveground biomass/carbon at the stand level with
    associated uncertainty;

For more information, see
[article](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12753)
or the
[vignette](https://CRAN.R-project.org/package=BIOMASS/vignettes/BIOMASS.html)
of the package, and the [reference
manual](https://CRAN.R-project.org/package=BIOMASS/BIOMASS.pdf).

## Citation

To cite 'BIOMASS', please use citation(“BIOMASS”).

## Install BIOMASS

The latest released version from CRAN:

``` r
install.packages("BIOMASS")
```

The latest version from Github (in development):

``` r
install.packages("remotes")
remotes::install_github('umr-amap/BIOMASS')
```

To use it :

``` r
library("BIOMASS")
```
