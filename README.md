BIOMASS
================


## The package

The `BIOMASS` package allows users to estimate above ground biomass/carbon and its uncertainty in tropical forests. 

The main implemented steps are as follows :

1.  retrieving and correcting tree taxonomy;
2.  estimating the wood density and its uncertainty;
3.  constructing height-diameter models;
4.  estimating the above ground biomass/carbon at the stand level with the associated uncertainty;
5.  managing tree and plot coordinates.

For more information, see the [paper of Réjou-Méchain et al. (2017)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12753)

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

## Tutorials/Vignettes

Two vignettes are available in the 'Articles' section of the following page : [https://umr-amap.github.io/BIOMASS/index.html](https://umr-amap.github.io/BIOMASS/index.html)

## Citation

Please cite this package as:

*Réjou-Méchain M, Tanguy A, Piponiot C, Chave J, Herault B* (2017). “BIOMASS : an R package for estimating above-ground biomass and its uncertainty in tropical forests.” _Methods in Ecology and Evolution_, *8*(9). ISSN 2041210X, [doi:10.1111/2041-210X.12753](https://doi.org/10.1111/2041-210X.12753).

Or you can also run 

``` r
citation("BIOMASS")
```

