# Angiosperm Phylogeny Group (APG III) dataset

APGIII Families taken from the Angiosperm Phylogeny Website
(http://www.mobot.org/MOBOT/research/APweb/)

## Usage

``` r
data("apgFamilies")
```

## Format

A data frame with 502 observations on the following 2 variables:

- `order`: Vector of order

- `famAPG`: Vector of APGIII families

## Source

Stevens, P. F. (2001 onwards). *Angiosperm Phylogeny Website*. Version
12, July 2012. Retrieved on 2016-07-25
http://www.mobot.org/MOBOT/research/APweb/

## Examples

``` r
data(apgFamilies)
str(apgFamilies)
#> 'data.frame':    502 obs. of  2 variables:
#>  $ order : chr  "Acorales" "Alismatales" "Alismatales" "Alismatales" ...
#>  $ famAPG: chr  "Acoraceae" "Alismataceae" "Aponogetonaceae" "Araceae" ...
```
