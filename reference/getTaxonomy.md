# Retrieve trees taxonomy

**\[deprecated\]** `getTaxonomy()` is now deprecated,
[`correctTaxo()`](https://umr-amap.github.io/BIOMASS/reference/correctTaxo.md)
being able to handle retrieving taxonomy all at once.

From given genus, the function finds the APG III family, and optionally
the order, from the
[genusFamily](https://umr-amap.github.io/BIOMASS/reference/genusFamily.md)
database and the
[apgFamilies](https://umr-amap.github.io/BIOMASS/reference/apgFamilies.md)
dataset

## Usage

``` r
getTaxonomy(genus, findOrder = FALSE)
```

## Arguments

- genus:

  Vector of genus names

- findOrder:

  (Boolean) If `TRUE`, the output will contain the taxonomical orders of
  the families.

## Value

Data frame with the order (if `findOrder` is `TRUE`), family and genus.

## Author

Ariane TANGUY, Arthur PERE, Maxime REJOU-MECHAIN

## Examples

``` r
# Find the Family of the Aphelandra genus
getTaxonomy("Aphelandra")
#> Warning: `getTaxonomy()` was deprecated in BIOMASS 2.2.8.
#> ℹ Please use `correctTaxo()` instead.
#>   inputGenus      family
#> 1 Aphelandra Acanthaceae
# ... and the order
# \donttest{
getTaxonomy("Aphelandra", findOrder = TRUE)
#>   inputGenus      family    order
#> 1 Aphelandra Acanthaceae Lamiales
# }
```
