# Function to clear cache content and possibly remove it

It will refuse to clear or remove a custom cache folder set using
BIOMASS.cache option as we don't know whether this folder contains other
possibly valuable files apart from our cached files.

## Usage

``` r
clearCache(remove = FALSE)
```

## Arguments

- remove:

  logical. If TRUE cache folder will be removed too (not only content)
  resulting in deactivating cache as a side effect

## Value

No return value, called for side effects
