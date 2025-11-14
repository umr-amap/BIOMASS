# Function used to create or activate a permanent cache.

Permanent cache is located by default in user data dir.

## Usage

``` r
createCache(path = NULL)
```

## Arguments

- path:

  Use a custom path to host cache

## Value

No return value, called for side effects

## Details

You can provide a custom path (that will be defined as a BIOMASS.cache
option) but clearCache function will refuse to operate on it for
security reasons.
