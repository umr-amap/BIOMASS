# Correct and match taxonomic names to the World Flora Online database

Match taxonomic names using the World Flora Online database, via their
GraphQL API

## Usage

``` r
correctTaxo(
  genus,
  species = NULL,
  interactive = TRUE,
  preferAccepted = FALSE,
  preferFuzzy = FALSE,
  sub_pattern = subPattern(),
  useCache = FALSE,
  useAPI = TRUE,
  capacity = 60,
  fill_time_s = 60,
  timeout = 10
)
```

## Arguments

- genus:

  vector of genera. Alternatively, the whole taxonomic name (genus +
  species)

- species:

  optional, vector of species epithets to be checked (same length as
  `genus`)

- interactive:

  logical, if TRUE (default) user will be prompted to pick names from a
  list where multiple ambiguous matches are found, otherwise names with
  multiple ambiguous matches will be skipped

- preferAccepted:

  logical, if TRUE, if multiple ambiguous matches are found, and if only
  one candidate is an "accepted" name, automatically choose that name

- preferFuzzy:

  logical, if TRUE, if multiple ambiguous matches are found, the
  accepted matched name with the lowest Levenshtein distance to the
  submitted name will be returned

- sub_pattern:

  character vector of regex patterns which will be removed from
  `paste(genus, species)` using
  [`gsub()`](https://rdrr.io/r/base/grep.html). The order of this vector
  matters, substitutions are applied sequentially. Sensible defaults are
  provided by
  [`subPattern()`](https://umr-amap.github.io/BIOMASS/reference/subPattern.md)

- useCache:

  logical, if TRUE use cached values in `the$wfo_cache` preferentially,
  to reduce the number of API calls

- useAPI:

  logical, if TRUE (default) allow API calls

- capacity:

  maximum number of API calls which can accumulate over the duration of
  `fill_time_s`. See documentation for
  [`httr2::req_throttle()`](https://httr2.r-lib.org/reference/req_throttle.html)

- fill_time_s:

  time in seconds to refill the capacity for repeated API calls. See
  documentation for
  [`httr2::req_throttle()`](https://httr2.r-lib.org/reference/req_throttle.html)

- timeout:

  time in seconds to wait before disconnecting from an unresponsive
  request

## Value

data.frame of taxonomic names with rows matching `genus` + `species`.

- nameOriginal:

  Original name as in `genus` + `species`

- nameSubmitted:

  Name after optional sanitisation according to `sub_pattern`

- nameMatched:

  Matched taxonomic name

- nameAccepted:

  Accepted taxonomic name

- familyAccepted:

  Family of accepted name

- genusAccepted:

  Genus of accepted name

- speciesAccepted:

  Species epithet of accepted name

- nameModified:

  Flag indicating if `matchedName` is different from `nameOriginal`, not
  including the removal of excess whitespace

## References

Borsch, T. et al. (2020). *World Flora Online: Placing taxonomists at
the heart of a definitive and comprehensive global resource on the
world's plants*. TAXON, 69, 6. doi10.1002/tax.12373:

## Author

John L. Godlee

## Examples

``` r
if (FALSE) { # \dontrun{
correctTaxo(genus = "Astrocarium", species = "standleanum")
correctTaxo(genus = "Astrocarium", species = "standleanum", interactive = F, preferFuzzy = T)
correctTaxo(genus = "Astrocarium standleanum", interactive = F, preferFuzzy = T)
} # }
```
