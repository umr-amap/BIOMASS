# Correct and match taxonomic names to the World Flora Online database

Match taxonomic names using the World Flora Online database, via their
GraphQL API

## Usage

``` r
correctTaxo(
  genus,
  species,
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

  character vector of regex patterns which will be removed from names in
  `x` using [`gsub()`](https://rdrr.io/r/base/grep.html). The order of
  this vector matters, substitutions are applied sequentially. Sensible
  defaults are provided by
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

- x:

  vector of taxonomic names

## Value

data.frame of taxonomic names with rows matching names in `x`.

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
# \donttest{
correctTaxo(genus = "Astrocarium", species = "standleanum")
#> 
#> 
#> --- Pick a name ---
#> Matching string: astrocarium standleanum
#> 1   wfo-0000293953   Astrocaryum standleyanum    L.H.Bailey  accepted    Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/standleyanum
#> 2   wfo-0000293087   Astrocaryum aculeatum   G.Mey.  accepted    Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/aculeatum
#> 3   wfo-0000293066   Astrocaryum aculeatum   Barb.Rodr.  synonym Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/rodriguesii$Astrocaryum/aculeatum
#> 4   wfo-0000293076   Astrocaryum aculeatum   Wallace synonym Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Bactris/balanophora$Astrocaryum/aculeatum
#> 5   wfo-0000293098   Astrocaryum alatum  H.F.Loomis  accepted    Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/alatum
#> 6   wfo-0000293920   Astrocaryum sechellarum (H.Wendl.) Baill.   synonym Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Phoenicophorium/borsigianum$Astrocaryum/sechellarum
#> 7   wfo-0000293252   Astrocaryum ciliatum    F.Kahn & B.Millán   accepted    Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/ciliatum
#> 8   wfo-0000293273   Astrocaryum confertum   H.Wendl. ex Burret  accepted    Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/confertum
#> 9   wfo-0000293909   Astrocaryum scopatum    F.Kahn & B.Millán   accepted    Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/scopatum
#> 10  wfo-0000293133   Astrocaryum aureum  Griseb. & H.Wendl.  synonym Code/Plantae/Pteridobiotina/Angiosperms/Arecales/Arecaceae/Astrocaryum/tucuma$Astrocaryum/aureum
#> Enter a number to pick a row from the list, a valid WFO ID, 'N' for the next page, 'P' for the previous page, 'S' to skip this name: 
#>              nameOriginal           nameSubmitted nameMatched nameAccepted
#> 1 Astrocarium standleanum astrocarium standleanum        <NA>         <NA>
#>   familyAccepted genusAccepted speciesAccepted nameModified
#> 1           <NA>          <NA>            <NA>           NA
correctTaxo(genus = "Astrocarium", species = "standleanum", interactive = F, preferFuzzy = T)
#>              nameOriginal           nameSubmitted              nameMatched
#> 1 Astrocarium standleanum astrocarium standleanum Astrocaryum standleyanum
#>               nameAccepted familyAccepted genusAccepted speciesAccepted
#> 1 Astrocaryum standleyanum      Arecaceae   Astrocaryum    standleyanum
#>   nameModified
#> 1         TRUE
# }
```
