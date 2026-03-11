# Correct trees taxonomy

This function corrects typos for a given taxonomic name using the
Taxonomic Name Resolution Service (TNRS).

## Usage

``` r
correctTaxo(
  genus,
  species = NULL,
  score = 0.5,
  useCache = FALSE,
  verbose = TRUE,
  accepted = FALSE
)
```

## Arguments

- genus:

  Vector of genera to be checked. Alternatively, the whole species name
  (genus + species) or (genus + species + author) may be given (see
  example).

- species:

  (optional) Vector of species to be checked (same size as the genus
  vector).

- score:

  Score of the matching ( see https://tnrs.biendata.org/instructions )
  below which corrections are discarded.

- useCache:

  logical. Whether or not use a cache to reduce online search of taxa
  names (NULL means use cache but clear it first)

- verbose:

  logical. If TRUE various messages are displayed during process

- accepted:

  logical. If TRUE accepted names will be returned instead of matched
  names. Cache will not be used as synonymy changes over time.

## Value

The function returns a dataframe with the corrected (or not) genera and
species.

## Details

This function create a file named correctTaxo.log (see Localisation),
this file have the memory of all the previous requests, as to avoid the
replication of time-consuming server requests.

By default, names are queried in batches of 500, with a 0.5s delay
between each query. These values can be modified using options:
`options(BIOMASS.batch_size=500)` for batch size (max 1000),
`options(BIOMASS.wait_delay=0.5)` for delay (in seconds).

## Localisation

Cache path discovery protocol

1.  BIOMASS.cache option set to an **existing** folder

2.  **existing** user data folder
    [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html)

    - On Linux : `~/.local/share/R/BIOMASS`

    - On Mac OS X : `~/Library/Application Support/R/BIOMASS`

    - On Windows 7 up to 10 :
      `C:\\Users\\<username>\\AppData\\Local\\R\\BIOMASS`

    - On Windows XP :
      `C:\\Documents and Settings\\<username>\\Data\\R\\BIOMASS`

3.  fallback to R session tempdir

## References

Boyle, B. et al. (2013). *The taxonomic name resolution service: An
online tool for automated standardization of plant names*. BMC
bioinformatics, 14, 1. doi:10.1186/1471-2105-14-16

## Author

Ariane TANGUY, Arthur PERE, Maxime REJOU-MECHAIN, Guillaume CORNU

## Examples

``` r
if (FALSE) { # \dontrun{
correctTaxo(genus = "Astrocarium", species = "standleanum")
correctTaxo(genus = "Astrocarium standleanum")
} # }
```
