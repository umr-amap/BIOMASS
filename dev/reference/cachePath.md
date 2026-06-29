# Function used to build a file path based on a cache folder

Parameters are similar to that of file.path function

## Usage

``` r
cachePath(...)
```

## Arguments

- ...:

  character vectors. Elements of the subpath of cache path

## Value

A character vector of normalized file path with a source attribute
holding a hint to cache path source ("option", "data", "temp")

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
