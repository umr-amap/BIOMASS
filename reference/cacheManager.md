# Function that return a possibly cached file, transparently downloading it if missing

Function that return a possibly cached file, transparently downloading
it if missing

## Usage

``` r
cacheManager(nameFile)
```

## Arguments

- nameFile:

  character. file to resolve cached path.

## Value

file path of the resolved cached file.

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

## Author

Guillaume CORNU
