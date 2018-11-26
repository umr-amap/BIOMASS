require(devtools)
require(data.table)


# download and reading ----------------------------------------------------

if (!file.exists("data-raw/wdData.csv")) {
  require(readxls)

  url <- "http://datadryad.org/bitstream/handle/10255/dryad.235/GlobalWoodDensityDatabase.xls?sequence=1"
  tmp <- tempfile(fileext = ".xls")
  download.file(url, tmp)

  wdData <- readxl::read_xls(tmp, sheet = 2)
  setDT(wdData)
  fwrite(wdData, file = "data-raw/wdData.csv")
}



# data preparation --------------------------------------------------------

wdData <- fread("data-raw/wdData.csv", stringsAsFactors = F)

# change the names of the column
setnames(
  wdData, c("Wood density (g/cm^3), oven dry mass/fresh volume", "Reference Number", "Family", "Region"),
  c("wd", "referenceNumber", "family", "region")
)

# split the Binomial column in two distinct part : genus and species
wdData[, c("genus", "species") := tstrsplit(Binomial, " ", keep = 1:2)]

# replace the space by _ in the column region
wdData[, region := gsub(" ", "_", region)]

# create the regionId column
wdData[, regionId := region]

# replace the values to regionId
wdData[region == "South_America_(tropical)", regionId := "SouthAmericaTrop"]
wdData[region == "Central_America_(tropical)", regionId := "CentralAmericaTrop"]
wdData[region == "Australia/PNG_(tropical)", regionId := "AustraliaTrop"]

wdData[region == "Africa_(extratropical)", regionId := "AfricaExtraTrop"]
wdData[region == "South_America_(extratropical)", regionId := "SouthAmericaExtraTrop"]
wdData[region == "South-East_Asia_(tropical)", regionId := "SouthEastAsiaTrop"]

wdData[region == "Africa_(tropical)", regionId := "AfricaTrop"]
wdData[region == "South-East_Asia", regionId := "SouthEastAsia"]

# remove all value NA from wd
wdData <- wdData[!is.na(wd), ]




# build the data ----------------------------------------------------------

setDF(wdData)
usethis::use_data(wdData, overwrite = F)



# Verification
# wdData[, Number := seq(.N)]
# wdData[ family != BIOMASS::wdData$family, .(Number, family)]
# wdData[, family := getTaxonomy(genus)$family]
#
# diff <- wdData[ family != BIOMASS::wdData$family, .(Number, family, genus, species, wd)]
