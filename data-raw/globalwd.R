require(devtools)
require(data.table)

if (!file.exists("data-raw/wdData.csv")) {
  require(readxls)

  url <- "http://datadryad.org/bitstream/handle/10255/dryad.235/GlobalWoodDensityDatabase.xls?sequence=1"
  tmp <- tempfile(fileext = ".xls")
  download.file(url, tmp)

  wdData <- readxl::read_xls(tmp, sheet = 2)
  setDT(wdData)
  fwrite(wdData, file = "data-raw/wdData.csv")
}

wdData <- fread("data-raw/wdData.csv", stringsAsFactors = F)

setnames(
  wdData, c("Wood density (g/cm^3), oven dry mass/fresh volume", "Reference Number", "Family", "Region"),
  c("wd", "referenceNumber", "family", "region")
)

wdData[, c("genus", "species") := tstrsplit(Binomial, " ", keep = 1:2)]

wdData[, region := gsub(" ", "_", region)]
wdData[, regionId := region]
wdData[region == "South_America_(tropical)", regionId := "SouthAmericaTrop"]
wdData[region == "Central_America_(tropical)", regionId := "CentralAmericaTrop"]
wdData[region == "Australia/PNG_(tropical)", regionId := "AustraliaTrop"]

wdData[region == "Africa_(extratropical)", regionId := "AfricaExtraTrop"]
wdData[region == "South_America_(extratropical)", regionId := "SouthAmericaExtraTrop"]
wdData[region == "South-East_Asia_(tropical)", regionId := "SouthEastAsiaTrop"]

wdData[region == "Africa_(tropical)", regionId := "AfricaTrop"]
wdData[region == "South-East_Asia", regionId := "SouthEastAsia"]

wdData <- wdData[!is.na(wd), ]
wdData[, Number := seq_along(Number)]

all.equal(wdData[, .(family, genus, species, wd, region, referenceNumber, regionId)], setDT(BIOMASS::wdData),
  check.attributes = FALSE
)

wdData[ family != BIOMASS::wdData$family, .(Number, family)]
wdData[, family := getTaxonomy(genus)$family]

diff <- wdData[ family != BIOMASS::wdData$family, .(Number, family, genus, species, wd)]
BIOMASS::wdData[diff$Number, c()]
