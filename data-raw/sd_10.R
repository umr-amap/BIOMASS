require(data.table)

# data_wd <- fread("data-raw/wdData.csv", stringsAsFactors = F)
# setnames(
#   data_wd, c("Wood density (g/cm^3), oven dry mass/fresh volume", "Reference Number", "Family", "Region"),
#   c("wd", "referenceNumber", "family", "region")
# )
# data_wd[, c("genus", "species") := tstrsplit(Binomial, " ", keep = 1:2)]

data_wd <- setDT(copy(BIOMASS::wdData))


species = data_wd[, .(family, genus, wd, .N), by = species][N >= 10][, .(sd = sd(wd)), by = species][, mean(sd)]

genus = data_wd[, .(family, wd, .N), by = genus][N >= 10][, .(sd = sd(wd)), by = genus][, mean(sd)]

family = data_wd[, .(family, wd, .N), by = family][N >= 10][, .(sd = sd(wd)), by = family][, mean(sd)]

sd_10 = data.frame(taxo = c("species", "genus", "family"), sd = c(species, genus, family))
usethis::use_data(sd_10, overwrite = F)