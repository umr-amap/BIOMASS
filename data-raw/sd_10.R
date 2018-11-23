require(data.table)

# data_wd <- fread("data-raw/wdData.csv", stringsAsFactors = F)
# setnames(
#   data_wd, c("Wood density (g/cm^3), oven dry mass/fresh volume", "Reference Number", "Family", "Region"),
#   c("wd", "referenceNumber", "family", "region")
# )
# data_wd[, c("genus", "species") := tstrsplit(Binomial, " ", keep = 1:2)]

data_wd <- setDT(copy(BIOMASS::wdData))


data_wd <- data_wd[, .(family, genus, wd, .N), by = species]
data_wd <- data_wd[N >= 10]

mean(data_wd[, sd(wd), by = species][, V1])


data_wd <- data_wd[, .(family, wd, .N), by = genus]
data_wd <- data_wd[N >= 10]

mean(data_wd[, sd(wd), by = genus][, V1])


data_wd <- data_wd[, .(family, wd, .N), by = family]
data_wd <- data_wd[N >= 10]

mean(data_wd[, sd(wd), by = family][, V1])

data_wd <- unique(data_wd[, .(family, genus, species_wd = mean(wd)), by = species])

sd(data_wd$species_wd)
sd_10
