require(data.table)

data_wd <- data.table(BIOMASS::wdData)

species <- data_wd[, .(family, genus, wd, .N), by = Binomial][N >= 10][, .(sd = sd(wd)), by = Binomial][, mean(sd)]

genus <- data_wd[, .(family, wd, .N), by = genus][N >= 10][, .(sd = sd(wd)), by = genus][, mean(sd)]

family <- data_wd[, .(family, wd, .N), by = family][N >= 10][, .(sd = sd(wd)), by = family][, mean(sd)]

sd_10 <- data.frame(taxo = c("species", "genus", "family"), sd = c(species, genus, family))
usethis::use_data(sd_10, overwrite = FALSE)
