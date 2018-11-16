require(data.table)

data_wd = setDT(copy(BIOMASS::wdData))

data_wd[, .(family, genus, species_wd = mean(wd)), by = species]
