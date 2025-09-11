require(data.table)

#gwdd_v2.1 <- fread("~/Documents/GWDD/gwdd_v2.1.csv", stringsAsFactors = FALSE)
load("~/Documents/GWDD/gwdd_v2.1.RData") # equivalent to gwdd_v2.1.csv with NA's instead of empty cell
# names(gwdd_v2.1)
# sum(gwdd_v2.1$family=="") # 34
# sum(gwdd_v2.1$genus=="") # 0
# sum(gwdd_v2.1$species=="") # 0
# sum(gwdd_v2.1$epithet=="") # 4806
# sum(gwdd_v2.1$epithet_infraspecific=="") # 108,021

gwdd_v2_sp <- fread("~/Documents/GWDD/gwddagg_v2.1_species.csv", stringsAsFactors = FALSE)


gwdd_v2.1 <- gwdd_v2.1[,c("family","genus_reference","species_reference","epithet_reference","epithet_infraspecific_reference","rank_taxonomic","wsg","region")]

setnames(gwdd_v2.1,
         old = c("genus_reference","species_reference","epithet_reference","epithet_infraspecific_reference","wsg"),
         new = c("genus","species","epithet","epithet_infraspecific","wd") )

species_v2 <- gwdd_v2.1[, .(family, genus, wd, .N), by = species][N >= 10][, .(sd = sd(wd)), by = species][, mean(sd)]
# 0.076 vs 0.133 for the GWDD_v1

genus_v2 <- gwdd_v2.1[, .(family, wd, .N), by = genus][N >= 10][, .(sd = sd(wd)), by = genus][, mean(sd)]
# 0.091 vs 0.094 for the GWDD_v1

family_v2 <- gwdd_v2.1[, .(family, wd, .N), by = family][N >= 10][, .(sd = sd(wd)), by = family][, mean(sd)]
# 0.111 vs 0.124 for the GWDD_v1

sd_10_GWDD_v2 <- data.frame(taxo = c("species", "genus", "family"), sd = c(species_v2, genus_v2, family_v2))
usethis::use_data(sd_10_GWDD_v2, overwrite = FALSE)
