# RUN HIERARCHICAL BAYESIAN MODEL FOR MEAN & SD FOR FAMILY/GENUS/SPECIES
# SAVE A BRMS FIT OBJECT IN data_raw directory

library(data.table)
library(dplyr)
library(stringr)
library(brms)
options(mc.cores = 5)

##1) DATA PREPARATION

# load raw database, not present in the BIOMASS repo because too large, can be found online:
# object "gwdd_v2.2.RData" in the Global Wood Density Database v.2 (GWDD v.2) Zenodo repository https://zenodo.org/records/18262736
load("gwdd_v2.2.RData")

# load binomial database for species names, not present in the BIOMASS repo because too large, can be found online:
# object "gwddagg_v2.2_binomial.csv" in the Global Wood Density Database v.2 (GWDD v.2) Zenodo repository https://zenodo.org/records/18262736
binom_db <- read.csv(file = "gwddagg_v2.2_binomial.csv")
binom_db <- binom_db  %>%
  select(c("binomial", "genus", "epithet", "family"))


# preliminary data cleaning and definition of variables for aggregation
gwdd_v2.2 = gwdd_v2.2[family != ""] # removes records that cannot be matched to families (usually dubious species names)
gwdd_v2.2 = gwdd_v2.2[rank_taxonomic != "genus"]
gwdd_v2.2[, `:=`(longitude_1km = round(longitude,2), latitude_1km = round(latitude,2))]
gwdd_v2.2[, site_withinsource := paste0(source_short, region, country, site, longitude_1km, latitude_1km)]
gwdd_v2.2[, ind := ifelse(plant_agg == 1 | is.na(plant_agg),0,1)]
gwd_db <- gwdd_v2.2 %>%
  select(c("id", "wsg", "family", "genus", "epithet", "species", "ind", "source_short", "site_withinsource"))

# dealing with subsp. or var. or sp x sp
index_subsp <- which(is.na(match(gwd_db$species, binom_db$binomial)))
gwd_db[index_subsp,]$species <- str_c(gwd_db[index_subsp,]$genus, gwd_db[index_subsp,]$epithet, sep = " ")

rm(list='binom_db', 'gwdd_v2.2')

##2) MODEL DEFINITION & INFERENCE

# define model
bf_wsg = bf(
  wsg ~ 1 + (1 | family / genus / species) + (1 | source_short)
  , sigma ~ 1 + ind + (1 | species)
)


fit_wsg = brm(
  formula = bf_wsg
  , data = gwd_db
  , control = list(adapt_delta = 0.99, max_treedepth = 15)
  , iter = 800
  , warmup = 50
  , chains = 4
  , cores = 4
)

saveRDS(fit_wsg, file = "data-raw/fit_wsg_nosubsp_all.RDS")
