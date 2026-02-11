# GENERATE MEAN & SD FOR FAMILY/GENUS/SPECIES FROM PREDICTIONS
# SAVED IN FILE data/wsg_estimates.rda

# authors: Dominique Lamonica, Fabian Fischer

library(data.table)
library(dplyr)
library(stringr)
library(brms)

# load the fit object resulting from "inference_gwd_all_cluster.R"
# not present in the BIOMASS repo because too large
 readRDS("data-raw/fit_wsg_nosubsp_all.RDS") -> fit_wsg

# data : 1 population per species, population size = ind_sample_size
ind_sample_size <- 10
data_for_prediction_unique <- unique(fit_wsg$data %>%
  select(c("species", "family", "genus", "family:genus", "family:genus:species")) %>%
  mutate(ind = 1) %>%
  mutate(source_short = "Aguilar-Rodríguez et al. 2001") %>%
  arrange(genus, species)
)# for now forced to keep a column "source_short" bo fitted bug when predicting sigmas with re_formula != NULL
data_for_prediction <-  do.call("rbind", replicate(
  ind_sample_size, data_for_prediction_unique, simplify = FALSE)) %>%
  arrange(genus, species)

#number nd of draws of joint posterior distributions
nd <- 200

## when BUG in fitted
sigma_param <- fitted(fit_wsg, newdata = data_for_prediction, summary = F, ndraws = nd, dpar= "sigma")
mu_param <- fitted(fit_wsg, newdata = data_for_prediction, summary = F, ndraws = nd, dpar= "mu", 
  re_formula = wsg ~ 1 + (1 | family / genus / species))
generated_ind_wd <- matrix(mapply(FUN = rnorm, n = 1, mean = mu_param, sd = sigma_param), 
nrow = nd*ind_sample_size)

## mean & sd : species level
#for each species ie each column compute mean and sd
mean_sp <- apply(generated_ind_wd, MARGIN = 2, FUN = mean)
sd_sp <- apply(generated_ind_wd, MARGIN = 2, FUN = sd)
estimates_sp <- cbind(data_for_prediction_unique[,1:3],
   mean_species = mean_sp, sd_species = sd_sp, level_tax = "species")

data_test <- cbind(data_for_prediction_unique[,1:3], t(generated_ind_wd))

## mean & sd : genus level
#for each genus get all generated values and compute mean and sd
genus_vect <- as.numeric()
mean_genus_vect <- as.numeric()
sd_genus_vect <- as.numeric()
nb_species_per_genus <- as.numeric()
for (i in 1:length(unique(data_test$genus))){

  temp <- data_test %>%
    filter(genus == unique(data_test$genus)[i]) 

  genus_vect <-  c(genus_vect, unique(data_test$genus)[i])
  nb_species_per_genus <- c(nb_species_per_genus,length(unique(temp$species)))#epithet does not work
  
   temp_values <- temp %>% 
    select(!1:3)
    
  mean_genus_vect <-  c(mean_genus_vect, mean(unlist(temp_values)))
  sd_genus_vect <- c(sd_genus_vect, sd(unlist(temp_values)))
}

df_genus <- data.frame(genus = genus_vect, n_species_in_genus = nb_species_per_genus,
  mean_genus = mean_genus_vect,  sd_genus = sd_genus_vect)


## mean & sd : family level
#for each family get all generated values and compute mean and sd
family_vect <- as.numeric()
mean_family_vect <- as.numeric()
sd_family_vect <- as.numeric()
nb_species_per_family <- as.numeric()
for (i in 1:length(unique(data_test$family))){

  temp <- data_test %>%
    filter(family == unique(data_test$family)[i]) 

  family_vect <-  c(family_vect, unique(data_test$family)[i])
  nb_species_per_family <- c(nb_species_per_family,length(unique(temp$species)))#epithet does not work
  
  temp_values <- temp %>% 
    select(!1:3)
    
  mean_family_vect <-  c(mean_family_vect, mean(unlist(temp_values)))
  sd_family_vect <- c(sd_family_vect, sd(unlist(temp_values)))
}

df_family <- data.frame(family = family_vect, n_species_in_family = nb_species_per_family,
  mean_family = mean_family_vect,  sd_family = sd_family_vect)

## now merge to get one table
estimates_sp_genus <- merge(estimates_sp, df_genus, by = "genus")
estimates_sp_genus_family <- merge(estimates_sp_genus, df_family, by = "family")


## export a rda file for BIOMASS function getWoodDensity
## family rows
family_unique <- unique(estimates_sp_genus_family %>%
  select(c(family, mean_family, sd_family))
) %>%
  mutate(genus = NA, species = NA, level_tax = "family") %>%
  rename(wsg = mean_family, sd = sd_family) %>%
  relocate(c(genus, species), .after = family)

## genus rows
genus_unique <- unique(estimates_sp_genus_family %>%
  select(c(family, genus, mean_genus, sd_genus))
) %>%
  mutate(species = NA, level_tax = "genus") %>%
  rename(wsg = mean_genus, sd = sd_genus) %>%
  relocate(c(species), .after = genus)

## species rows
species_unique <- estimates_sp_genus_family %>%
  select(c(family, genus, species, mean_species, sd_species, level_tax)) %>%
  rename(wsg = mean_species, sd = sd_species)

## combine all three tables & properly order rows
wsg_estimates <- rbind(family_unique, genus_unique, species_unique)
setorder(wsg_estimates, family, genus, species,na.last=F)

## save in rda format
save(wsg_estimates, file = "data/wsg_estimates.rda")
