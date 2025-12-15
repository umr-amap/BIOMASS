# Here is the script to produce data/wsg_estimates.rda from wsg_hierarchical.csv which should be available to download at the zenodo repository:
# https://zenodo.org/records/16928343 -> products.zip

# Otherwise, if Fabian didn't update the zenodo repository, wsg_hierarchical can be created with make_wdagg.R script (which is not available yet but that can be found at data-raw/make.wdagg.R)

# Otherwise, wsg_hierarchical.csv is available at Fabian's dropbox (email 11 Novembre 2025 13:28)

# ONCE THE wsg_estimates.rda WILL BE FINALISED, DON'T FORGET TO UPDATE data_documentation.R !!!

library(data.table)

wsg_hierarchical <- fread(file = "~/Documents/GWDD/v2.2/products_from_Fabian_dropbox/wsg_hierarchical_v2.csv")
# the wsg_hierarchical_v2.csv contains wsg_hierarchical.csv with standardized taxa names (some "_" and "  " were still present in the original dataset)

### Format GWDD2 predictions data-base: 
gwdd02 <- fread("~/Documents/GWDD/v2.2/gwdd02/gwdd_v2.2.csv")
gwdd02 <- gwdd02[family != ""] 
gwdd02 <- gwdd02[rank_taxonomic != "genus"]
# species
gwdd02_species <- gwdd02[, species := gsub("_"," ",species)]
gwdd02_species[, species := gsub("  "," ",species)]
gwdd02_species <- unique(gwdd02_species[,c("family","genus","species")])
# genus
gwdd02_genus <- unique(gwdd02[,c("family","genus")])
gwdd02_genus[, species  := NA]
# family
gwdd02_family <- unique(gwdd02[,"family"])
gwdd02_family[, c("genus","species")  := list(NA,NA)]

wsg_estimates <- unique(rbind(gwdd02_species, gwdd02_genus, gwdd02_family))

# Verification:
wsg_estimates[!is.na(species), ][! species %in% wsg_hierarchical$species, ] # Paulownia Paulownia: we will have to remove it
wsg_estimates[is.na(species) & !is.na(genus), ][! genus %in% wsg_hierarchical$genus, ] # OK
wsg_estimates[is.na(species) & is.na(genus), ][! family %in% wsg_hierarchical$family, ] # OK

### Fill wsg_estimates with estimates and sd at species, genus and family level:
## At species level:
wsg_hierarchical_species <- wsg_hierarchical[, c("family","genus","species","wsg_species","sigma_species")]
setnames(wsg_hierarchical_species, c("wsg_species","sigma_species"), c("wsg","sd"))
wsg_hierarchical_species[, level_tax := "species"]
wsg_estimates <- merge(wsg_estimates, wsg_hierarchical_species, all.x = TRUE)

## At genus level:
wsg_hierarchical_genus = unique(wsg_hierarchical[,c("family","genus","wsg_genus","n_species","sdest_species","sigma_genus")])
wsg_hierarchical_genus[, wsg := wsg_genus]
# THIS WILL BE REPLACED WITH THE NEW MODEL sigma ~ 1 + (1|species) WITH SIMULATIONS OF SD(species) TO RETRIEVE SD(genus)
wsg_hierarchical_genus[, sd := sqrt(sigma_genus^2 + sdest_species^2)] # BUT FOR NOW ADDITION OF VARIANCES : 
# sigma_genus = exp(sigmalog_genus) = exp(intercept + ranef(fit_wsg)$`family + ranef(fit_wsg)$`family:genus) , i.e the prediction of the model
# sdest_species = summary(fit_wsg)$random$`family:genus:species`$Estimate , i.e the mean species variability
wsg_hierarchical_genus[, level_tax := "genus"]

# merging with wsg_estimates
wsg_estimates[ is.na(species) & !is.na(genus),
                 c("wsg", "sd", "level_tax") := wsg_hierarchical_genus[.SD,
                                                                       on = .(family, genus),
                                                                       .(wsg, sd, level_tax)]
]

## At family level:
wsg_hierarchical_family <- unique(wsg_hierarchical[, c("family","n_genus","sd_genus","wsg_family","sdest_genus","sdest_species","sigma_family")])
wsg_hierarchical_family[, wsg := wsg_family]
wsg_hierarchical_family[, sd := sqrt(sigma_family^2 + sdest_species^2 + sdest_genus^2)] # ADDITION OF VARIANCES : 
# sigma_family = exp(sigmalog_family) = intercept + ranef(fit_wsg)$`family , i.e the prediction of the model
# sdest_genus = summary(fit_wsg)$random$`family:genus`$Estimate , i.e the mean genus variability
# sdest_species = summary(fit_wsg)$random$`family:genus:species`$Estimate , i.e the mean species variability
wsg_hierarchical_family[, level_tax := "family"]

# merging with wsg_estimates
wsg_estimates[ is.na(genus) & is.na(species),
                 c("wsg", "sd", "level_tax") := wsg_hierarchical_family[.SD,
                                                                        on = .(family),
                                                                        .(wsg, sd, level_tax)]


]

usethis::use_data(wsg_estimates, overwrite = TRUE)
