# script to create estimates of mean wood density and its uncertainty  (intraspecific variation + measurement error)
# models are fitted with brms to hierarchically model variance
# model fitting is time consuming due to large number of hierarchical layers

# install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
# library(cmdstanr)
# check_cmdstan_toolchain()
# install_cmdstan(cores = 2)
# cmdstan_path()
# cmdstan_version()

library(data.table)
library(brms)

dir_current = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(dir_current)

#%%%%%%%%%%%%%%%%%%%%%%%#
###### Prepare data #####
#%%%%%%%%%%%%%%%%%%%%%%%#

# load database
gwdd02 = fread("gwdd02/gwdd_v2.2.csv")

# preliminary data cleaning and definition of variables for aggregation
gwdd02 = gwdd02[family != ""] # removes records that cannot be matched to families (usually dubious species names)
gwdd02 = gwdd02[rank_taxonomic != "genus"]
gwdd02[, `:=`(longitude_1km = round(longitude,2), latitude_1km = round(latitude,2))]
gwdd02[, site_withinsource := paste0(source_short, region, country, site, longitude_1km, latitude_1km)]
gwdd02[, ind := ifelse(plant_agg == 1 | is.na(plant_agg),0,1)]

# define model
bf_wsg = bf(
  wsg ~ 1 + (1 | family / genus / species) + (1 | source_short) # alternative: (1 | source_short / site_withinsource)
  , sigma ~ 1 + ind + (1 | family / genus / species)
)

# define model without source effect
bf_wsg_nosource = bf(
  wsg ~ 1 + (1 | family / genus / species)
  , sigma ~ 1 + ind + (1 | family / genus / species)
)


# # check default priors
# get_prior(
#   formula = bf_wsg
#   , data = gwdd02
# )

# weakly informative priors
prior_wsg = c(
  # Mean part
  prior(normal(0.55, 0.10), class = "Intercept"),
  prior(normal(0.05, 0.05), class = "sd", lb = 0),
  
  # Sigma part (log link)
  prior(normal(log(0.1), 0.30), class = "Intercept", dpar = "sigma"), 
  prior(normal(0, 0.25), class = "b", dpar = "sigma"),
  prior(normal(0.25, 0.25), class = "sd", dpar = "sigma", lb = 0)
)

# # run test model (only Pinales, only 100 records per species)
# gwdd02_pinales = gwdd02[order == "Pinales"]
# gwdd02_pinales = gwdd02_pinales[, .SD[sample(.N, min(100, .N))], by = species]
# gwdd02_pinales[,.N,ind]
# 
# fit_wsg_pinales = brm(
#   formula = bf_wsg
#   , prior = prior_wsg
#   , data = gwdd02_pinales
#   , control = list(adapt_delta = 0.99, max_treedepth = 15)
#   , iter = 2500
#   , warmup = 1500
#   , chains = 4
#   , cores = 4
#   , silent = F
#   , backend = "cmdstanr"
#   , sample_prior = T
# )
# 
# prior_d = prior_draws(fit_wsg_pinales)
# posterior_d = as_draws_df(fit_wsg_pinales)
# 
# get_prior(fit_wsg_pinales)
# 
# ggplot() + geom_histogram(data = prior_d, aes(x = Intercept), fill = "orange", alpha = 0.5) + geom_histogram(data = posterior_d, aes(x = Intercept), fill = "red", alpha = 0.5) + theme_classic()

#%%%%%%%%%%%%%%%%%%%%%#
###### Fit models #####
#%%%%%%%%%%%%%%%%%%%%%#

# run full model
fit_wsg = brm(
  formula = bf_wsg
  , prior = prior_wsg
  , data = gwdd02
  , control = list(adapt_delta = 0.99, max_treedepth = 15)
  , iter = 2500
  , warmup = 1500
  , chains = 4
  , cores = 4
  , silent = F
  , backend = "cmdstanr"
  , sample_prior = T
)
save(fit_wsg, file = "products/fit_wsg.RData")

# model without informed priors
fit_wsg_uninformed = brm(
  formula = bf_wsg
  , data = gwdd02
  , control = list(adapt_delta = 0.99, max_treedepth = 15)
  , iter = 2500
  , warmup = 1500
  , chains = 4
  , cores = 4
  , silent = F
  , sample_prior = T
)
save(fit_wsg_uninformed, file = "products/fit_wsg_uninformed.RData")

# model without source effect
fit_wsg_nosource = brm(
  formula = bf_wsg_nosource
  , prior = prior_wsg
  , data = gwdd02
  , control = list(adapt_delta = 0.99, max_treedepth = 15)
  , iter = 2500
  , warmup = 1500
  , chains = 4
  , cores = 4
  , silent = F
  , backend = "cmdstanr"
  , sample_prior = T
)
save(fit_wsg_nosource, file = "products/fit_wsg_nosource.RData")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###### Extract wood density and sigma estimates #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# The aim is to create a simple data.frame that works almost exactly like posterior_predict

# load the data
# load("products/fit_wsg.RData")
load("products/fit_wsg_uninformed.RData")
load("products/fit_wsg_nosource.RData")

# we select one and process it further (could be turned into a function with input fit_wsg)
fit_wsg = copy(fit_wsg_uninformed)
summary(fit_wsg)

# extract fixefs
fit_wsg_fixefs = fixef(fit_wsg)
intercept_wsg = fit_wsg_fixefs["Intercept","Estimate"]
intercept_sigmalog = fit_wsg_fixefs["sigma_Intercept","Estimate"] 

# if applicable, add the indicator for individual measurements (as opposed to aggregates)
intercept_sigmalog = intercept_sigmalog + fit_wsg_fixefs["sigma_ind","Estimate"]

# extract variances
varcorr_wsg = VarCorr(fit_wsg)

# extract ranefs
fit_wsg_ranefs = ranef(fit_wsg)

# family level
ranefs_family = as.data.table(fit_wsg_ranefs$`family`)
colnames(ranefs_family) = c("family","summary","param","dev_family")
ranefs_family = ranefs_family[summary == "Estimate"] # for now just the estimates
ranefs_family[, sd_family := sd(dev_family), c("param")]
ranefs_family[, n_family := .N, c("param")]

# genus level
ranefs_genus = as.data.table(fit_wsg_ranefs$`family:genus`)
colnames(ranefs_genus) = c("family_genus","summary","param","dev_genus")
ranefs_genus[, `:=`(
  family = tstrsplit(family_genus,"_")[[1]]
  , genus = tstrsplit(family_genus,"_")[[2]]
)][,family_genus := NULL]
ranefs_genus = ranefs_genus[summary == "Estimate"] # for now just the estimates
ranefs_genus[, sd_genus := sd(dev_genus), c("family","param")]
ranefs_genus[, n_genus := .N, c("family","param")]

# species level
ranefs_species = as.data.table(fit_wsg_ranefs$`family:genus:species`)
colnames(ranefs_species) = c("family_genus_species","summary","param","dev_species")
ranefs_species[, `:=`(
  family = tstrsplit(family_genus_species,"_")[[1]]
  , genus = tstrsplit(family_genus_species,"_")[[2]]
  , species = tstrsplit(family_genus_species,"_")[[3]]
)][,family_genus_species := NULL][, epithet := tstrsplit(species," ")[[2]]]
ranefs_species = ranefs_species[summary == "Estimate"] # for now just the estimates
ranefs_species = ranefs_species[!is.na(epithet)] # remove genus only input records (could also be removed before model fitting, but provide some extra information on genus means)
ranefs_species[, sd_species := sd(dev_species), c("family","genus","param")]
ranefs_species[, n_species := .N, c("family","genus","param")]

# now put everything together
wsg_hierarchical = merge(ranefs_family[param == "Intercept",.(family,dev_family,sd_family,n_family)],ranefs_genus[param == "Intercept",.(family, genus, dev_genus,sd_genus,n_genus)], by = "family")
wsg_hierarchical = merge(wsg_hierarchical, ranefs_species[param == "Intercept",.(family, genus, species, dev_species, sd_species, n_species)], by = c("family","genus"))
wsg_hierarchical[, intercept := intercept_wsg]
wsg_hierarchical[
  , `:=`(
    sdest_family = varcorr_wsg$family$sd["Intercept","Estimate"]
    , sdest_genus = varcorr_wsg$`family:genus`$sd["Intercept","Estimate"]
    , sdest_species = varcorr_wsg$`family:genus:species`$sd["Intercept","Estimate"]
    , wsg_family = intercept + dev_family
    , wsg_genus = intercept + dev_family + dev_genus
    , wsg_species = intercept + dev_family + dev_genus + dev_species
  )
]
wsg_hierarchical = wsg_hierarchical[, c("family","genus","species","n_family","n_genus","n_species","sd_family","sd_genus","sd_species","sdest_family","sdest_genus","sdest_species","wsg_family","wsg_genus","wsg_species")]

# same for sigma (logscale)
sigmalog_hierarchical = merge(ranefs_family[param == "sigma_Intercept",.(family,dev_family)],ranefs_genus[param == "sigma_Intercept",.(family, genus, dev_genus)], by = "family")
sigmalog_hierarchical = merge(sigmalog_hierarchical, ranefs_species[param == "sigma_Intercept",.(family, genus, species, dev_species)], by = c("family","genus"))
sigmalog_hierarchical[, intercept := intercept_sigmalog]
sigmalog_hierarchical[
  , `:=`(
    sigmalog_family = intercept + dev_family
    , sigmalog_genus = intercept + dev_family + dev_genus
    , sigmalog_species = intercept + dev_family + dev_genus + dev_species
  )
][
  , `:=`(
    sigma_family = exp(sigmalog_family)
    , sigma_genus = exp(sigmalog_genus)
    , sigma_species = exp(sigmalog_species)
  )
]
sigma_hierarchical = sigmalog_hierarchical[, c("family","genus","species","sigma_family","sigma_genus","sigma_species")]
wsg_hierarchical = merge(wsg_hierarchical, sigma_hierarchical, by = c("family","genus","species"))

fwrite(wsg_hierarchical, file = "products/wsg_hierarchical.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###### Predict wood density and uncertainty #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# quick and dirty function to predict
predict.wsg = function(taxa_supplied, wsg_hierarchical, cutoff.sd_emp = Inf){
  wsg_hierarchical_species = wsg_hierarchical[species %in% taxa_supplied$species,c("family","genus","species","wsg_species","sigma_species")]
  colnames(wsg_hierarchical_species) = c("family","genus","species","wsg","sd") 
  wsg_hierarchical_species[, level_tax := "species"]
  
  wsg_hierarchical_genus = unique(merge(wsg_hierarchical[,c("family","genus","wsg_genus","n_species","sd_species","sdest_species","sigma_genus")], taxa_supplied[!species %in% wsg_hierarchical$species,c("species","genus","family")], by = c("family","genus")))
  wsg_hierarchical_genus[n_species >= cutoff.sd_emp, sdest_species := sd_species] # replace estimated SD with measured one for extremely well sampled genera
  wsg_hierarchical_genus = wsg_hierarchical_genus[, list(wsg = wsg_genus, sd = sqrt(sdest_species^2 + sigma_genus^2)),c("family","genus","species")]
  wsg_hierarchical_genus[, level_tax := "genus"]
  
  wsg_pred = rbind(wsg_hierarchical_species, wsg_hierarchical_genus)
  return(wsg_pred)
}

# load the hierarchical decomposition
wsg_hierarchical = fread(file = "products/wsg_hierarchical.csv")

# now apply to the entire gwdd02
gwdd02 = fread("gwdd02/gwdd_v2.2.csv")
gwdd02 = gwdd02[family != ""] 
gwdd02 = gwdd02[rank_taxonomic != "genus"]
gwdd02[, species := gsub("_"," ",species)]

gwdd02_species = unique(gwdd02[,c("family","genus","species")])
gwdd02_genus = unique(gwdd02[,c("family","genus")])
gwdd02_genus[, species  := genus]
gwdd02_combined = unique(rbind(gwdd02_species,gwdd02_genus))

wsg_estimates = predict.wsg(gwdd02_combined, wsg_hierarchical)
fwrite(wsg_estimates, file = "products/wsg_estimates.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###### Check predictions #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# check out prediction at species level
wsg_check_species = merge(gwdd02[,c("family","genus","species","wsg")], wsg_estimates[,c("family","genus","species","wsg","sd")], by = c("family","genus","species"))

ggplot(data = wsg_check_species, aes(x = wsg.y, y = wsg.x)) + geom_point(size = 0.1) + theme_classic() + geom_smooth(method = "lm") + xlim(c(0,1.5)) + ylim(c(0,1.5)) + geom_abline(intercept = 0, slope = 1) + xlab("predicted") + ylab("measured") # note: we removed source effects/systematic errors, so some deviations are expected

cor(wsg_check_species$wsg.x,  wsg_check_species$wsg.y)^2 # R2 = 0.75
sqrt(mean((wsg_check_species$wsg.x - wsg_check_species$wsg.y)^2)) # RMSE ~ 0.076
mean(wsg_check_species$sd) # mean error ~ 0.067, slightly smaller than RMSE, but makes sense, because RMSE includes a lot of measurement errors etc.


# check out genus level
wsg_check_genus = merge(gwdd02[,c("family","genus","wsg")], unique(wsg_estimates[level_tax == "genus",c("family","genus","wsg","sd")]), by = c("family","genus"))

ggplot(data = wsg_check_genus, aes(x = wsg.y, y = wsg.x)) + geom_point(size = 0.1) + theme_classic() + geom_smooth(method = "lm") + xlim(c(0,1.5)) + ylim(c(0,1.5)) + geom_abline(intercept = 0, slope = 1) + xlab("predicted") + ylab("measured") # note: we removed source effects/systematic errors, so some deviations are expected

cor(wsg_check_genus$wsg.x,  wsg_check_genus$wsg.y)^2 # R2 = 0.75
sqrt(mean((wsg_check_genus$wsg.x - wsg_check_genus$wsg.y)^2)) # RMSE ~ 0.104
mean(wsg_check_genus$sd) # mean error ~ 0.095, slightly smaller than RMSE, but makes sense, because RMSE includes a lot of measurement errors etc.

# check out a few interesting taxa
wsg_estimates[species %like% "Ochroma"]
wsg_estimates[species %like% "Guaiacum"] 
wsg_estimates[species %like% "Adansonia"]
wsg_estimates[species %like% "Ficus"]
wsg_estimates[species %like% "Fagus"] 
wsg_estimates[species %like% "Picea"] 
wsg_estimates[species %like% "Larix"] 
wsg_estimates[species %like% "Eschweilera"] 
wsg_estimates[species %like% "Dipterocarpus"] 
wsg_estimates[species %like% "Acacia"] 
wsg_estimates[species %like% "Eucalypt"] 
