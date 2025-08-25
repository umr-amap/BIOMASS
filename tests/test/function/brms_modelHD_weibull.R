library(brms)
library(tidyverse)

data("NouraguesTrees")
Taxo <- readRDS(file = "vignettes/saved_data/Taxo_vignette.rds")
NouraguesTrees$GenusCorrected <- Taxo$genusCorrected
NouraguesTrees$SpeciesCorrected <- Taxo$speciesCorrected
wood_densities <- getWoodDensity(
  genus = NouraguesTrees$GenusCorrected,
  species = NouraguesTrees$SpeciesCorrected,
  stand = NouraguesTrees$Plot # for unidentified or non-documented trees in the reference database
)
NouraguesTrees$WD <- wood_densities$meanWD

### Testing without error propagation

# Testing without stand-specific models
lm_model <- modelHD(D =  NouraguesHD$D, H = NouraguesHD$H, method = "weibull", useWeight = FALSE, drawGraph = T, bayesian = FALSE)
plot(lm_model$model)
lm_model$RSE

lm_model_weight <- modelHD(D =  NouraguesHD$D, H = NouraguesHD$H, method = "weibull", useWeight = TRUE, drawGraph = T, bayesian = FALSE)
lm_model_weight$model
lm_model_weight$RSE
predict(lm_model_weight$model , newdata = data.frame(D = 50))
coefficients(lm_model_weight$model)
44.1396429 * (1-exp(-(50/33.8848210)^0.8527532))

brm_model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H,
                     method = "weibull", bayesian = TRUE, useCache = TRUE,
                     useWeight = FALSE, drawGraph = T, cores = 3)

brm_model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H,
                     method = "weibull", bayesian = TRUE, useCache = TRUE,
                     useWeight = FALSE, drawGraph = T, cores = 3,
                     thin = 10, iter = 6000, warmup = 1000,
                     prior = c(set_prior(prior = "uniform(0,80)", lb = 0, ub = 80, class = "b",nlpar = "a"),
                               set_prior(prior = "uniform(0,100)", lb = 0, ub = 100, class = "b",nlpar = "b"),
                               set_prior(prior = "uniform(0.1,0.9)", lb = 0.1, ub = 0.9, class = "b",nlpar = "c")))
# saveRDS(brm_model, file = "~/Documents/saves/brm_models/weibull/brm_model.rds")
# brm_model <- readRDS(file = "~/Documents/saves/brm_models/weibull/brm_model.rds")
brm_model$model
plot(brm_model$model)
brm_model$RSE
brm_model_weight <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H,
                            method = "weibull", bayesian = TRUE, useCache = TRUE,
                            useWeight = TRUE, drawGraph = T, cores = 3,
                            thin = 10, iter = 6000, warmup = 1000,
                            prior = c(set_prior(prior = "uniform(0,80)", lb = 0, ub = 80, class = "b",nlpar = "a"),
                                      set_prior(prior = "uniform(0,100)", lb = 0, ub = 100, class = "b",nlpar = "b"),
                                      set_prior(prior = "uniform(0.1,0.99)", lb = 0.1, ub = 0.99, class = "b",nlpar = "c")))
# saveRDS(brm_model_weight, file = "~/Documents/saves/brm_models/weibull/brm_model_weight.rds")
# brm_model_weight <- readRDS(file = "~/Documents/saves/brm_models/weibull/brm_model_weight.rds")
plot(brm_model_weight$model)
brm_model_weight$model
brm_model_weight$RSE
verif_df <- data.frame(D = 10:100)
coef_brm_model <- fixef(brm_model$model)[,1]
verif_df <- verif_df |> 
  mutate(pred_fun = predict(brm_model$model , newdata = verif_df)[,1] ,
         pred_formula_weibull = coef_brm_model[1] * (1-exp(-(D/coef_brm_model[2])^coef_brm_model[3])) )
verif_df |> pivot_longer(c("pred_fun","pred_formula_weibull")) |> ggplot(aes(x=D,y=value,col=name)) + geom_point() + theme_minimal()
coef_brm_model <- fixef(brm_model_weight$model)[,1]
verif_df <- verif_df |> 
  mutate(pred_fun = predict(brm_model_weight$model , newdata = verif_df)[,1] ,
         pred_formula_weibull = coef_brm_model[1] * (1-exp(-(D/coef_brm_model[2])^coef_brm_model[3])) )
verif_df |> pivot_longer(c("pred_fun","pred_formula_weibull")) |> ggplot(aes(x=D,y=value,col=name)) + geom_point() + theme_minimal()

H_lm_model <- retrieveH(D = NouraguesTrees$D, model = lm_model)
H_brm_model <- retrieveH( D = NouraguesTrees$D, model = brm_model)
H_brm_model_weighted <- retrieveH( D = NouraguesTrees$D, model = brm_model_weight)
plot(H_lm_model$H , predict(lm_model$model, newdata = data.frame(D=NouraguesTrees$D))) ; abline(a = c(0,1), col="red") # OK !
plot(H_brm_model$H , predict(brm_model$model, newdata = data.frame(D=NouraguesTrees$D))[,1]) ; abline(a = c(0,1), col="red") # OK !
plot(H_brm_model_weighted$H , predict(brm_model_weight$model, newdata = data.frame(D=NouraguesTrees$D))[,1]) ; abline(a = c(0,1), col="red") # OK !
plot(H_lm_model$H , H_brm_model$H) ; abline(a = c(0,1), col="red") # OK !
plot(H_brm_model$H , H_brm_model_weighted$H) ; abline(a = c(0,1), col="red") # OK !


# Testing with stand-specific models
forest_inv <- read.csv("../BIOMASSapp/inst/shinyApp/exemple_data/forest_inv_exemple.csv")
lm_model_plots <- modelHD(D =  forest_inv$D, H = forest_inv$H, method = "weibull", useWeight = FALSE, drawGraph = T, plot = forest_inv$Plot)
lm_model_plots_weighted <- modelHD(D =  forest_inv$D, H = forest_inv$H, method = "weibull", useWeight = TRUE, drawGraph = T, plot = forest_inv$Plot)
brm_model_plots <- modelHD(D = forest_inv$D, H = forest_inv$H, plot = forest_inv$Plot,
                           method = "weibull", bayesian = TRUE, useCache = TRUE,
                           useWeight = FALSE, drawGraph = T, cores = 3,
                           thin = 10, iter = 6000, warmup = 1000,
                           prior = c(set_prior(prior = "uniform(0,80)", lb = 0, ub = 80, class = "b",nlpar = "a"),
                                     set_prior(prior = "uniform(0,150)", lb = 0, ub = 150, class = "b",nlpar = "b"),
                                     set_prior(prior = "uniform(0.1,0.99)", lb = 0.1, ub = 0.99, class = "b",nlpar = "c")))
# saveRDS(brm_model_plots, file = "~/Documents/saves/brm_models/weibull/brm_model_plots.rds")
# brm_model_plots <- readRDS(file = "~/Documents/saves/brm_models/weibull/brm_model_plots.rds")
plot(brm_model_plots$`201`$model)
plot(brm_model_plots$`204`$model)
plot(brm_model_plots$`213`$model)
plot(brm_model_plots$`223`$model)
brm_model_plots_weighted <- modelHD(D =  forest_inv$D, H = forest_inv$H, plot = forest_inv$Plot,
                                    method = "weibull", bayesian = TRUE, useCache = TRUE,
                                    useWeight = TRUE, drawGraph = T, cores = 3,
                                    thin = 10, iter = 6000, warmup = 1000,
                                    prior = c(set_prior(prior = "uniform(0,80)", lb = 0, ub = 80, class = "b",nlpar = "a"),
                                              set_prior(prior = "uniform(0,150)", lb = 0, ub = 150, class = "b",nlpar = "b"),
                                              set_prior(prior = "uniform(0.1,0.99)", lb = 0.1, ub = 0.99, class = "b",nlpar = "c")))
# saveRDS(brm_model_plots_weighted, file = "~/Documents/saves/brm_models/weibull/brm_model_plots_weighted.rds")
# brm_model_plots_weighted <- readRDS(file = "~/Documents/saves/brm_models/weibull/brm_model_plots_weighted.rds")
plot(brm_model_plots_weighted$`201`$model)
plot(brm_model_plots_weighted$`204`$model)
plot(brm_model_plots_weighted$`213`$model)
plot(brm_model_plots_weighted$`223`$model)

ggpubr::ggarrange(lm_model_plots$`201`$fitPlot,lm_model_plots$`204`$fitPlot,lm_model_plots$`213`$fitPlot,lm_model_plots$`223`$fitPlot)
ggpubr::ggarrange(lm_model_plots_weighted$`201`$fitPlot,lm_model_plots_weighted$`204`$fitPlot,lm_model_plots_weighted$`213`$fitPlot,lm_model_plots_weighted$`223`$fitPlot)
ggpubr::ggarrange(brm_model_plots$`201`$fitPlot,brm_model_plots$`204`$fitPlot,brm_model_plots$`213`$fitPlot,brm_model_plots$`223`$fitPlot)
ggpubr::ggarrange(brm_model_plots_weighted$`201`$fitPlot,brm_model_plots_weighted$`204`$fitPlot,brm_model_plots_weighted$`213`$fitPlot,brm_model_plots_weighted$`223`$fitPlot)

H_lm_model_plots <- retrieveH(D = forest_inv$D, model = lm_model_plots, plot = forest_inv$Plot)
H_lm_model_plots_weighted <- retrieveH(D = forest_inv$D, model = lm_model_plots_weighted, plot = forest_inv$Plot) ; H_lm_model_plots_weighted$RSE
plot(H_lm_model_plots$H , H_lm_model_plots_weighted$H) ; abline(a = c(0,1), col="red") # OK !

H_brm_model_plots <- retrieveH(D = forest_inv$D, model = brm_model_plots, plot = forest_inv$Plot)
H_brm_model_plots_weighted <- retrieveH(D = forest_inv$D, model = brm_model_plots_weighted, plot = forest_inv$Plot)

plot(H_lm_model_plots$H , H_brm_model_plots$H) ; abline(a = c(0,1), col="red") # OK !
plot(H_lm_model_plots_weighted$H, H_brm_model_plots_weighted$H) ; abline(a = c(0,1), col="red") # OK !
plot(H_brm_model_plots_weighted$H , H_brm_model_plots$H) ; abline(a = c(0,1), col="red") # OK !

### Testing with error propagation
len <- length(NouraguesTrees$D) ; n=500
fivePercent <- round(len * 5 / 100)
myrtruncnorm <- function(n, lower = -1, upper = 1, mean = 0, sd = 1) {
  qnorm(p = runif(n = n, min = pnorm(lower, mean = mean, sd = sd), max = pnorm(upper, mean = mean, sd = sd)), mean = mean, sd = sd)}
chaveError <- function(x, len) {
  ## Assigning large errors on 5% of the trees
  largeErrSample <- sample(len, fivePercent)
  D_sd <- 0.0062 * x + 0.0904 # Assigning small errors on the remaining 95% trees
  D_sd[largeErrSample] <- 4.64
  x <- myrtruncnorm(n = len, mean = x, sd = D_sd, lower = 0.1, upper = 500)
  return(x)
}
D_simu <- suppressWarnings(replicate(n, chaveError(x = NouraguesTrees$D, len = len) ) )
D_simu[1:10,1:10] ; dim(D_simu)
Dpropag <- 0
D_simu <- suppressWarnings(replicate(n, myrtruncnorm(len, mean = NouraguesTrees$D, sd = Dpropag, lower = 0.1, upper = 500)))

### non stand-specific models
pred_lm_model <- predictHeight(D = D_simu, model = lm_model, err = TRUE, plot = NULL)
pred_lm_model_weighted <- predictHeight(D = D_simu, model = lm_model_weight, err = TRUE, plot = NULL)
pred_brm_model <- predictHeight(D = D_simu, model = brm_model, err = TRUE, plot = NULL)
pred_brm_model_weighted <- predictHeight(D = D_simu, model = brm_model_weight, err = TRUE, plot = NULL)

plot(pred_lm_model |> rowMeans() , pred_brm_model |> rowMeans()) ; abline(a=c(0,1),col="red")
plot(pred_lm_model |> rowMeans() , pred_lm_model_weighted |> rowMeans()) ; abline(a=c(0,1),col="red")

plot(pred_brm_model |> rowMeans() , pred_brm_model_weighted |> rowMeans()) ; abline(a=c(0,1),col="red")
plot(pred_lm_model_weighted |> rowMeans() , pred_brm_model_weighted |> rowMeans()) ; abline(a=c(0,1),col="red")

plot(apply(pred_lm_model,1,sd) , apply(pred_brm_model,1,sd) ) ; abline(a=c(0,1),col="red") 
plot(apply(pred_lm_model,1,sd) , apply(pred_lm_model_weighted,1,sd) ) ; abline(a=c(0,1),col="red") 
plot(apply(pred_brm_model,1,sd) , apply(pred_brm_model_weighted,1,sd) ) ; abline(a=c(0,1),col="red") 

# Visualize differences on the sum of the tree heights
sum_lm <- summaryByPlot(AGB_val = pred_lm_model, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method="method_lm")
sum_brm <- summaryByPlot(AGB_val = pred_brm_model, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method="method_brm")
sum_brm_weighted <- summaryByPlot(AGB_val = pred_brm_model_weighted, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method="method_brm_weighted")
sum_df <- as.data.table(rbind(sum_lm,sum_brm,sum_brm_weighted))
sum_df[order(plot,method) , i:=.I ]
ggplot(data = sum_df, aes(x = as.factor(i), y = AGB, col=method)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=Cred_2.5, ymax=Cred_97.5), width=.2) +
  labs(x = "", title = "AGB by plot") +
  scale_x_discrete(breaks = sum_df$i, labels = sum_df$plot) +
  theme_minimal()

selec_tree <- 1:10
plot_list <- lapply(selec_tree , function(i) {
  df <- data.frame(lm_model = pred_lm_model[i,], brm_model = pred_brm_model[i,]) |> pivot_longer(cols = c("lm_model","brm_model"), names_to = "model", values_to = "H_simu")
  ret <- df |> ggpubr::gghistogram(x="H_simu" , col="model", y = "density", add_density = TRUE)
})
ggpubr::ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]])

# AGBmonCarlo verification: 
WD_simu <- suppressWarnings(replicate(n, myrtruncnorm(n = len, mean = NouraguesTrees$WD, sd = rep(0,nrow(NouraguesTrees)), lower = 0.08, upper = 1.39)))

AGB_lm_model <- AGBmonteCarlo(D = NouraguesTrees$D, WD = NouraguesTrees$WD, errWD = rep(0,nrow(NouraguesTrees)), H = NULL, errH = NULL, HDmodel = lm_model,
                              coord = NULL, Dpropag = "chave2004", n = 1000, Carbon = FALSE, Dlim = NULL)
sum_lm <- summaryByPlot(AGB_val = AGB_lm_model$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method="lm")

AGB_brm_model <- AGBmonteCarlo(D = NouraguesTrees$D, WD = NouraguesTrees$WD, errWD = rep(0,nrow(NouraguesTrees)), H = NULL, errH = NULL, HDmodel = brm_model,
                                   coord = NULL, Dpropag = "chave2004", n = 1000, Carbon = FALSE, Dlim = NULL)
sum_brm <- summaryByPlot(AGB_val = AGB_brm_model$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method = "brm")

AGB_brm_model_weighted <- AGBmonteCarlo(D = NouraguesTrees$D, WD = NouraguesTrees$WD, errWD = rep(0,nrow(NouraguesTrees)), H = NULL, errH = NULL, HDmodel = brm_model_weight,
                                            coord = NULL, Dpropag = "chave2004", n = 1000, Carbon = FALSE, Dlim = NULL)
sum_brm <- summaryByPlot(AGB_val = AGB_brm_model_weighted$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method = "brm")

sum_df <- as.data.table(rbind(sum_lm, sum_brm))
sum_df[order(AGB) , i:=.I ]
ggplot(data = sum_df, aes(x = as.factor(i), y = AGB, col=method)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=Cred_2.5, ymax=Cred_97.5), width=.2) +
  labs(x = "", title = "AGB by plot") +
  scale_x_discrete(breaks = sum_df$i, labels = sum_df$plot) +
  theme_minimal()

plot(apply(AGB_lm_model$AGB_simu,1,sd) , apply(AGB_brm_model$AGB_simu,1,sd) ) ; abline(a=c(0,1),col="red")

plot_list <- lapply(selec_tree , function(i) {
  df <- data.frame(lm_model = AGB_lm_model$AGB_simu[i,], brm_model = AGB_brm_model$AGB_simu[i,]) |> pivot_longer(cols = c("lm_model","brm_model"), names_to = "model", values_to = "AGB_simu")
  ret <- df |> ggpubr::gghistogram(x="AGB_simu" , col="model", y = "density", add_density = TRUE)
})
ggpubr::ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]])


# stand-specific models
pred_lm_model <- predictHeight(D = D_simu, model = lm_model_plots, err = TRUE, plot = NouraguesTrees$Plot)
pred_brm_model <- predictHeight(D = D_simu, model = brm_model_plots, err = TRUE, plot = NouraguesTrees$Plot)

plot(pred_lm_model |> rowMeans() , pred_brm_model |> rowMeans()) ; abline(a=c(0,1),col="red")

plot(apply(pred_lm_model,1,sd) , apply(pred_brm_model,1,sd) ) ; abline(a=c(0,1),col="red") 

AGB_lm_model <- AGBmonteCarlo(D = NouraguesTrees$D, WD = NouraguesTrees$WD, errWD = rep(0,nrow(NouraguesTrees)), H = NULL, errH = NULL, HDmodel = lm_model_plots,
                              coord = NULL, Dpropag = "chave2004", n = 1000, Carbon = FALSE, Dlim = NULL)
sum_lm <- summaryByPlot(AGB_val = AGB_lm_model$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method="lm")

AGB_brm_model <- AGBmonteCarlo(D = NouraguesTrees$D, WD = NouraguesTrees$WD, errWD = rep(0,nrow(NouraguesTrees)), H = NULL, errH = NULL, HDmodel = brm_model_plots,
                                   coord = NULL, Dpropag = "chave2004", n = 1000, Carbon = FALSE, Dlim = NULL)
sum_brm <- summaryByPlot(AGB_val = AGB_brm_model$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = T) |> mutate(method = "brm")

sum_df <- as.data.table(rbind(sum_lm, sum_brm))
sum_df[order(AGB) , i:=.I ]
ggplot(data = sum_df, aes(x = as.factor(i), y = AGB, col=method)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=Cred_2.5, ymax=Cred_97.5), width=.2) +
  labs(x = "", title = "AGB by plot") +
  scale_x_discrete(breaks = sum_df$i, labels = sum_df$plot) +
  theme_minimal()

plot(apply(AGB_lm_model$AGB_simu,1,sd) , apply(AGB_brm_model$AGB_simu,1,sd) ) ; abline(a=c(0,1),col="red")

