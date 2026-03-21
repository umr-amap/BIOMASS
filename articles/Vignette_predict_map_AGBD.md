# Predict maps of AGBD based on inventory and LiDAR data

## Overview

- what data + updated workflow
- bayesian method to propagate uncertainties, using brms (ref), parallel
  using doFuture (ref)
- mathematical model, ref Hunka
- example with 4 plots to illustrate, but keep in mind it is very likely
  not enough data to get proper and reliable final predictions !

## Previous steps (see other vignettes for details)

### Load inventory, plot coordinates and LiDAR data

``` r
data("NouraguesTrees")
data("NouraguesCoords")
nouraguesRaster <- terra::rast(system.file("extdata", "NouraguesRaster.tif",package = "BIOMASS", mustWork = TRUE))
```

### Compute AGBD

``` r
# Height
data("NouraguesHD")
brm_model <- modelHD(
  D = NouraguesHD$D, H = NouraguesHD$H,
  method = "log2",
  bayesian = TRUE, useCache = TRUE)

# Wood density
Taxo <- readRDS(file = "saved_data/Taxo_vignette.rds")
NouraguesTrees$GenusCorrected <- Taxo$genusAccepted
NouraguesTrees$SpeciesCorrected <- Taxo$speciesAccepted
NouraguesTrees$family <- Taxo$familyAccepted
wood_densities <- getWoodDensity(
  genus = NouraguesTrees$GenusCorrected,
  species = NouraguesTrees$SpeciesCorrected,
  family = NouraguesTrees$family,
  stand = NouraguesTrees$Plot
)
NouraguesTrees$WD <- wood_densities$meanWD

error_prop_4plots <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD,
  HDmodel = brm_model,
  Dpropag = "chave2004",
  errWD = wood_densities$sdWD)
# keep only 50 iterations per tree for vignette example
error_prop_4plots$AGB_simu <- error_prop_4plots$AGB_simu[,1:50]
```

### Spatialize AGBD

``` r
# divide plots into subplots
multiple_subplots <- divide_plot(
  grid_size = 25, 
  corner_data = NouraguesCoords,
  rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), corner_plot_ID = "Plot",
  tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"
)
#> Warning in divide_plot(grid_size = 25, corner_data = NouraguesCoords, rel_coord
#> = c("Xfield", : One or more trees could not be assigned to a subplot (not in a
#> subplot area)

# check with raster, optional
multiple_checks <- check_plot_coord(
  corner_data = NouraguesCoords, # NouraguesCoords contains 4 plots
  proj_coord = c("Xutm", "Yutm"), rel_coord = c("Xfield", "Yfield"),
  trust_GPS_corners = TRUE,
  plot_ID = "Plot",
  tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"),
  prop_tree = "D", tree_plot_ID = "Plot",
  ref_raster = nouraguesRaster)
#> In plot 201 : Be careful, one or more trees are not inside the plot defined by rel_coord (see is_in_plot column of tree_data output)
#> In plot 213 : Be careful, one or more trees are not inside the plot defined by rel_coord (see is_in_plot column of tree_data output)
#> In plot 223 : Be careful, one or more trees are not inside the plot defined by rel_coord (see is_in_plot column of tree_data output)
```

![](Vignette_predict_map_AGBD_files/figure-html/spatial_agbd-1.png)![](Vignette_predict_map_AGBD_files/figure-html/spatial_agbd-2.png)![](Vignette_predict_map_AGBD_files/figure-html/spatial_agbd-3.png)![](Vignette_predict_map_AGBD_files/figure-html/spatial_agbd-4.png)

``` r

# compute AGBD estimates and their uncertainty per subplot
subplot_AGBD <- subplot_summary(
  subplots = multiple_subplots,
  AGB_simu = error_prop4plots$AGB_simu, draw = F
)
#> AGB uncertainties will be propagated without propagation of corner GPS measurement uncertainties.
```

### Spatialize LiDAR metric

``` r
# quick plot to visualise plot corners in the landscape
terra::plot(nouraguesRaster)
points(NouraguesCoords$Xutm, NouraguesCoords$Yutm, col ="red", pch = 20)
```

![](Vignette_predict_map_AGBD_files/figure-html/spatial_lidar-1.png)

``` r

# get CHM median values for each suplot
raster_summary <- subplot_summary(
  subplots = multiple_subplots,
  ref_raster = nouraguesRaster, raster_fun = median, na.rm = T)
#> Extracting raster metric...Extracting raster metric done.
#> [[1]]
```

![](Vignette_predict_map_AGBD_files/figure-html/spatial_lidar-2.png)

    #> 
    #> [[1]]

![](Vignette_predict_map_AGBD_files/figure-html/spatial_lidar-3.png)

    #> 
    #> [[1]]

![](Vignette_predict_map_AGBD_files/figure-html/spatial_lidar-4.png)

    #> 
    #> [[1]]

![](Vignette_predict_map_AGBD_files/figure-html/spatial_lidar-5.png)

``` r
chm_subplot <- raster_summary$tree_summary |>
  rename(raster_metric = z2012_median)
```

## Calibrate model

Gather data for agbd-chm model inference (ie join predictor-predicted):

``` r
agbd_subplot <- subplot_AGBD$long_AGB_simu
dt_inf <- agbd_subplot %>%
  left_join(chm_subplot, by = "subplot_ID") %>%
  arrange(subplot_ID)
```

then run calibration function, here parallelized on four CPUs:

``` r
model_cal <- calibrate_model(long_AGB_simu = dt_inf, nb_rep = 50, useCache = T,
                            plot_model = TRUE, chains = 4, thin = 20, iter = 2500,
                            warmup = 500, cores = 4)
```

Let’s check inference results:

``` r
summary(model_cal)
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: log_AGBD ~ 0 + betatilde * log_CHM 
#>          betatilde ~ 1 + gp(x, y, gr = T, scale = T, cov = "matern32")
#>    Data: dt_inf (Number of observations: 3200) 
#>   Draws: 4 chains, each with iter = 2500; warmup = 500; thin = 20;
#>          total post-warmup draws = 400
#> 
#> Gaussian Process Hyperparameters:
#>                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sdgp(betatilde_gpxy)       0.12      0.01     0.09     0.14 1.02      293
#> lscale(betatilde_gpxy)     0.04      0.01     0.03     0.05 1.01      325
#>                        Tail_ESS
#> sdgp(betatilde_gpxy)        223
#> lscale(betatilde_gpxy)      359
#> 
#> Regression Coefficients:
#>                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> betatilde_Intercept     1.70      0.02     1.66     1.73 1.01      238      248
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.18      0.00     0.17     0.18 1.00      392      328
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

Parameters explained

Things to look at : Rhat (should be below 1.05, more generally, the
closest to 1.00), Bulk and Tail ESS (should be the closest possible to
“total post-warmup draws”)

Let’s have a look at the posterior predictive check:

``` r
pp_check(model_cal, ndraws = 50, type = "scatter_avg")
```

![](Vignette_predict_map_AGBD_files/figure-html/mod_ppcheck-1.png)

## Predict AGBD map on the LiDAR footprint

``` r
map_agbd <- predict_map(fit_brms = model_cal,
                         pred_raster = nouraguesRaster,
                         grid_size = 25,
                         raster_fun = median,
                         n_cores = 4,
                         n_post_draws = 100,
                         alignment_raster = NULL,
                         plot_maps = T)
```

If you want to predict on another footprint, you can supply the raster
footprint with `alignment_raster` argument.

## What about validation ?

We suggest to validate the model by subsetting your subplot dataset in
two: a calibration dataset with about 70% of the subplots, and a
validation dataset with the rest. We have 4 plots divided in 16
subplots, so we need to set aside 45 subplots for the calibration step,
the rest will be used to validate.

``` r
# let's select subplots for each step
set.seed(1234)
vector_of_subplots <- unique(dt_inf$subplot_ID)
calibration_subplots <- sample(vector_of_subplots, 45)
validation_subplots <- vector_of_subplots[!(vector_of_subplots %in% calibration_subplots)]

# then divide our complete dataset
dt_calibration <- dt_inf %>% 
  filter(subplot_ID %in% calibration_subplots)
dt_validation <- dt_inf %>% 
  filter(subplot_ID %in% validation_subplots) %>%
  # change in column names to make predict function work
  mutate(x = x_center) %>% 
  mutate(y = y_center) %>%
  mutate(log_CHM = log(raster_metric))
```

Now we can calibrate the model (ie run the inference) with the
calibration dataset:

``` r
cal_step1 <- calibrate_model(long_AGB_simu = dt_calibration, nb_rep = 50, useCache = T,
                            plot_model = TRUE, chains = 4, thin = 20, iter = 2500,
                            warmup = 500, cores = 4)
```

Now we are going to predict the model outputs (namely AGBD) at observed
data points using the parameters that we inferred at step 1 when
calibrating. For that we use the predict function of brms (link to
help):

``` r
val_step2 <- predict(cal_step1, newdata = dt_validation, ndraws = 1)
```

Now we may compare those values predicted by the model, which was
calibrated with the calibration dataset, with observed values from the
validation dataset:

``` r
dt_validation <- cbind(dt_validation, logAGBD_pred = val_step2[,1])

ggplot(dt_validation, aes( x = log(AGBD), y = logAGBD_pred, colour = subplot_ID))+ 
  geom_point(shape = 1)+
  geom_abline(colour = "red", linetype = 2, linewidth = 1.2)+
  xlab("Observed log AGBD")+
  ylab("Predicted log AGBD")
```

![](Vignette_predict_map_AGBD_files/figure-html/plot_val-1.png) Here we
can see that predictions are overall satisfying, except for 3 subplots.
It could be wise to re-run the validation step with a different split of
the original dataset, and to dig into those plots. When you are good
with your validation results, you infer the model *with the entire
dataset*, and then predict a robust AGBD map.

Let’s keep in mind that in this vignette example we are working with a
very small number of inventory plots: we lack of data to get robust AGBD
map predictions, and properly conduct the validation step. But we
encourage BIOMASS users to run that validation procedure, or another one
that is suitable to your data!
