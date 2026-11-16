# Estimate stand biomass

This vignette describes how to compute above ground biomass (AGB) and
its associated uncertainty from plot inventory data. For the sake of
clarity, the article follows the same workflow as in the BIOMASS paper,
excluded the shaded area:

![BIOMASS workflow - Article Estimate
biomass](img/BIOMASS_workflow_articles.png)

BIOMASS workflow - Article Estimate biomass

## General workflow and required data

As can be seen, the **estimate of the above ground biomass** (AGB) of a
tree, and **its associated uncertainty**, is based on its **wood
density**, **diameter**, and **height**.

However, exhaustive values of **wood density** and **height** are rarely
available in forest inventory data. This is why the package proposes an
estimate of these two covariables, based on more usual data.

In this vignette, we will use some of the data obtained in 2012 from a
[forest
inventory](https://dataverse.cirad.fr/dataset.xhtml?persistentId=doi:10.18167/DVN1/TZ1RL9)
conducted in 2012 in the Nouragues forest (French Guiana). For
educational purpose, some virtual trees have been added to the data.

``` r

data("NouraguesTrees")
knitr::kable(head(NouraguesTrees))
```

|  | Site | Plot | Xfield | Yfield | Family | Genus | Species | D |
|:---|:---|---:|---:|---:|:---|:---|:---|---:|
| 14 | Petit_Plateau | 201 | 0.0 | 31.5 | Burseraceae | Protium | surinamense | 11.0 |
| 44 | Petit_Plateau | 201 | 0.1 | 75.2 | Anacardiaceae | Tapirira | guianensis | 74.4 |
| 13 | Petit_Plateau | 201 | 0.2 | 27.6 | Lecythidaceae | Indet.Lecythidaceae | Indet. | 25.4 |
| 2810 | Petit_Plateau | 201 | -4.0 | 67.5 | Euphorbiaceae | Conceveiba | guyanensis | 10.0 |
| 24 | Petit_Plateau | 201 | 0.3 | 39.9 | Burseraceae | Protium | altissimum | 18.9 |
| 12100 | Petit_Plateau | 201 | -3.5 | 41.5 | Euphorbiaceae | Mabea | speciosa | 10.0 |

These data do not contain any information on wood density or height of
trees. Only diameter is known, as no estimate can be made without this
information.

## Wood density

**Wood density** is estimated **from tree taxonomy**, using the [Global
Wood Density Database v.2](https://zenodo.org/records/18262736) as a
reference. So the first step might be to correct tree taxonomy.

### Checking and retrieving tree taxonomy

This is done with the
[`correctTaxo()`](https://umr-amap.github.io/BIOMASS/reference/correctTaxo.md)
function, but before calling it, let’s speak about cache !

When the function is called for the first time with the argument
`useCache = TRUE`, a temporary file containing the request to
[WFO](https://www.worldfloraonline.org/) will be automatically created
in an existing folder. Once this has been done, during the current
session **the use of `useCache = TRUE` will access the saved temporary
file** in order to avoid time-consuming replication of server requests.
But **by quitting the current R session, this temporary file will be
removed**. So before calling
[`correctTaxo()`](https://umr-amap.github.io/BIOMASS/reference/correctTaxo.md),
we advise you to define a folder which will host the cache file
permanently, enabling to work offline.

``` r

# By default
createCache() 
# Or if you want to set your own cache folder
createCache("the_path_to_your_cache_folder") 
# Or 
options("BIOMASS.cache" = "the_path_to_your_cache_folder")
```

That said, let’s continue with the call to
[`correctTaxo()`](https://umr-amap.github.io/BIOMASS/reference/correctTaxo.md)
function:

``` r

Taxo <- correctTaxo(
  genus = NouraguesTrees$Genus, # genus also accepts the whole species name (genus + species) or (genus + species + author) 
  species = NouraguesTrees$Species, 
  useCache = TRUE, interactive = F, preferFuzzy = T)
```

The corrected genus and species of the trees can now be added to the
data:

``` r

NouraguesTrees$GenusCorrected <- Taxo$genusAccepted
NouraguesTrees$SpeciesCorrected <- Taxo$speciesAccepted
```

Here, as an example, the species name of the fourth tree has been
corrected from “guyanensis” to “guianensis” (the fourth row of
correctTaxo() output has a TRUE value for the column nameModified) :

``` r

NouraguesTrees$Species[4]
#> [1] "guyanensis"
Taxo[4,]
#>            nameOriginal         nameSubmitted           nameMatched
#> 4 Conceveiba guyanensis conceveiba guyanensis Conceveiba guianensis
#>            nameAccepted familyAccepted genusAccepted speciesAccepted
#> 4 Conceveiba guianensis  Euphorbiaceae    Conceveiba      guianensis
#>   nameModified
#> 4         TRUE
```

You can also retrieve families from genus names in the familyAccepted
column.

``` r

NouraguesTrees$family <- Taxo$familyAccepted
```

### Getting wood density

Wood densities are retrieved using
[`getWoodDensity()`](https://umr-amap.github.io/BIOMASS/reference/getWoodDensity.md)
function. By default, this function assigns to each taxon a species- or
genus-level average if at least one wood density value of the same
species or genus is available in the reference database. For
unidentified trees or if the genus is missing in the reference database,
the stand-level mean wood density is assigned to the tree.

``` r

wood_densities <- getWoodDensity(
  genus = NouraguesTrees$GenusCorrected,
  species = NouraguesTrees$SpeciesCorrected,
  stand = NouraguesTrees$Plot # for unidentified or non-documented trees in the reference database
)
#> Your taxonomic table contains 379 taxa
#> Warning in getWoodDensity(genus = NouraguesTrees$GenusCorrected, species =
#> NouraguesTrees$SpeciesCorrected, : 169 taxa don't match the Global Wood Density
#> Database V2. You may provide 'family' to match wood density estimates at family
#> level.

NouraguesTrees$WD <- wood_densities$meanWD
```

For information, here are the number of wood density values estimated at
the species, genus and plot level:

``` r

# At species level
sum(wood_densities$levelWD == "species")
#> [1] 1635
# At genus level
sum(wood_densities$levelWD == "genus")
#> [1] 246
# At plot level
sum(!wood_densities$levelWD %in% c("genus", "species"))
#> [1] 169
```

The `family` argument also assigns to the trees a family-level wood
density average, but bear in mind that the taxon-average approach gives
relatively poor estimates above the genus level (Flores & Coomes 2011).

**Additional wood density values (mean and error) can be added** using
the `addWoodDensityData` argument (here invented for the example):

``` r

LocalWoodDensity <- data.frame(
  genus = c("Paloue", "Handroanthus"),
  species = c("princeps", "serratifolius"),
  meanWD = c(0.65, 0.72),
  sdWD = c(0.061, 0.042))

add_wood_densities <- getWoodDensity(
  genus = NouraguesTrees$GenusCorrected,
  species = NouraguesTrees$SpeciesCorrected,
  family = NouraguesTrees$family,
  stand = NouraguesTrees$Plot,
  addWoodDensityData = LocalWoodDensity
)
```

## Height

As tree height measurements are rare, or rarely exhaustive, BIOMASS
proposes three methods to estimate tree height:

If a subset of well-measured trees is available in the studied region:

- Construct a local Height–Diameter (H-D) allometry

If not:

- Use the continent- or region-speciﬁc H–D models proposed by
  [Feldpausch et
  al. (2012)](https://bg.copernicus.org/articles/9/3381/2012/)
- Use a generic H–D model based on a single bioclimatic predictor E (eqn
  6a in [Chave et
  al. 2014](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.12629))

### Building a local H-D model

As no height measurements is available in the `NouraguesTrees` dataset,
we will use the `NouraguesHD` dataset which contains the height and
diameter measurements of two 1-ha plots from the Nouragues forest.

``` r

data("NouraguesHD")
```

The
[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)
function is used to either **compare four implemented models to ﬁt H–D
relationships** in the tropics, or to **compute the desired H-D model**.

Here we first compare the four H-D models using linear regression (for
log^1 and log^2 models) or nonlinear least squares estimates (for
Michaelis-Menten and Weibull models) :

``` r

HD_res <- modelHD(
  D = NouraguesHD$D, H = NouraguesHD$H,
  useWeight = TRUE, drawGraph = T)
```

![](Vignette_BIOMASS_files/figure-html/multiple_modelHD-1.png)

    #> To build a HD model you must use the parameter 'method' in this function

    kable(HD_res)

| method    |      RSE |    RSElog | Average_bias |
|:----------|---------:|----------:|-------------:|
| log1      | 4.700088 | 0.2472750 |    0.0724598 |
| log2      | 4.329727 | 0.2240379 |    0.0182504 |
| weibull   | 4.307951 |        NA |   -0.0186951 |
| michaelis | 4.294488 |        NA |   -0.0064068 |

As the log2 model has the lowest RSE, we will build this model using the
`method` argument and add its predictions to the dataset with the
[`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
function:

``` r

HDmodel <- modelHD(
  D = NouraguesHD$D, H = NouraguesHD$H,
  method = "log2", useWeight = TRUE, 
  bayesian = FALSE)

H_model <- retrieveH(
  D = NouraguesTrees$D,
  model = HDmodel)

NouraguesTrees$H_model <-  H_model$H
```

By default, `bayesian = TRUE` in
[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)
arguments. In this case, a Bayesian model will be fitted using the brms
package (see section [Building bayesian Height-Diameter
models](#bayesian-models)), ensuring proper propagation of parameter
uncertainties. As this method is more time-consuming, for educational
purposes, we will set bayesian = FALSE.

Note that if some of the trees’ heights had been measured in the
`NouraguesTrees` dataset, we could have provided these heights to
[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md).
In this case, we could also have created a model for each
stand/sub-region present in `NouraguesTrees` using the `plot` argument.
But keep in mind that even for well-measured trees,
[`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
gives you the predicted height (see [this
section](#mixing-measured_estimated)).

### Using the continent- or region-speciﬁc H–D model (Feldpausch)

No need to compute any model here, as the predictions of the model
proposed by Feldspausch et al. are directly retrieved by the
[`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
function. Simply indicate the region concerned:

``` r

H_feldspausch <- retrieveH(
  D = NouraguesTrees$D,
  region = "GuianaShield")

NouraguesTrees$H_feldspausch <- H_feldspausch$H
```

Available regions are listed in the documentation of the function.

### Using the generic H–D model based on a bioclimatic predictor (Chave)

In the same way as for the previous model, the predictions of the model
proposed by Chave et al. are directly retrieved by the
[`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
function. Coordinates of the plot (or trees) in a longitude/latitude
format must be provided.

``` r

data("NouraguesCoords") #contains corner coordinates
coords <- apply(NouraguesCoords[c("Long","Lat")] , 2, mean) # compute the mean of the corner coordinates

H_chave  <- retrieveH(
  D = NouraguesTrees$D,
  coord = coords)

NouraguesTrees$H_chave <- H_chave$H
```

## Estimate AGB

Once tree diameter, wood density and height have been retrieved, the
generalised allometric model (eqn 4 of [Chave et
al. (2014)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.12629))
can be used with the
[`computeAGB()`](https://umr-amap.github.io/BIOMASS/reference/computeAGB.md)
function, where AGB values are reported in Mg instead of in kg:

``` r

NouraguesTrees$AGB <- computeAGB(
  D = NouraguesTrees$D,
  WD = NouraguesTrees$WD,
  H = NouraguesTrees$H_model #here with the local H-D predictions
  )
```

For AGB estimates using tree heights obtained by the “Chave method”
(H_chave), it is more accurate to provide the area coordinates directly
instead of the tree height estimates:

``` r

NouraguesTrees$AGB_Chave <- computeAGB(
    D = NouraguesTrees$D,
    WD = NouraguesTrees$WD,
    coord = coords)
```

## Propagate AGB errors

The
[`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
function allows the user to propagate different sources of error up to
the final AGB estimate. The propagation of the error due to the
**uncertainty of the model variables** (D, WD and H) can be
**parameterized by the user**. The propagation of the error due to the
**uncertainty of the allometry model parameters** is automatically
performed by the function.

### Diameter measurement error

Using the `Dpropag` argument of the
[`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
function, the user can set diameter measurement errors by:

- providing a standard deviation value corresponding to the measurement
  uncertainty (e.g `Dpropag = 1`)
- providing a vector of standard deviation values associated with each
  tree measurement uncertainty
- using the implemented example of Chave et al. 2004 with
  `Dpropag = "chave2004"`, where small errors are applied on 95% of the
  trees and large errors to the remaining 5%

``` r

D_error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD, H = NouraguesTrees$H_model,
  Dpropag = "chave2004",
  errWD = rep(0,nrow(NouraguesTrees)), errH = 0 # no error propagation on WD and H here
)
```

### Wood density error

The
[`getWoodDensity()`](https://umr-amap.github.io/BIOMASS/reference/getWoodDensity.md)
function returns prior standard deviation values associated with each
tree wood density using the mean standard deviation of the [global wood
density database](https://datadryad.org/dataset/doi:10.5061/dryad.234)
at the species, genus and family levels.

This output can be provided through the `errWD` argument:

``` r

WD_error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD, H = NouraguesTrees$H_model,
  errWD = wood_densities$sdWD,
  Dpropag = 0 , errH = 0 # no error propagation on D and H here
)
```

### Height error

The user can provide either a SD value or a vector of SD values
associated with tree height measurement errors, using the `errH`
argument.

- **If tree heights have been estimated via a local HD-model**, instead
  of the providing `errH` and `H` arguments, the user has to provide the
  output of the
  [`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)
  function using the `modelHD` argument:

``` r

H_model_error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD, # we do not provide H
  HDmodel = HDmodel, # but we provide HDmodel
  Dpropag = 0 , errWD = rep(0,nrow(NouraguesTrees)) # no error propagation on D and WD here
)
```

Note that when HDmodel is not a bayesian model, uncertainties of the
parameters are not propagated during the error propagation of the
AGBmonteCarlo() function.

- **If tree heights have been estimated via the “Feldspausch” method**,
  the user has to provide the output of the
  [`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
  function for the `errH` argument:

``` r

H_feld_error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD,
  H = NouraguesTrees$H_feldspausch, errH = H_feldspausch$RSE, # we provide H and errH
  Dpropag = 0 , errWD = rep(0,nrow(NouraguesTrees)) # no error propagation on D and WD here
)
```

- **If tree heights have been estimated via the “Chave” method**, the
  user has to provide the coordinates of the area (or of the trees)
  using the `coord` argument:

``` r

H_chave_error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD, # we do not provide H
  coord = coords, # but we provide the vector of median coordinates of the plots
  Dpropag = 0 , errWD = rep(0,nrow(NouraguesTrees)) # no error propagation on D and WD here
)
```

### AGB visualisation and plot level summary

Let’s propagate diameter, wood density and height errors, using the
HD-model for height:

``` r

error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesTrees$WD, # we do not provide H
  HDmodel = HDmodel, # but we provide HDmodel
  Dpropag = "chave2004",
  errWD = wood_densities$sdWD)

error_prop[(1:4)]
#> $meanAGB
#> [1] 1674.958
#> 
#> $medAGB
#> [1] 1674.216
#> 
#> $sdAGB
#> [1] 45.70764
#> 
#> $credibilityAGB
#>     2.5%    97.5% 
#> 1587.405 1769.685
```

The first four elements of the output contain the mean, median, standard
deviation and credibility intervals of the total AGB of the dataset but
nothing about the AGB at the plot level. To do this, you can use the
[`summaryByPlot()`](https://umr-amap.github.io/BIOMASS/reference/summaryByPlot.md)
function:

``` r

AGB_by_plot <- summaryByPlot(AGB_val = error_prop$AGB_simu, plot = NouraguesTrees$Plot, drawPlot = TRUE)
```

![](Vignette_BIOMASS_files/figure-html/summaryByPlot-1.png)

Finally, the last element (`$AGB_simu`) of the
[`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
output is a matrix containing the simulated tree AGB values (in rows)
for each iteration of the Monte Carlo procedure (in columns).

## Provide your own AGB allometry

### Default allometry or user allometry?

For tropical forests, users can choose to model AGB allometry using eqn
4 of [Chave et
al. (2014)](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.12629).
In that case you do not need to provide a value to the `allometry`
argument, default value being `"chave2014"`. Hence any code written
using previous versions of BIOMASS remains valid if you update to v3.1
and above. Note that the entire BIOMASS vignette uses Chave’s AGB
allometry equation.

However, if you have an allometry dataset (*e.g.* TLS measurements,
destructive AGB) you can use it to calibrate the allometric model, then
use the calibrated model as the allometry equation in `AGBmonteCarlo`.
The error due uncertainty in model parameters will still be propagated
by the function. The easiest way is to pass to the `allometry` argument
the output of a `brms` fit. Two examples are shown below.

### First example

[Takoudjou et al. 2017 MEE](https://doi.org/10.1111/2041-210X.12933)
data: calibrate user volume allometry with TLS data (DBH, H, volume)
using brms, compute volume with destructive data (DBH, H), and compare
with destructive volume from the dataset.

``` r

#1) LOAD DATA
tree_data_raw <- readxl::read_excel("saved_data/database_takoudjou_paper.xls", sheet = 2)
tree_data <- tree_data_raw %>% 
  select(c('ID', 'Genus', 'Species', 'DBH_dest', 'H_tot_dest', 'Destructive total volume',
           'DBH_L', 'Hauteur_L', 'TLS edited total volume_5')) %>%
  mutate_at(c('DBH_dest', 'H_tot_dest', 'Destructive total volume',
              'DBH_L', 'Hauteur_L', 'TLS edited total volume_5'), as.numeric) %>%
  rename(Vol_tls = 'TLS edited total volume_5') %>%
  rename(Vol_dest = 'Destructive total volume')

#2) CALIBRATE VOLUME ALLOMETRY WITH TLS DATA
tree_data_allom <- tree_data %>%
  select(c('ID', 'DBH_L', 'Hauteur_L', 'Vol_tls'))

# plot predictor vs. predicted
tree_data_allom$predictor <- log(tree_data_allom$DBH_L^2 * tree_data_allom$Hauteur_L) 
tree_data_allom$logV <- log(tree_data_allom$Vol_tls)
ggplot(tree_data_allom, aes(x=predictor, y=logV)) +
  geom_point()+
  theme_minimal()
```

![](Vignette_BIOMASS_files/figure-html/user_allom_takoudjou_ex-1.png)

``` r


# write allometry equation as formula
allom_formula <- bf(logV ~ a + b * log(Hauteur_L * DBH_L^2), a ~ 1, b ~ 1, nl = TRUE)

# model fit & checks
fit_allom <- brm(formula = allom_formula, 
                 data = tree_data_allom,
                 family = gaussian(),
                 thin = 10, control = list(max_treedepth = 13), iter = 3500, warmup = 1500, chains = 3, 
                 refresh = 0, silent = 2)
plot(fit_allom)
```

![](Vignette_BIOMASS_files/figure-html/user_allom_takoudjou_ex-2.png)

``` r

summary(fit_allom)
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: logV ~ a + b * log(Hauteur_L * DBH_L^2) 
#>          a ~ 1
#>          b ~ 1
#>    Data: tree_data_allom (Number of observations: 61) 
#>   Draws: 3 chains, each with iter = 3500; warmup = 1500; thin = 10;
#>          total post-warmup draws = 600
#> 
#> Regression Coefficients:
#>             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> a_Intercept   -10.19      0.34   -10.84    -9.53 1.00      579      580
#> b_Intercept     1.02      0.03     0.97     1.08 1.00      602      588
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.42      0.04     0.35     0.52 1.00      500      568
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
pp_check(fit_allom, ndraws = 150)
```

![](Vignette_BIOMASS_files/figure-html/user_allom_takoudjou_ex-3.png)

``` r


#3) VALIDATION WITH DESTRUCTIVE VOLUME
# compute volume with allometry uncertainty only 
predV <- AGBmonteCarlo(n = 1000,
                         D = tree_data$DBH_dest, Dpropag = NULL,
                         H = tree_data$H_tot_dest, errH = 0,
                         fitted_allom = fit_allom, var_in_data = c("logV", "H", "D"),
                         volume_allom = T, return_volume = T
)

# plot predicted Volume vs observed destrucTive Volume
tree_data_pred <- tree_data %>%
  mutate(predV_median = predV$V_simu_per_stem$medV,
         lower_CI_V = predV$V_simu_per_stem$lower_CI_V,
         upper_CI_V = predV$V_simu_per_stem$upper_CI_V)

ggplot(tree_data_pred, aes(x= Vol_dest, y=predV_median)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = "grey")+
  theme_minimal()+
  geom_segment(aes(x = Vol_dest, y = lower_CI_V, 
                   xend = Vol_dest, yend = upper_CI_V))+
  geom_point()+
  theme(legend.position = "none")+
  xlab("Observed destructive Volume (m^3)")+
  ylab("Predicted Volume (m^3)")
```

![](Vignette_BIOMASS_files/figure-html/user_allom_takoudjou_ex-4.png)

### Second example

[Schepaschenko et al. 2017
PANGAEA](https://doi.pangaea.de/10.1594/PANGAEA.871491150) data: select
one species (P. *sylvestris* L.) in one biome (Temperate Grasslands,
Savannas, and Shrublands), dataset partition for calibration (70%) and
validation (30%), calibrate AGB allometry with calibration data (DBH, H,
destructive AGB) using brms, compute AGB for validation data (DBH, H)
and compare with observed AGB.

``` r

#1) LOAD & PREPARE DATA
tree_data_raw <- suppressWarnings(readxl::read_xlsx("saved_data/Biomass_tree_DB_Scientific_data_2017.xlsx"))
tree_data <- tree_data_raw %>% 
  select(c('ID', 'Species', 'DBH', 'H tree', 'Pabo', 'Location', 'Latitude', 'Longitude',
           'Ecoregion', 'ID_Plot')) %>%
  rename(H = 'H tree') %>%
  rename(D = DBH) %>%
  rename(AGB = Pabo) %>%
  filter(Species == "Pinus sylvestris L.") %>%
  filter(!str_starts(Ecoregion, "808"))  %>%
  filter(D >=5) %>%  
  filter(!is.na(H)) %>%
  filter(!is.na(AGB)) %>%
  mutate(logAGB = log(AGB))

# get wood density
woodDens <- getWoodDensity(genus = "Pinus", species = "Pinus sylvestris L.")
#> Your taxonomic table contains 1 taxa
tree_data$WD <- woodDens$meanWD
tree_data$sdWD <- woodDens$sdWD

#2) SPATIAL PARTITION OF THE DATASET for calibration & validation
set.seed(5678)
nb_cluster <- 4
cluster <- stats::kmeans(x = as.matrix(tree_data[,c('Latitude', 'Longitude')]), centers = nb_cluster)
plot(tree_data$Latitude, tree_data$Longitude, col = rainbow(nb_cluster)[cluster$cluster])
```

![](Vignette_BIOMASS_files/figure-html/user_allom_Schepaschenko_ex-1.png)

``` r

table(cluster$cluster)
#> 
#>   1   2   3   4 
#> 181 253 117 261

# choosing cluster 2 for validation
sum(table(cluster$cluster)[c(2)])/sum(table(cluster$cluster))
#> [1] 0.3115764
tree_data$cluster <- cluster$cluster
tree_data_cal <- tree_data %>%
  filter(cluster != 2)
tree_data_val <- tree_data %>%
  filter(cluster == 2)

#3) CALIBRATE AGB ALLOMETRY WITH CALIBRATION DATA
# plot predictor vs. predicted
tree_data_cal$predictor <- log(tree_data_cal$D^2 * tree_data_cal$H * tree_data_cal$WD) 
ggplot(tree_data_cal, aes(x=predictor, y=logAGB, colour = as.factor(cluster))) +
  geom_point()+
  theme_minimal()
```

![](Vignette_BIOMASS_files/figure-html/user_allom_Schepaschenko_ex-2.png)

``` r


# write allometry equation as formula
allom_formula <- bf(logAGB ~ a + b * log(WD * H * D^2), a ~ 1, b ~ 1, nl = TRUE)

# model fit & checks
fit_allom <- brm(formula = allom_formula, 
                 data = tree_data_cal,
                 family = gaussian(),
                 thin = 10, control = list(max_treedepth = 13), iter = 3500, warmup = 1500, chains = 3, 
                 refresh = 0, silent = 2)
plot(fit_allom)
```

![](Vignette_BIOMASS_files/figure-html/user_allom_Schepaschenko_ex-3.png)

``` r

summary(fit_allom)
#>  Family: gaussian 
#>   Links: mu = identity 
#> Formula: logAGB ~ a + b * log(WD * H * D^2) 
#>          a ~ 1
#>          b ~ 1
#>    Data: tree_data_cal (Number of observations: 559) 
#>   Draws: 3 chains, each with iter = 3500; warmup = 1500; thin = 10;
#>          total post-warmup draws = 600
#> 
#> Regression Coefficients:
#>             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> a_Intercept    -2.61      0.05    -2.70    -2.51 1.01      611      487
#> b_Intercept     0.92      0.01     0.91     0.94 1.01      615      516
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.24      0.01     0.22     0.25 1.00      587      547
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
pp_check(fit_allom, ndraws = 150)
```

![](Vignette_BIOMASS_files/figure-html/user_allom_Schepaschenko_ex-4.png)

``` r


#4) VALIDATION
# compute AGB with allometry uncertainty only for validation data
predAGB <- AGBmonteCarlo(n = 1000,
                         D = tree_data_val$D, Dpropag = NULL,
                         H = tree_data_val$H, errH = 0,
                         WD = tree_data_val$WD, errWD = 0,
                         fitted_allom = fit_allom, var_in_data = c("logAGB", "WD", "H", "D")
)

# plot predicted AGB vs. observed AGB
tree_data_val_pred <- tree_data_val %>%
  mutate(predAGB_median = predAGB$AGB_simu_per_stem$medAGB,
         lower_CI_AGB = predAGB$AGB_simu_per_stem$lower_CI_AGB,
         upper_CI_AGB = predAGB$AGB_simu_per_stem$upper_CI_AGB)

ggplot(tree_data_val_pred, aes(x=AGB, y=predAGB_median, colour = as.factor(Location))) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = "grey")+
  scale_color_brewer(palette = "Dark2")+
  theme_minimal()+
  geom_segment(aes(x = AGB, y = lower_CI_AGB, xend = AGB, yend = upper_CI_AGB, colour = Location))+
  geom_point()+
  #theme(legend.position = "none")+
  xlab("Observed AGB (Mg)")+
  ylab("Predicted AGB (Mg)")
```

![](Vignette_BIOMASS_files/figure-html/user_allom_Schepaschenko_ex-5.png)

## Some tricks

### Mixing measured and estimated height values

If you want to use a mix of directly-measured height and of estimated
ones, you can proceed as follows:

1.  Build a vector of H and RSE where we assume an error of 0.5 m on
    directly measured trees

``` r

# NouraguesHD contains 163 trees that were not measured
NouraguesHD$Hmix <- NouraguesHD$H
NouraguesHD$RSEmix <- 0.5
filt <- is.na(NouraguesHD$Hmix)
NouraguesHD$Hmix[filt] <- retrieveH(NouraguesHD$D, model = HDmodel)$H[filt]
NouraguesHD$RSEmix[filt] <- HDmodel$RSE
```

2.  Apply the AGBmonteCarlo by setting the height values and their
    errors (which depend on whether the trees were directly measured or
    estimated)

``` r

wd <- getWoodDensity(NouraguesHD$genus, NouraguesHD$species)
resultMC <- AGBmonteCarlo(
  D = NouraguesHD$D, WD = wd$meanWD, errWD = wd$sdWD,
  H = NouraguesHD$Hmix, errH = NouraguesHD$RSEmix,
  Dpropag = "chave2004"
)
summaryByPlot(AGB_val = resultMC$AGB_simu, plot = NouraguesHD$plotId, drawPlot = TRUE)
```

### Building bayesian Height-Diameter models

Since version 2.2.6, BIOMASS enables to build bayesian H-D models using
the [brms package](https://paulbuerkner.com/brms/).

As compiling the Stan programme may take some time the first time
around, we recommend setting the ‘useCache’ argument to TRUE in the
[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)
function. This will save the model as a .rds file in the defined cache
path, meaning that next time, the model will simply be loaded and
updated, bypassing the compilation stage.

``` r

# First, define the user cache path
createCache("the_path_to_your_cache_folder") 
# Or 
options("BIOMASS.cache" = "the_path_to_your_cache_folder")

brm_model <- modelHD(
  D = NouraguesHD$D, H = NouraguesHD$H,
  method = "log2",
  bayesian = TRUE, useCache = TRUE)

H_brm_model <- retrieveH(
  D = NouraguesTrees$D,
  model = brm_model)
NouraguesTrees$H_model <-  H_brm_model$H
```

If the **Michaelis-Menten or Weibull methods are used**, you must pay
close attention to the returned models as the algorithm is likely to
identify numerous **local minima for the estimated parameters**. Here is
an example of the Weibull method being used with the default arguments:

``` r

brm_model <- modelHD(
  D = NouraguesHD$D, H = NouraguesHD$H,
  method = "weibull",
  bayesian = TRUE, useCache = TRUE)

plot(brm_model$model)
```

![brm_model output](img/weibull_plot.png)

brm_model output

As one can see, the two first parameters a and b (resp. called
`b_a_Intercept` and `b_b_Intercept`) of the Weibull equation:
``` math
H=a  \cdot (1-exp(-(D/b)^c))
```
are strongly correlated. Therefore, it is strongly recommended to define
priors that do not allow for unrealistic values. To do this, we need to
examine the meanings of the Weibull parameters:

- `a` can be considered as the hypothetical maximum height, so defining
  a uniform prior between 0 and 80m seems realistic.
- `b` is the scaling parameter, so defining a uniform prior between 0
  and a high value of D (e.g. the 90th percentile) makes sense.
- `c` is the shape parameter which must be between 0 and 1

Besides, since model parameters and chain iterations are strongly
correlated, an increase of `thin`, `iter` and `warmup` is required (see
the help of brms::brm() and [this
page](https://mc-stan.org/learn-stan/diagnostics-warnings.html) for more
details).

The arguments of
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) (cited
above) can be provided to
[`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md):

``` r

weibull_priors <- c(
  set_prior(prior = "uniform(0,80)", lb = 0, ub = 80, class = "b",nlpar = "a"),
  set_prior(prior = "uniform(0,100)", lb = 0, ub = 100, class = "b",nlpar = "b"),
  set_prior(prior = "uniform(0.01,0.99)", lb = 0.01, ub = 0.99, class = "b",nlpar = "c")
)

brm_model <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H,
                     method = "weibull", bayesian = TRUE, useCache = TRUE,
                     prior = weibull_priors,
                     thin = 10, iter = 6000, warmup = 1000,
                     cores = 3 # number of cores = 3 since 3 chains are running 
)

plot(brm_model$model)
```

![brm_model output](img/weibull_plot_2.png)

brm_model output

Posterior distributions look better but the right-skewed distributions
are not yet satisfactory. So **we recommend using the log2 method when
the RSE given by lm and nls models (when bayesian = FALSE) are close**.

### Add your tricks

If you would like to share a code that might be useful to users (code
authorship will be respected), you can create a new issue on the
[BIOMASS github page](https://github.com/umr-amap/BIOMASS/issues), or
contact Dominique (<dominique.lamonica@ird.fr>).
