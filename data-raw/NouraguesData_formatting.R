#################
### Tree data ###
#################

# Data coming from https://doi.org/10.18167/DVN1/TZ1RL9
nouragues_trees <- read.csv("2023-09-29_Petit_Plateau2012.csv", fileEncoding = "ISO-8859-1")

# Replace ö (which is a non ASCI character) by o (same result when retrieving wood density)
nouragues_trees$Genus <- gsub(pattern = "ö", replacement = "o", x = nouragues_trees$Genus)
nouragues_trees$GenusFilled <- gsub(pattern = "ö", replacement = "o", x = nouragues_trees$GenusFilled)

# Filter Petit_Plateau trees
pp_trees <- nouragues_trees %>% filter(Plot=="Petit_Plateau")

# Compute diameter (cm)
pp_trees$D <- round(pp_trees$CircCorr / pi, 1)

# Removing dead pp_trees
pp_trees <- pp_trees %>% filter(CodeAlive == 1)

# Removing 5 unlocated pp_trees
pp_trees <- pp_trees %>% filter(!is.na(Xfield))

summary(pp_trees)

pp_trees[ pp_trees$Family != pp_trees$FamilyFilled , ]

# Renaming some columns
pp_trees$Family <- NULL ; pp_trees$Genus <- NULL ; pp_trees$Species <- NULL
pp_trees <- pp_trees %>% rename(Site = Plot, Plot = SubPlot, Family = FamilyFilled , Genus = GenusFilled, Species = SpeciesFilled)


# ### Computing AGB in order to select the 4 most different plot in terms of AGB
# taxo <- correctTaxo(genus = pp_trees$Genus, species = pp_trees$Species, useCache = TRUE, verbose = FALSE)
# taxo$genusCorrected[taxo$genusCorrected != pp_trees$Genus] ; pp_trees$Genus[pp_trees$Genus != taxo$genusCorrected]
# taxo$speciesCorrected[taxo$speciesCorrected != pp_trees$Species] ; pp_trees$Species[pp_trees$Species != taxo$speciesCorrected]
# 
# pp_trees$genus_corr <- taxo$genusCorrected
# pp_trees$species_corr <- taxo$speciesCorrected
# 
# data_wd <- getWoodDensity(
#   genus = pp_trees$genus_corr,
#   species = pp_trees$species_corr,
#   stand = pp_trees$plot
# )
# 
# pp_trees$WD <- data_wd$meanWD
# 
# pp_trees$AGB <- computeAGB(D = pp_trees$D, WD = pp_trees$WD, H = pp_trees$H_pred)
# 
# resultMC <- AGBmonteCarlo(D = pp_trees$D, WD = pp_trees$WD, errWD = data_wd$sdWD, HDmodel = HDmodel, Dpropag = "chave2004")
# resultMC <- summaryByPlot(resultMC$AGB_simu, pp_trees$plot)
# resultMC <- resultMC[order(resultMC$AGB), ]
# resultMC %>% ggplot(aes(x=plot, y=AGB)) + 
#   geom_pointrange(aes(ymin=Cred_2.5, ymax=Cred_97.5))

# As we have to choose 4 plots, I recommand :
# - plot 201 because of its relative coordinates (from 0 to 100) for a basic example
# - plot 204 : the highest AGB
# - plot 223 : the lowest AGB
# - plot 213 : to get a plot on the middle row and to maximize the range of the AGB

### Columns and plots selection

# Selecting desired columns
pp_trees <- pp_trees %>% select(Site, Plot, Xfield, Yfield, Family, Genus, Species, D)

# Selecting desired plots (rows)
pp_trees <- pp_trees %>% filter(Plot %in% c(201,204,213,223))

summary(pp_trees)

### pp_trees outside the plot for illustrative purpose, and with distorted species names
outside_trees <- pp_trees[c(3,12,28),]
outside_trees$Xfield <- c(51, -3.5, -4)
outside_trees$Yfield <- c(-4.5, 41.5, 67.5)
outside_trees$Species <- c("guyanense", "speciosa","guyanensis")

pp_trees <- rbind(pp_trees, outside_trees)

pp_trees <- pp_trees %>% arrange(Plot, Xfield, Yfield)

# Put the 2 outside trees (in X axis) in 4th and 6th position (for vignette kables)
pp_trees <- rbind( pp_trees[c(3:5,1,6,2),] , pp_trees[c(7:nrow(pp_trees)),] )


pp_trees %>% write.csv(file="NouraguesTrees.csv", row.names = F, fileEncoding = "UTF-8")

NouraguesTrees <- pp_trees
usethis::use_data(NouraguesTrees, overwrite = TRUE)


#########################
### Plots coordinates ###
#########################

# Data coming from https://doi.org/10.18167/DVN1/HXKS4E

# 2 datasets will figure for plot coordinates : 
# - one with the coordinates of the 4 selected plot
# - one with 10 simulated measurements of each corner for the plot 201

nouragues_coords <- read.table("guyafor_format_data.csv", T, ",") %>% rename(Subplot = SubPlot)

### 4 plots dataset
# Filter desired plots
sub_pp_coords <- nouragues_coords %>% 
  filter(Subplot %in% c(201,204,213,223)) %>% 
  arrange(Subplot, Xfield, Yfield)

sub_pp_coords <- sub_pp_coords %>% rename(Site = Plot, Plot = Subplot)

# Get long lat coordinates
sub_pp_coords[c("Long","Lat")] <- as.data.frame( proj4::project(sub_pp_coords[c("Xutm","Yutm")], proj = "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs", inverse = TRUE) )

# Select desired columns
sub_pp_coords <- sub_pp_coords[c("Site","Plot", "Xfield", "Yfield", "Xutm", "Yutm", "Long", "Lat")]

sub_pp_coords %>% write.csv("Coords/NouraguesCoords.csv", row.names = FALSE, fileEncoding = "UTF-8")

usethis::use_data(NouraguesCoords, overwrite = TRUE)

### Simulating 10 measurements of each corner with outliers, for the distorted 201 plot
plot201_coords <- sub_pp_coords %>% filter(Plot == 201)

sim_coords <- BIOMASS::bilinear_interpolation(
  coord = data.frame(
    x_rel = c(-5, 0, 105, 100),
    y_rel = c(-5, 105, -5, 100)), 
  from_corner_coord = plot201_coords[c("Xfield","Yfield")] ,
  to_corner_coord = plot201_coords[c("Xutm","Yutm")] )

sim_coords <- cbind(plot201_coords[c("Site","Plot","Xfield","Yfield")] , sim_coords)

sim_coords <- do.call(rbind,lapply(1:10 , function(x) sim_coords))
set.seed(1)
sim_coords <- sim_coords %>% mutate(
  Xutm = Xutm + rnorm(40 , 0 , 5) , 
  Yutm = Yutm + rnorm(40 , 0 , 5)
)

sim_coords[c("Long","Lat")] <- as.data.frame( proj4::project(sim_coords[c("Xutm","Yutm")], proj = "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs", inverse = TRUE) )

sim_coords <- sim_coords %>% arrange(Xfield,Yfield)

sim_coords %>% write.csv(file = "NouraguesPlot201Coords.csv", row.names = FALSE, fileEncoding = "UTF-8")

usethis::use_data(NouraguesPlot201, overwrite = TRUE)