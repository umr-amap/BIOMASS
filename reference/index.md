# Package index

## Estimate above ground biomass (ABG) and its uncertainty

The following functions aim to retrieve compulsory parameters such as
wood density and tree height, which lead to the final estimate of the
AGB and its uncertainty

### Correct tree taxonomy

- [`correctTaxo()`](https://umr-amap.github.io/BIOMASS/reference/correctTaxo.md)
  : Correct trees taxonomy
- [`getTaxonomy()`](https://umr-amap.github.io/BIOMASS/reference/getTaxonomy.md)
  : Retrieve trees taxonomy

### Estimate wood density

- [`getWoodDensity()`](https://umr-amap.github.io/BIOMASS/reference/getWoodDensity.md)
  : Estimating wood density

### Estimate tree height

- [`modelHD()`](https://umr-amap.github.io/BIOMASS/reference/modelHD.md)
  : Fitting height-diameter models
- [`retrieveH()`](https://umr-amap.github.io/BIOMASS/reference/retrieveH.md)
  : Retrieving tree height from models

### Estimate AGB

- [`computeAGB()`](https://umr-amap.github.io/BIOMASS/reference/computeAGB.md)
  : Computing tree above ground biomass (AGB)

### Propagate uncertainties

- [`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)
  : Propagating above ground biomass (AGB) or carbon (AGC) errors to the
  stand level

### Summarise at stand level

- [`summaryByPlot()`](https://umr-amap.github.io/BIOMASS/reference/summaryByPlot.md)
  : Summarise by plot the posterior distribution of AGB values

## Spatialized trees and forest stand metrics

### Check plot coordinates

- [`check_plot_coord()`](https://umr-amap.github.io/BIOMASS/reference/check_plot_coord.md)
  : Check coordinates of plot corners and trees

### Divide plot into subplots

- [`divide_plot()`](https://umr-amap.github.io/BIOMASS/reference/divide_plot.md)
  : Divides one ore more plots into subplots

- [`subplot_summary()`](https://umr-amap.github.io/BIOMASS/reference/subplot_summary.md)
  : Summarise and display tree information by subplot

## Other functions

- [`bilinear_interpolation()`](https://umr-amap.github.io/BIOMASS/reference/bilinear_interpolation.md)
  : Generalized bilinear interpolation of coordinates
- [`procrust()`](https://umr-amap.github.io/BIOMASS/reference/procrust.md)
  : Procrust analysis

- [`latlong2UTM()`](https://umr-amap.github.io/BIOMASS/reference/latlong2UTM.md)
  : Translate the long lat coordinate in UTM coordinate

### Retrieve Feldpausch regions

- [`computeFeldRegion()`](https://umr-amap.github.io/BIOMASS/reference/computeFeldRegion.md)
  : Retrieving Feldpausch regions

### Predict tree height from a HD-model

- [`predictHeight()`](https://umr-amap.github.io/BIOMASS/reference/predictHeight.md)
  : Tree height predictions

### Manage user cache

- [`cacheManager()`](https://umr-amap.github.io/BIOMASS/reference/cacheManager.md)
  : Function that return a possibly cached file, transparently
  downloading it if missing
- [`cachePath()`](https://umr-amap.github.io/BIOMASS/reference/cachePath.md)
  : Function used to build a file path based on a cache folder
- [`createCache()`](https://umr-amap.github.io/BIOMASS/reference/createCache.md)
  : Function used to create or activate a permanent cache.
- [`clearCache()`](https://umr-amap.github.io/BIOMASS/reference/clearCache.md)
  : Function to clear cache content and possibly remove it

## Documentation

### Methods used for modeling height-diameter relationship

- [`loglogFunction()`](https://umr-amap.github.io/BIOMASS/reference/HDmethods.md)
  [`michaelisFunction()`](https://umr-amap.github.io/BIOMASS/reference/HDmethods.md)
  [`weibullFunction()`](https://umr-amap.github.io/BIOMASS/reference/HDmethods.md)
  : HDmethods

## Datasets

The four datasets used for examples and vignettes

- [`NouraguesHD`](https://umr-amap.github.io/BIOMASS/reference/NouraguesHD.md)
  : Height-Diameter data
- [`NouraguesCoords`](https://umr-amap.github.io/BIOMASS/reference/NouraguesCoords.md)
  : Nouragues plot coordinates
- [`NouraguesPlot201`](https://umr-amap.github.io/BIOMASS/reference/NouraguesPlot201.md)
  : Nouragues plot 201 coordinates
- [`NouraguesTrees`](https://umr-amap.github.io/BIOMASS/reference/NouraguesTrees.md)
  : Nouragues forest dataset

## Deprecated functions

These functions are deprecated and will be removed in the version 3.0

- [`attributeTree()`](https://umr-amap.github.io/BIOMASS/reference/attributeTree.md)
  : Attribute trees to subplots
- [`attributeTreeCoord()`](https://umr-amap.github.io/BIOMASS/reference/attributeTreeCoord.md)
  : Attribute GPS coordinates to trees
- [`correctCoordGPS()`](https://umr-amap.github.io/BIOMASS/reference/correctCoordGPS.md)
  : Correct the GPS coordinates
- [`cutPlot()`](https://umr-amap.github.io/BIOMASS/reference/cutPlot.md)
  : Divides one or more plots into subplots
- [`numberCorner()`](https://umr-amap.github.io/BIOMASS/reference/numberCorner.md)
  : Get the UTM coordinates with the corner of the plot
