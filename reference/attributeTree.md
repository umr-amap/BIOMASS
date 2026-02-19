# Attribute trees to subplots

**\[deprecated\]** `attributeTree()` is now deprecated. The tree
attribution to subplots is now done by the
[`divide_plot()`](https://umr-amap.github.io/BIOMASS/reference/divide_plot.md)
function Please see the vignette
`Spatialized trees and forest stand metrics with BIOMASS`

Function to attribute the trees on each subplot, the trees that are at
the exterior of the subplot will be marked as NA

## Usage

``` r
attributeTree(xy, plot, coordAbs)
```

## Arguments

- xy:

  The coordinates of the trees for each plot

- plot:

  The label of the plot (same length as the number of rows of `xy`)

- coordAbs:

  Output of the function
  [`cutPlot()`](https://umr-amap.github.io/BIOMASS/reference/cutPlot.md)

## Value

A vector with the code of the subplot for each trees, the code will be
`plot_X_Y`. `X` and `Y` are the coordinate where the tree is inside the
plot in regards to the corresponding subplot.

## Author

Arthur PERE

## Examples

``` r
# Trees relative coordinates
xy <- data.frame(x = runif(200, min = 0, max = 200), y = runif(200, min = 0, max = 200))


# cut the plot in multiple part
coord <- data.frame(X = rep(c(0, 200, 0, 200), 2), Y = rep(c(0, 0, 200, 200), 2))
coord[1:4, ] <- coord[1:4, ] + 5000
coord[5:8, ] <- coord[5:8, ] + 6000
corner <- rep(c(1, 2, 4, 3), 2)
plot <- rep(c("plot1", "plot2"), each = 4)

cut <- cutPlot(coord, plot, corner, gridsize = 100, dimX = 200, dimY = 200)
#> Warning: 'cutPlot()' has been replaced by `divide_plot()` and will be removed in the next version.
#> Please see the vignette `Spatialized trees and forest stand metrics with BIOMASS`


# Assign a plot to 200 trees
plot <- rep(c("plot1", "plot2"), 100)

# attribute trees to subplots
attributeTree(xy, plot, cut)
#> Warning: 'attributeTree()' is deprecated and will be removed in the next version. The tree attribution to subplots is now done by the `divide_plot()` function
#> Please see the vignette `Spatialized trees and forest stand metrics with BIOMASS`
#>   [1] "plot1_0_0" "plot2_0_0" "plot1_0_0" "plot2_1_1" "plot1_1_1" "plot2_1_1"
#>   [7] "plot1_1_0" "plot2_1_0" "plot1_0_1" "plot2_0_0" "plot1_1_0" "plot2_0_1"
#>  [13] "plot1_0_0" "plot2_0_1" "plot1_1_1" "plot2_0_1" "plot1_1_0" "plot2_1_0"
#>  [19] "plot1_0_1" "plot2_0_1" "plot1_0_0" "plot2_0_1" "plot1_1_0" "plot2_1_1"
#>  [25] "plot1_0_1" "plot2_1_1" "plot1_0_1" "plot2_0_1" "plot1_1_0" "plot2_0_0"
#>  [31] "plot1_1_1" "plot2_1_1" "plot1_0_0" "plot2_0_0" "plot1_1_0" "plot2_1_1"
#>  [37] "plot1_1_0" "plot2_0_0" "plot1_0_0" "plot2_1_0" "plot1_1_1" "plot2_0_1"
#>  [43] "plot1_1_1" "plot2_0_0" "plot1_0_0" "plot2_1_1" "plot1_0_0" "plot2_1_1"
#>  [49] "plot1_1_1" "plot2_1_1" "plot1_0_1" "plot2_1_1" "plot1_0_0" "plot2_0_1"
#>  [55] "plot1_0_0" "plot2_1_1" "plot1_0_0" "plot2_0_0" "plot1_1_1" "plot2_1_0"
#>  [61] "plot1_1_0" "plot2_0_0" "plot1_1_1" "plot2_0_0" "plot1_1_0" "plot2_0_1"
#>  [67] "plot1_0_0" "plot2_0_1" "plot1_0_1" "plot2_0_1" "plot1_1_1" "plot2_1_0"
#>  [73] "plot1_0_0" "plot2_1_1" "plot1_0_1" "plot2_1_0" "plot1_0_0" "plot2_1_1"
#>  [79] "plot1_1_0" "plot2_0_0" "plot1_0_1" "plot2_0_0" "plot1_1_0" "plot2_1_1"
#>  [85] "plot1_0_1" "plot2_1_1" "plot1_1_1" "plot2_0_0" "plot1_1_1" "plot2_1_0"
#>  [91] "plot1_0_0" "plot2_0_0" "plot1_1_1" "plot2_1_0" "plot1_0_1" "plot2_0_1"
#>  [97] "plot1_0_0" "plot2_0_0" "plot1_0_1" "plot2_1_0" "plot1_0_1" "plot2_1_1"
#> [103] "plot1_1_1" "plot2_0_1" "plot1_0_0" "plot2_1_0" "plot1_1_0" "plot2_0_0"
#> [109] "plot1_0_1" "plot2_0_0" "plot1_1_1" "plot2_1_1" "plot1_1_0" "plot2_0_0"
#> [115] "plot1_1_1" "plot2_0_0" "plot1_1_0" "plot2_1_1" "plot1_0_1" "plot2_1_1"
#> [121] "plot1_0_0" "plot2_0_0" "plot1_1_0" "plot2_0_1" "plot1_1_1" "plot2_0_0"
#> [127] "plot1_1_0" "plot2_1_0" "plot1_1_1" "plot2_1_1" "plot1_0_1" "plot2_1_1"
#> [133] "plot1_1_1" "plot2_1_0" "plot1_0_0" "plot2_0_1" "plot1_0_0" "plot2_1_0"
#> [139] "plot1_0_0" "plot2_1_1" "plot1_1_0" "plot2_1_0" "plot1_1_0" "plot2_0_1"
#> [145] "plot1_1_0" "plot2_1_1" "plot1_0_1" "plot2_0_0" "plot1_0_1" "plot2_0_0"
#> [151] "plot1_0_1" "plot2_0_0" "plot1_0_0" "plot2_0_1" "plot1_1_0" "plot2_0_0"
#> [157] "plot1_0_1" "plot2_1_0" "plot1_0_1" "plot2_1_0" "plot1_1_0" "plot2_1_1"
#> [163] "plot1_0_0" "plot2_0_1" "plot1_1_1" "plot2_0_1" "plot1_0_0" "plot2_1_1"
#> [169] "plot1_1_1" "plot2_1_0" "plot1_0_0" "plot2_1_1" "plot1_0_0" "plot2_1_1"
#> [175] "plot1_0_0" "plot2_0_0" "plot1_0_0" "plot2_0_1" "plot1_1_0" "plot2_0_1"
#> [181] "plot1_0_0" "plot2_1_0" "plot1_0_1" "plot2_1_0" "plot1_0_0" "plot2_1_0"
#> [187] "plot1_1_1" "plot2_1_1" "plot1_0_0" "plot2_0_1" "plot1_0_0" "plot2_1_0"
#> [193] "plot1_1_1" "plot2_0_0" "plot1_1_0" "plot2_1_1" "plot1_1_0" "plot2_0_1"
#> [199] "plot1_1_1" "plot2_0_0"
```
