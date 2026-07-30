# Divides one ore more plots into subplots

This function divides a plot (or several plots) into subplots in the
relative coordinates system, and returns the coordinates of subplot
corners.

## Usage

``` r
divide_plot(
  corner_data,
  rel_coord,
  proj_coord = NULL,
  longlat = NULL,
  grid_size,
  grid_tol = 0.1,
  origin = NULL,
  tree_data = NULL,
  tree_coords = NULL,
  corner_plot_ID = NULL,
  tree_plot_ID = NULL,
  sd_coord = NULL,
  n = 100
)
```

## Arguments

- corner_data:

  A data frame, data frame extension, containing the plot corner
  coordinates. Typically, the output `$corner_coord` of the
  [`check_plot_coord()`](https://umr-amap.github.io/BIOMASS/reference/check_plot_coord.md)
  function.

- rel_coord:

  A character vector of length 2, specifying the column names (resp.
  x, y) of the corner relative coordinates.

- proj_coord:

  (optional, if longlat is not provided) A character vector of length 2,
  specifying the column names (resp. x, y) of the corner projected
  coordinates.

- longlat:

  (optional, if proj_coord is not provided) A character vector of length
  2, specifying the column names of the corner geographic coordinates
  (long,lat).

- grid_size:

  A vector indicating the dimensions of grid cells (resp. X and Y
  dimensions). If only one value is given, grid cells will be considered
  as squares.

- grid_tol:

  A numeric between (0;1) corresponding to the percentage of the plot
  area allowed to be excluded from the plot division (when grid_size
  doesn't match exactly plot dimensions).

- origin:

  Alignment of the subplot grid, based on relative coordinates. If NULL
  (default), the grid is aligned to the origin corner of the relative
  coordinates. Alternatively provide a numeric vector of length 2,
  specifying the relative coordinates to which the grid should be
  aligned. This option is especially useful when `grid_size` doesn't
  match exactly plot dimensions.

- tree_data:

  A data frame containing tree relative coordinates and other optional
  tree metrics (one row per tree).

- tree_coords:

  A character vector of length 2, specifying the column names of the
  relative coordinates of the trees.

- corner_plot_ID:

  If dealing with several plots: a vector indicating plot IDs for
  corners.

- tree_plot_ID:

  If dealing with several plots: a vector indicating tree plot IDs.

- sd_coord:

  used to propagate GPS measurements uncertainties to the subplot
  polygon areas and the ref_raster footprint in
  [`subplot_summary()`](https://umr-amap.github.io/BIOMASS/reference/subplot_summary.md).
  See Details.

- n:

  used to propagate GPS measurements uncertainties: the number of
  iterations to be used (as in
  [`AGBmonteCarlo()`](https://umr-amap.github.io/BIOMASS/reference/AGBmonteCarlo.md)).
  Cannot be smaller than 50 or larger than 1000.

## Value

Returns a list containing:

- \$sub_corner_coord: a data-frame containing as many rows as there are
  corners corresponding to the subplots, and the following columns :

  - `plot_ID`: If dealing with multiple plots: the plot code, else, a
    column containing an empty character

  - `subplot_ID`: The automatically generated subplot code, using the
    following rule : subplot_X_Y

  - `x_rel` and `y_rel` : the relative X-axis and Y-axis coordinates of
    subplots corners.

  - `x_proj` and `y_proj` : if proj_coord is provided, the projected
    X-axis and Y-axis coordinates of subplots corners

- \$tree_data: the tree_data argument with the subplot_ID of each tree
  in the last column

- \$UTM_code: if 'longlat' is provided, a data.frame containing the UTM
  code of the corner GPS coordinates for each plot

- \$simu_coord: if sd_coord is provided, a list of n data-tables
  containing the simulated coordinates

## Details

If corner coordinates in the projected coordinate system are provided
(proj_coord), projected coordinates of subplot corners are calculated by
a bilinear interpolation in relation with relative coordinates of plot
corners. Be aware that this bilinear interpolation only works if the
plot in the relative coordinates system is rectangular (ie, has 4 right
angles).

In order to propagate GPS measurement uncertainties, the `sd_coord`
argument has to be provided and must contains the average standard
deviation of the GPS measurements for each corner on the X and Y axes
(typically, the output \$sd_coord of the
[`check_plot_coord()`](https://umr-amap.github.io/BIOMASS/reference/check_plot_coord.md)
function). If corner_data contains only one plot, `sd_coord` must be a
numeric. If dealing with several plot, `sd_coord` must be a data frame
of two columns named 'plot_ID' and 'sd_coord' containing respectively
the plot IDs and the previous metric (again, see the output \$sd_coord
of the
[`check_plot_coord()`](https://umr-amap.github.io/BIOMASS/reference/check_plot_coord.md)
function).

## Author

Arthur PERE, Arthur BAILLY, John L. GODLEE

## Examples

``` r
# One plot with repeated measurements of each corner
data("NouraguesPlot201")
check_plot201 <- check_plot_coord(
  corner_data = NouraguesPlot201,
  proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
  trust_GPS_corners = TRUE, draw_plot = FALSE)
subplots_201 <- divide_plot(
  corner_data = check_plot201$corner_coord, 
  rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"),
  grid_size = 50)
subplots_201
#> $sub_corner_coord
#>    plot_ID  subplot_ID x_rel y_rel   x_proj   y_proj
#> 1          subplot_0_0     0     0 313005.7 451723.2
#> 2          subplot_0_0    50     0 312981.3 451676.7
#> 3          subplot_0_0    50    50 313028.4 451650.5
#> 4          subplot_0_0     0    50 313053.1 451694.5
#> 5          subplot_1_0    50     0 312981.3 451676.7
#> 6          subplot_1_0   100     0 312956.9 451630.2
#> 7          subplot_1_0   100    50 313003.6 451606.4
#> 8          subplot_1_0    50    50 313028.4 451650.5
#> 9          subplot_0_1     0    50 313053.1 451694.5
#> 10         subplot_0_1    50    50 313028.4 451650.5
#> 11         subplot_0_1    50   100 313075.4 451624.2
#> 12         subplot_0_1     0   100 313100.5 451665.9
#> 13         subplot_1_1    50    50 313028.4 451650.5
#> 14         subplot_1_1   100    50 313003.6 451606.4
#> 15         subplot_1_1   100   100 313050.2 451582.6
#> 16         subplot_1_1    50   100 313075.4 451624.2
#> 

# Assigning trees to subplots
data("NouraguesTrees")
plot201_trees <- NouraguesTrees[NouraguesTrees$Plot == 201,]
subplots_201 <- suppressWarnings(
  divide_plot(
    corner_data = check_plot201$corner_coord, 
    rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"),
    grid_size = 50,
    tree_data = plot201_trees, tree_coords = c("Xfield","Yfield")))
head(subplots_201$sub_corner_coord)
#>   plot_ID  subplot_ID x_rel y_rel   x_proj   y_proj
#> 1         subplot_0_0     0     0 313005.7 451723.2
#> 2         subplot_0_0    50     0 312981.3 451676.7
#> 3         subplot_0_0    50    50 313028.4 451650.5
#> 4         subplot_0_0     0    50 313053.1 451694.5
#> 5         subplot_1_0    50     0 312981.3 451676.7
#> 6         subplot_1_0   100     0 312956.9 451630.2
head(subplots_201$tree_data)
#>            Site Plot x_rel y_rel        Family               Genus     Species
#> 1 Petit_Plateau  201   0.0  31.5   Burseraceae             Protium surinamense
#> 2 Petit_Plateau  201   0.1  75.2 Anacardiaceae            Tapirira  guianensis
#> 3 Petit_Plateau  201   0.2  27.6 Lecythidaceae Indet.Lecythidaceae      Indet.
#> 4 Petit_Plateau  201  -4.0  67.5 Euphorbiaceae          Conceveiba  guyanensis
#> 5 Petit_Plateau  201   0.3  39.9   Burseraceae             Protium  altissimum
#> 6 Petit_Plateau  201  -3.5  41.5 Euphorbiaceae               Mabea    speciosa
#>      D plot_ID  subplot_ID
#> 1 11.0         subplot_0_0
#> 2 74.4         subplot_0_1
#> 3 25.4         subplot_0_0
#> 4 10.0                <NA>
#> 5 18.9         subplot_0_0
#> 6 10.0                <NA>

# When grid dimensions (40m x 40m) don't fit perfectly plot dimensions
# an origin at (10 ; 10) will center the grid
# \donttest{
  divide_plot(
    corner_data = check_plot201$corner_coord, 
    rel_coord = c("x_rel","y_rel"),
    grid_size = c(40,40),
    grid_tol = 0.4,
    origin = c(10,10)
 )
#> Warning: 
#> The x-dimension of the plot is not a multiple of the x-dimension of the grid size and origin offset
#> Warning: 
#> The y-dimension of the plot is not a multiple of the y-dimension of the grid size and origin offset
#> $sub_corner_coord
#>    plot_ID  subplot_ID x_rel y_rel
#> 1          subplot_0_0    10    10
#> 2          subplot_0_0    50    10
#> 3          subplot_0_0    50    50
#> 4          subplot_0_0    10    50
#> 5          subplot_1_0    50    10
#> 6          subplot_1_0    90    10
#> 7          subplot_1_0    90    50
#> 8          subplot_1_0    50    50
#> 9          subplot_0_1    10    50
#> 10         subplot_0_1    50    50
#> 11         subplot_0_1    50    90
#> 12         subplot_0_1    10    90
#> 13         subplot_1_1    50    50
#> 14         subplot_1_1    90    50
#> 15         subplot_1_1    90    90
#> 16         subplot_1_1    50    90
#> 
# }

# Dealing with multiple plots
data("NouraguesCoords")
nouragues_subplots <- suppressWarnings(
  divide_plot(
    corner_data = NouraguesCoords,
    rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"),
    corner_plot_ID = "Plot",
    grid_size = 50,
    tree_data = NouraguesTrees, tree_coords =  c("Xfield","Yfield"),
    tree_plot_ID = "Plot"))
head(nouragues_subplots$sub_corner_coord)
#>   plot_ID subplot_ID x_rel y_rel   x_proj   y_proj
#> 1     201    201_0_0     0     0 313007.9 451717.2
#> 2     201    201_0_0    50     0 312984.0 451673.2
#> 3     201    201_0_0    50    50 313028.0 451649.4
#> 4     201    201_0_0     0    50 313051.8 451693.3
#> 5     201    201_1_0    50     0 312984.0 451673.2
#> 6     201    201_1_0   100     0 312960.2 451629.3
head(nouragues_subplots$tree_data)
#>            Site plot_ID x_rel y_rel        Family               Genus
#> 1 Petit_Plateau     201   0.0  31.5   Burseraceae             Protium
#> 2 Petit_Plateau     201   0.1  75.2 Anacardiaceae            Tapirira
#> 3 Petit_Plateau     201   0.2  27.6 Lecythidaceae Indet.Lecythidaceae
#> 4 Petit_Plateau     201  -4.0  67.5 Euphorbiaceae          Conceveiba
#> 5 Petit_Plateau     201   0.3  39.9   Burseraceae             Protium
#> 6 Petit_Plateau     201  -3.5  41.5 Euphorbiaceae               Mabea
#>       Species    D subplot_ID
#> 1 surinamense 11.0    201_0_0
#> 2  guianensis 74.4    201_0_1
#> 3      Indet. 25.4    201_0_0
#> 4  guyanensis 10.0       <NA>
#> 5  altissimum 18.9    201_0_0
#> 6    speciosa 10.0       <NA>
```
