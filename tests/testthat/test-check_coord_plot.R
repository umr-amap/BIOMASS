
data("NouraguesCoords")
data("NouraguesPlot201")
data("NouraguesTrees")

test_that("check_plot_coord error", {

  expect_error(check_plot_coord(proj_coord=NouraguesPlot201[c("Xutm","Yutm")], rel_coord=NouraguesPlot201[c("Xfield","Yfield")]),
               "The way in which arguments are provided to the function has changed since version 2.2.1. You now have to provide corner_data data frame and its associated coordinates variable names.")
  expect_error(check_plot_coord(corner_data = as.matrix(NouraguesPlot201), rel_coord = c("Xfield","Yfield")),
               "corner_data must be a data frame or a data frame extension")
  expect_error(check_plot_coord(corner_data = NouraguesPlot201, rel_coord = c("Xfield","Yfield")),
               "You must provide the name of at least one set of coordinates : longlat or proj_coord")
  expect_error(check_plot_coord(corner_data = NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("x","y")),
               "column names provided by rel_coord are not found in corner_data")
  expect_error(check_plot_coord(corner_data = NouraguesPlot201, proj_coord = c("xproj","yproj"), rel_coord = c("Xfield","Yfield")),
               "column names provided by proj_coord are not found in corner_data")
  expect_error(check_plot_coord(corner_data = NouraguesPlot201, longlat = c("xproj","yproj"), rel_coord = c("Xfield","Yfield")),
               "column names provided by longlat are not found in corner_data")

  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield")), "The trust_GPS_corners argument must be set to TRUE or FALSE")
  
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=F, tree_data = c(1,1)),"tree_data must a data frame or a data frame extension")
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=F, tree_data = NouraguesTrees),"You must provide the column names corresponding to the relative coordinates of tree_data using the argument `tree_coords`")
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=F, tree_data = NouraguesTrees, tree_coords = c("a","b")),"column names provided by tree_coords are not found in tree_data")
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=F, tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), prop_tree = "a"), "column name provided by prop_tree is not found in tree_data")
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=F, tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), prop_tree = "D", threshold_tree = "a"), "'threshold_tree' must be a numeric of length 1.")
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=F, tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), threshold_tree = 15), "the 'prop_tree' argument must also be provided.")
  
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, max_dist=c(10,20) ),"The max_dist argument must be of length 1")
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "Plot", draw_plot = F, max_dist = 0.1) , "increasing the distance")

  wrong_coords <- NouraguesPlot201 ; wrong_coords[1,"Xfield"] <- NA
  expect_error(check_plot_coord(wrong_coords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T), "Missing values are detected in corner relative coordinates. Please remove them and call the function again")
  wrong_coords[1,"Xfield"] <- 0 ; wrong_coords[1,"Xutm"] <- NA
  expect_error(check_plot_coord(wrong_coords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T), "Missing values are detected in corner projected coordinates. Please remove them and call the function again")
  wrong_coords[1,"Xutm"] <- 0 ; wrong_coords[1,"Long"] <- NA
  expect_error(check_plot_coord(wrong_coords, longlat = c("Long","Lat"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T), "Missing values are detected in corner longitude/latitude coordinates. Please remove them and call the function again")
  
  expect_error(suppressMessages(check_plot_coord(corner_data = NouraguesCoords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=TRUE)),
               "It seems that 'corner_data' contains several plots")
  expect_error(check_plot_coord(NouraguesCoords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "a"),"is not found in corner_data column names.")
  
  expect_error(check_plot_coord(NouraguesCoords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "Plot", tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield")), "The argument tree_plot_ID is required if plot_ID is provided.")
  
  expect_message(check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "Plot", tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"), "These ID's are found in tree_plot_ID but not in plot_ID : 201 213 223")

})

test_that("check_plot_coord outputs and outliers", {
  # with max dist equal 10
  expect_message(
    check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = F, draw_plot = F, max_dist = 10),"Be carefull, you may have GNSS measurement outliers"
  )
  outputs <- check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F, max_dist = 10)
  expect_true(is.list(outputs))
  expect_length(outputs, 5)
  expect_equal(names(outputs), c("corner_coord", "polygon", "plot_design", "outlier_corners","sd_coord"))
  expect_true(is.data.frame(outputs$corner_coord))
  expect_true(is.data.frame(outputs$outlier_corners))
  expect_type(outputs$sd_coord, "double")

  expect_equal(dim(outputs$outlier_corners), c(7,3))
  expect_equal(dim(outputs$corner_coord), c(4, 4))

  # with max dist equal 25 there isn't outliers anymore
  expect_failure(expect_warning(
    check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = F, draw_plot = F, max_dist = 25),
    "Be carefull, you may have GNSS measurement outliers"
  ))

  # with rm_outliers = TRUE
  expect_failure(expect_warning(
    check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F, max_dist = 25),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  outputs_2 <- check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F, max_dist = 15)

  expect_failure(expect_equal(outputs$corner_coord, outputs_2$corner_coord))
  expect_failure(expect_equal(outputs$polygon, outputs_2$polygon))
})


test_that("check_plot_coord in long lat", {
  outputs_longlat <- check_plot_coord(NouraguesPlot201, longlat = c("Long","Lat"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F, max_dist = 10)
  outputs_proj_coord <- check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F, max_dist = 10)
  expect_true(is.list(outputs_longlat))
  expect_equal(names(outputs_longlat), c("corner_coord", "polygon", "plot_design", "outlier_corners","UTM_code","sd_coord"))
  expect_equal(names(outputs_longlat$corner_coord)[c(5,6)], c("long","lat"))
  expect_equal(outputs_longlat$corner_coord[,c(1:4)], outputs_proj_coord$corner_coord[,c(1:4)])
  expect_equal(outputs_longlat$outlier_corners, outputs_proj_coord$outlier_corners)
})

test_that("check_plot_coord, trust_GPS_corners", {
  expect_message(
    check_plot_coord(NouraguesPlot201[-(1:7),], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = T, rm_outliers = T, draw_plot = F),"At least one corner has less than 5 measurements. We suggest using the argument trust_GPS_corners = FALSE"
  )
  
  res_trust_GPS_corners_T <- check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = T, rm_outliers = T, draw_plot = F)
  res_trust_GPS_corners_F <- check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F)
  expect_equal(res_trust_GPS_corners_T$corner_coord, res_trust_GPS_corners_F$corner_coord, ignore_attr = TRUE)

})


test_that("check_plot_coord, tree data, raster and shapefile", {
  
  res <- suppressMessages(
    check_plot_coord(
      NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
      tree_data = NouraguesTrees[NouraguesTrees$Plot=="201",], tree_coords = c("Xfield","Yfield")))
  
  expect_true(is.data.frame(res$tree_data))
  expect_equal(dim(res$tree_data), c(nrow(NouraguesTrees[NouraguesTrees$Plot=="201",]), ncol(NouraguesTrees)+3))
  expect_equal(sum(!res$tree_data$is_in_plot),3)
  
  res_trust_F <- suppressMessages(
    check_plot_coord(
      NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = FALSE, rm_outliers = T, draw_plot = F,
      tree_data = NouraguesTrees[NouraguesTrees$Plot=="201",], tree_coords = c("Xfield","Yfield")))
  
  nouragues_raster <- terra::rast(system.file("extdata", "NouraguesRaster.tif", package = "BIOMASS", mustWork = TRUE))
  res_prop_raster <- suppressMessages(
    check_plot_coord(
      NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
      tree_data = NouraguesTrees[NouraguesTrees$Plot=="201",], tree_coords = c("Xfield","Yfield"), prop_tree = "D", threshold_tree = 20,
      ref_raster = nouragues_raster))
  nouragues_raster_path <- system.file("extdata", "NouraguesRaster.tif", package = "BIOMASS", mustWork = TRUE)
  res_prop_raster <- 
    check_plot_coord(
      NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
      ref_raster = nouragues_raster_path)
  
  ### Shapefile
  shapefile_path <- "../testdata/shapefiles/pp_canopy_gap_lines_proj.shp"
  check_plot_coord(
    NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
    trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
    shapefile = shapefile_path, ref_raster = nouragues_raster)
  
  shapefile <- sf::st_read("../testdata/shapefiles/pp_canopy_gap_lines.shp", quiet = TRUE)
  expect_message(check_plot_coord(
    NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
    trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
    shapefile = shapefile) , 
    "The shapefile will not be displayed.")
  check_plot_coord(
    NouraguesPlot201, longlat = c("Long","Lat"), rel_coord = c("Xfield","Yfield"),
    trust_GPS_corners = T, rm_outliers = T, draw_plot = T,
    shapefile = shapefile, ref_raster = nouragues_raster)
  
})

test_that("check_plot_coord, sd_coord", {
  controled_sd <- as.data.table(rbind(NouraguesPlot201,NouraguesPlot201,NouraguesPlot201,NouraguesPlot201))
  controled_sd[, c("Xutm","Yutm") := list(mean(Xutm, na.rm=TRUE) + rnorm(40,0,5), mean(Yutm, na.rm=TRUE) + rnorm(40,0,5)) , by = list(Xfield, Yfield) ]
  res <- suppressMessages(
    check_plot_coord(
      controled_sd, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = T, rm_outliers = FALSE, draw_plot = F))
  
  expect_equal(res$sd_coord, 5, tolerance = 0.3)
  
})
