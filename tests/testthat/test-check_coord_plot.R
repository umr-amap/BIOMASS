context("Check plot coordinates")

data("NouraguesCoords")
data("NouraguesPlot201")
data("NouraguesTrees")

test_that("check_plot_coord error", {

  expect_error(check_plot_coord(proj_coord=NouraguesPlot201[c("Xutm","Yutm")], rel_coord=NouraguesPlot201[c("Xfield","Yfield")]),
               "The way in which arguments are provided to the function has changed since version 2.2.1. You now have to provide corner_data data frame and its associated coordinates variable names.")
  expect_error(check_plot_coord(corner_data = as.matrix(NouraguesPlot201), rel_coord = c("Xfield","Yfield")),
               "corner_data must a data frame or a data frame extension")
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
  
  
  expect_error(check_plot_coord(NouraguesPlot201, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, max_dist=c(10,20) ),"The max_dist argument must be of length 1")

  wrong_coords <- NouraguesPlot201 ; wrong_coords[1,"Xfield"] <- NA
  expect_error(check_plot_coord(wrong_coords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T), "Missing values are detected in corner relative coordinates. Please remove them and call the function again")
  wrong_coords[1,"Xfield"] <- 0 ; wrong_coords[1,"Xutm"] <- NA
  expect_error(check_plot_coord(wrong_coords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T), "Missing values are detected in corner projected coordinates. Please remove them and call the function again")
  wrong_coords[1,"Xutm"] <- 0 ; wrong_coords[1,"Long"] <- NA
  expect_error(check_plot_coord(wrong_coords, longlat = c("Long","Lat"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T), "Missing values are detected in corner longitude/latitude coordinates. Please remove them and call the function again")
  
  expect_error(check_plot_coord(NouraguesCoords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T),"If multiple plots are present in corner_data, then the argument plot_ID is required.")
  expect_error(check_plot_coord(NouraguesCoords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "a"),"is not found in corner_data column names.")
  
  expect_error(check_plot_coord(NouraguesCoords, proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "Plot", tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield")), "The argument tree_plot_ID is required if plot_ID is provided.")
  
  expect_warning(check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "Plot", tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"), "These ID's are found in tree_plot_ID but not in plot_ID : 201 213 223")
  
  NA_trees <- NouraguesTrees[NouraguesTrees$Plot==204,] ; NA_trees[1,"Xfield"] <- NA
  expect_warning(check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord=c("Xutm","Yutm"), rel_coord=c("Xfield","Yfield"), trust_GPS_corners=T, plot_ID = "Plot", tree_data = NA_trees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot", draw_plot = F), "Missing values are detected in the relative coordinates of the trees. These trees will be removed from the dataset.")

})

test_that("check_plot_coord outputs and outliers", {
  # with max dist equal 10
  expect_warning(
    check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = F, draw_plot = F, max_dist = 10),"Be carefull, you may have GNSS measurement outliers"
  )
  outputs <- check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F, max_dist = 10)
  expect_is(outputs, "list")
  expect_length(outputs, 4)
  expect_equal(names(outputs), c("corner_coord", "polygon", "plot_design", "outlier_corners"))
  expect_is(outputs$corner_coord, "data.frame")
  expect_is(outputs$outlier_corners, "data.frame")

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
  expect_is(outputs_longlat, "list")
  expect_length(outputs_longlat, 5)
  expect_equal(names(outputs_longlat), c("corner_coord", "polygon", "plot_design", "outlier_corners","UTM_code"))
  expect_equal(outputs_longlat[c(1,4)], outputs_proj_coord[c(1,4)])
})

test_that("check_plot_coord, trust_GPS_corners", {
  expect_warning(
    check_plot_coord(NouraguesPlot201[-(1:7),], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = T, rm_outliers = T, draw_plot = F),"At least one corner has less than 5 measurements. We suggest using the argument trust_GPS_corners = FALSE"
  )
  
  res_trust_GPS_corners_T <- check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = T, rm_outliers = T, draw_plot = F)
  res_trust_GPS_corners_F <- check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, rm_outliers = T, draw_plot = F)
  expect_equivalent(res_trust_GPS_corners_T$corner_coord, res_trust_GPS_corners_F$corner_coord)

})

test_that("check_plot_coord, plot design", {
  
  vdiffr::expect_doppelganger("check-plot-201-trust-T", 
                              check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
                                               trust_GPS_corners = T, rm_outliers = T, draw_plot = F)$plot_design)
  
  vdiffr::expect_doppelganger("check-plot-201-trust-F", 
                              check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
                                               trust_GPS_corners = F, rm_outliers = T, draw_plot = F)$plot_design)
  
  vdiffr::expect_doppelganger("check-plot-204", 
                              check_plot_coord(NouraguesCoords[NouraguesCoords$Plot=="204",], proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
                                               trust_GPS_corners = T, rm_outliers = T, draw_plot = F)$plot_design)
})


test_that("check_plot_coord, tree data and raster", {
  
  res <- suppressWarnings(
    check_plot_coord(
      NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
      tree_data = NouraguesTrees[NouraguesTrees$Plot=="201",], tree_coords = c("Xfield","Yfield")))
  
  expect_is(res$tree_data, "data.frame")
  expect_equal(dim(res$tree_data), c(nrow(NouraguesTrees[NouraguesTrees$Plot=="201",]), ncol(NouraguesTrees)+3))
  
  vdiffr::expect_doppelganger("check-plot-201-trees", 
                              res$plot_design)
  
  nouragues_raster <- terra::rast(system.file("extdata", "NouraguesRaster.tif", package = "BIOMASS", mustWork = TRUE))
  res_prop_raster <- suppressWarnings(
    check_plot_coord(
      NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
      trust_GPS_corners = T, rm_outliers = T, draw_plot = F,
      tree_data = NouraguesTrees[NouraguesTrees$Plot=="201",], tree_coords = c("Xfield","Yfield"), prop_tree = "D",
      ref_raster = nouragues_raster))
  vdiffr::expect_doppelganger("check-plot-201-rast-prop", 
                              res_prop_raster$plot_design)
  
  
})
