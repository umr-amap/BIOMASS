context("subplot_summary")

data("NouraguesCoords")
data("NouraguesPlot201")
data("NouraguesTrees")

corner_data <- suppressWarnings(check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = T, draw_plot = F))$corner_coord

test_that("subplot_summary error", {
  subplots <- divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 50)

  expect_error(subplot_summary(subplots), "subplots argument does'nt contain any tree data frame. Use the divide_plot function with a non-null tree_data argument")
  expect_error(subplot_summary(subplots = list(subplots)), "subplots argument must be the output of the divide_plot_function, with a non-null tree_data argument")
  expect_error(subplot_summary(subplots = list(sub_corner_coord = subplots)), "subplots argument must be the output of the divide_plot_function, with a non-null tree_data argument")

  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 50, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  expect_message(subplot_summary(subplots, value = "D", draw_plot = F), "Projected coordinates are not found in sub_corner_coord$subplots, tree metric will be summarised in the relative coordinate system", fixed=TRUE)

  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 50, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  expect_error(subplot_summary(subplots, draw_plot = F) , "You must provide the tree variable to be summarised via the value argument.", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = "a", draw_plot = F) , "a is not a column name of subplots$tree_data", fixed=TRUE)

  expect_error(subplot_summary(subplots, value = "D", draw_plot = F, fun = "quantile") , "the function provided using `fun =` is not a function", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = c("D","D"), draw_plot = F, fun = list(D="quantile",D=mean)) , "function(s) provided in `fun` (not a function)", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = "D", draw_plot = F, fun = quantile) , "the function provided using `fun` must return a single value", fixed=TRUE)
  
  expect_error(subplot_summary(subplots, value = c("D","D"), draw_plot = F, fun = list(D=mean)) , "the lengths of 'value' and 'fun' are not the same", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = c("D","D","D"), draw_plot = F, fun = list(D=mean,sum,sd), per_ha = c(T,F)) , "the lengths of 'value' and 'per_ha' are not the same", fixed=TRUE)
  
})

test_that("subplot_summary", {

  # Test without proj_coord
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 25, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  vdiffr::expect_doppelganger("subplot-summary-rel-coords", suppressMessages(subplot_summary(subplots, value = "D", draw_plot = F)$plot_design))
  
  # Test with proj_coord
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 25, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  res <- suppressMessages(subplot_summary(subplots, value = "D", draw_plot = F))
  vdiffr::expect_doppelganger("subplot-summary-proj-coords", res$plot_design)

  # Test with longlat
  corner_data[c("long","lat")] <- as.data.frame( proj4::project(corner_data[c("x_proj","y_proj")], proj = "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs", inverse = TRUE) )
  subplots_longlat <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), longlat = c("long","lat"), grid_size = 25, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  res <- suppressMessages(subplot_summary(subplots_longlat, value = "D", draw_plot = F))
  # vdiffr::expect_doppelganger("subplot-summary-geographic-coords", res$plot_design) # this test works for ubuntu checks but not for windows and MAC
  expect_equivalent(res$polygon$sf_subplot_polygon[[1]][[1]][1:4,], as.matrix(subplots_longlat$sub_corner_coord[1:4,c("long","lat")]) )
  
  # Test when there isn't a tree in a subplot
  subplots_less_trees <- subplots
  subplots_less_trees$tree_data <- subplots_less_trees$tree_data[subplots_less_trees$tree_data$subplot_ID != "subplot_0_1",]
  res_less <- subplot_summary(subplots_less_trees, value = "D", draw_plot = F)
  expect_equivalent(res_less$tree_summary[2,"D_sum_per_ha"] , 0)

  # Test with quantile function
  res_quantile <- subplot_summary(subplots, value = "D", draw_plot = F, fun = quantile, probs=0.75)
  vdiffr::expect_doppelganger("subplot-summary-quantile", res_quantile$plot_design)
  
  # Test multiple plots
  multiple_subplots <- suppressWarnings(divide_plot(corner_data = NouraguesCoords,
                                                    rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), 
                                                    grid_size = 25, corner_plot_ID = "Plot",
                                                    tree_data = NouraguesTrees , tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"))
  
  res_multiple <- subplot_summary(multiple_subplots, value = "D", draw_plot = F)
  vdiffr::expect_doppelganger("subplot-summary-multiple-plot-201", res_multiple$plot_design$`201`)
  vdiffr::expect_doppelganger("subplot-summary-multiple-plot-204", res_multiple$plot_design$`204`)
  
  # Test with multiple metrics
  res_metrics <- subplot_summary(multiple_subplots, value = c("D","x_rel"), fun = list(D=sum,x_rel=mean), per_ha = c(T,F) , draw_plot = F)
  vdiffr::expect_doppelganger("subplot-summary-multiple-metrics-204", res_metrics$plot_design$`204`[[2]])
  
})

test_that("subplot_summary_raster", {
  multiple_subplots <- suppressWarnings(divide_plot(corner_data = NouraguesCoords,
                                                    rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), 
                                                    grid_size = 25, corner_plot_ID = "Plot",
                                                    tree_data = NouraguesTrees , tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"))
  
  nouragues_raster <- terra::rast( system.file("extdata", "NouraguesRaster.tif", package = "BIOMASS", mustWork = TRUE))
  
  res_multiple <- subplot_summary(multiple_subplots, value = c("D","D"), draw_plot = FALSE, fun = list(mean,sd), ref_raster = nouragues_raster, raster_fun = list(mean,sd))
  rownames(res_multiple$tree_summary) <- NULL
  
  expect_equal(res_multiple$tree_summary[1,] ,
                   data.frame(plot_ID=201, subplot_ID="201_0_0",
                              D_mean_per_ha=409.8663, D_sd_per_ha=313.5045,
                              z2012_mean=24.26298,z2012_sd=11.45732), 
               tolerance = 1e-4)
  vdiffr::expect_doppelganger("subplot-summary-multiple-metrics-CHM-204", res_multiple$plot_design$`204`[[3]])
  
  subplots <- suppressWarnings(divide_plot(NouraguesCoords[NouraguesCoords$Plot==201,], rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), grid_size = 25, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  res_unique <- subplot_summary(subplots, value = "D", draw_plot = FALSE, fun = mean, ref_raster = nouragues_raster, raster_fun = mean)
  rownames(res_unique$tree_summary) <- NULL
  
  expect_equal(res_multiple$tree_summary[1:16, c(3,5)] , res_unique$tree_summary[,c(2,3)], tol=1e-5)
  
})

