
data("NouraguesCoords")
data("NouraguesPlot201")
data("NouraguesTrees")
NouraguesTrees201 <- NouraguesTrees[NouraguesTrees$Plot==201,]
nouragues_raster <- terra::rast( system.file("extdata", "NouraguesRaster.tif", package = "BIOMASS", mustWork = TRUE))

corner_data <- suppressWarnings(check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, draw_plot = F))$corner_coord

test_that("subplot_summary error", {
  subplots <- divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 50)
  
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 50, tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
  
  expect_message(subplot_summary(subplots = subplots, value = "D", draw_plot = F), "Projected coordinates are not found in sub_corner_coord$subplots, metric(s) will be summarised in the relative coordinate system.", fixed=TRUE)
  
  expect_error(suppressMessages(subplot_summary(subplots = subplots, AGB_simu = matrix(1:4))), "The rows in 'subplots$tree_data' must match the rows in 'AGB_simu'", fixed = TRUE)
  
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 50, tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
  
  expect_error(subplot_summary(subplots, value = "a", draw_plot = F) , "a is not a column name of subplots$tree_data", fixed=TRUE)
  
  expect_error(subplot_summary(subplots, value = "D", draw_plot = F, fun = "quantile") , "the function provided using `fun =` is not a function", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = c("D","D"), draw_plot = F, fun = list(D="quantile",D=mean)) , "the function quantile provided in `fun =` is not a function", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = "D", draw_plot = F, fun = quantile) , "the function provided using `fun` must return a single value", fixed=TRUE)
  
  expect_error(subplot_summary(subplots, value = c("D","D"), draw_plot = F, fun = list(D=mean)) , "the lengths of 'value' and 'fun' are not the same", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = c("D","D","D"), draw_plot = F, fun = list(D=mean,sum,sd), per_ha = c(T,F)) , "the lengths of 'value' and 'per_ha' are not the same", fixed=TRUE)
  
})

test_that("subplot_summary_value", {
  corner_data <- suppressWarnings(check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = FALSE, draw_plot = F))$corner_coord
  
  # Test with rel_coord
  subplots_rel_coord <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 25,
                                                     tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
  res_rel_coord <- suppressMessages(subplot_summary(subplots_rel_coord, value = "D", draw_plot = F))
  expect_equal(res_rel_coord$tree_summary[1,2], as.data.frame(as.data.table(subplots_rel_coord$tree_data)[subplot_ID=="subplot_0_0", sum(D)] * 16),
               ignore_attr = T)
  
  
  # Test with proj_coord
  subplots_proj_coord <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 25, tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
  res_proj_coord <- suppressMessages(subplot_summary(subplots_proj_coord, value = "D", draw_plot = F))
  expect_equal(res_rel_coord$tree_summary, res_proj_coord$tree_summary) # equal as corner_dat has been evaluated with trust_GPS_corners=FALSE 
  
  # Test with longlat
  longlat_corner_data <- suppressWarnings(check_plot_coord(NouraguesPlot201, longlat = c("Long","Lat"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = F, draw_plot = F))
  corner_data[c("long","lat")] <- as.data.frame( proj4::project(corner_data[c("x_proj","y_proj")],
                                                                proj = longlat_corner_data$UTM_code$UTM_code, inverse = TRUE) )
  subplots_longlat <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), longlat = c("long","lat"), grid_size = 25, tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
  res_longlat <- subplot_summary(subplots = subplots_longlat, value = "D", draw_plot = F)
  expect_equal(res_longlat$tree_summary, res_proj_coord$tree_summary)
  
  # Test when there isn't a tree in a subplot
  subplots_less_trees <- subplots_proj_coord
  subplots_less_trees$tree_data <- subplots_less_trees$tree_data[subplots_less_trees$tree_data$subplot_ID != "subplot_0_1",]
  res_less <- subplot_summary(subplots_less_trees, value = "D", draw_plot = F)
  expect_true(is.na(res_less$tree_summary[5,"D_sum_per_ha"]))
  
  # Test with quantile function
  res_quantile <- subplot_summary(subplots_longlat, value = "D", draw_plot = F, fun = quantile, probs=0.75, per_ha = FALSE)
  expect_equal(res_quantile$tree_summary$D_quantile[1], as.data.table(subplots_longlat$tree_data)[subplot_ID == "subplot_0_0",quantile(D,probs=0.75)],
               ignore_attr = T)
  
  # Test multiple plots
  multiple_subplots <- suppressWarnings(divide_plot(corner_data = NouraguesCoords,
                                                    rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), 
                                                    grid_size = 50, corner_plot_ID = "Plot",
                                                    tree_data = NouraguesTrees , tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"))
  
  res_multiple <- subplot_summary(subplots = multiple_subplots, value = "D", fun=mean, draw_plot = F, per_ha = FALSE)
  res_mean_D <- as.data.table(multiple_subplots$tree_data)[!is.na(subplot_ID), mean(D), by=subplot_ID]
  expect_equal(res_multiple$tree_summary[order(res_multiple$tree_summary$subplot_ID),"D_mean"], as.data.frame(res_mean_D[order(res_mean_D$subplot_ID),V1]),
               ignore_attr = T)
  
  # Test with multiple metrics
  res_metrics <- subplot_summary(subplots = multiple_subplots, value = c("D","x_rel"), fun = list(D=sum,x_rel=mean), per_ha = c(T,F) , draw_plot = F)
  expect_equal(names(res_metrics$tree_summary), c("subplot_ID","x_rel_mean","D_sum_per_ha"))
  
  # Test with user function : 
  count_fun <- function(x) length(na.omit(x))
  res_metrics <- subplot_summary(subplots = subplots_longlat, value = c("D","x_rel"), fun = list(D=sum,x_rel=count_fun), per_ha = c(T,F) , draw_plot = F)
  expect_equal(names(res_metrics$tree_summary), c("subplot_ID","x_rel_count_fun","D_sum_per_ha"))
  
})

test_that("subplot_summary_raster", {
  multiple_subplots <- suppressWarnings(divide_plot(corner_data = NouraguesCoords,
                                                    rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), 
                                                    grid_size = 25, corner_plot_ID = "Plot",
                                                    tree_data = NouraguesTrees , tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"))
  
  expect_error(subplot_summary(subplots = multiple_subplots, value = c("D","D"), draw_plot = FALSE, fun = list(mean,sd), ref_raster = "toto", raster_fun = list(mean,sd)) , "ref_raster is not recognised as a SpatRaster of terra package")
  
  expect_error(subplot_summary(multiple_subplots, value = "D", draw_plot = F, raster_fun = "quantile") , "the function provided using `raster_fun =` is not a function", fixed=TRUE)
  expect_error(subplot_summary(multiple_subplots, value = c("D","D"), draw_plot = F, raster_fun = list(D="quantile",D=mean)) , "not a function")
  expect_error(subplot_summary(multiple_subplots, value = "D", draw_plot = F, ref_raster = nouragues_raster, raster_fun = quantile) , "the function provided using `raster_fun` must return a single value", fixed=TRUE)
  
  
  res_multiple <- subplot_summary(multiple_subplots, value = c("D","D"), draw_plot = FALSE, fun = list(mean,sd), ref_raster = nouragues_raster, raster_fun = list(mean,sd))
  rownames(res_multiple$tree_summary) <- NULL
  
  expect_equal(as.data.frame(res_multiple$tree_summary[1,]) ,
               data.frame(subplot_ID="201_0_0",
                          D_mean_per_ha=409.8663, D_sd_per_ha=313.5045,
                          z2012_mean=24.26298,z2012_sd=11.45732), 
               tolerance = 1e-2)
  
  
  subplots <- suppressWarnings(divide_plot(NouraguesCoords[NouraguesCoords$Plot==201,], rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), grid_size = 25, tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
  res_unique <- subplot_summary(subplots, value = "D", draw_plot = FALSE, fun = mean, ref_raster = nouragues_raster, raster_fun = mean)
  rownames(res_unique$tree_summary) <- NULL
  
  expect_equal(res_multiple$tree_summary[1:16, c(2,4)] , res_unique$tree_summary[,c(2,3)], tolerance=1e-2)
  
})


HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2", bayesian = FALSE)
NouraguesWD <- suppressWarnings(getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species))

error_prop <- AGBmonteCarlo(
  D = NouraguesTrees$D, WD = NouraguesWD$meanWD, # we do not provide H
  HDmodel = HDmodel, # but we provide HDmodel
  Dpropag = "chave2004",
  errWD = NouraguesWD$sdWD, n = 50)
error_prop_201 <- error_prop$AGB_simu[NouraguesTrees$Plot==201,]

subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 25,
                                         tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield")))
multiple_subplots <- suppressWarnings(divide_plot(corner_data = NouraguesCoords,
                                                  rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), 
                                                  grid_size = 25, corner_plot_ID = "Plot",
                                                  tree_data = NouraguesTrees , tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"))


test_that("subplot_summary_AGB_uncertainties", {
  
  expect_error(subplot_summary(subplots = subplots, AGB_simu = error_prop), "'AGB_simu' must be a matrix containing individual AGB (one row per tree), typically, the output '$AGB_simu' of the AGBmonteCarlo() function.", fixed=TRUE)
  expect_error(subplot_summary(subplots = subplots, AGB_simu = error_prop$AGB_simu), "The rows in 'subplots$tree_data' must match the rows in 'AGB_simu'", fixed=TRUE)
  
  ### One plot:
  expect_message(res <- subplot_summary(subplots = subplots, AGB_simu = error_prop_201, draw_plot = FALSE), "AGB uncertainties will be propagated without propagation of corner GPS measurement uncertainties." )
  
  # check $tree_summary
  expect_equal(names(res$tree_summary), c("subplot_ID","AGBD_median","AGBD_cred_2.5","AGBD_cred_97.5"))
  # check $long_AGB_simu
  expect_equal(dim(res$long_AGB_simu), c(800, 6))
  expect_equal(length(unique(res$long_AGB_simu$x_center)), 16)
  expect_equal(names(res$long_AGB_simu), c("plot_ID","subplot_ID","N_simu","x_center","y_center","AGBD"))
  # Check by hand of the AGBD calculation
  AGB_simu_201 <- data.table(error_prop_201)
  AGB_simu_201$subplot_ID <- subplots$tree_data$subplot_ID
  AGB_simu_201_sum <- AGB_simu_201[!is.na(subplot_ID), lapply(.SD, sum), by=subplot_ID, .SDcols = !"subplot_ID"]
  AGB_simu_201_sum <- data.frame(AGB_simu_201_sum)
  rownames(AGB_simu_201_sum) <- AGB_simu_201_sum$subplot_ID
  AGB_simu_201_sum$subplot_ID <- NULL
  AGB_simu_201_sum <- AGB_simu_201_sum*16
  AGB_simu_201_sum <- apply(AGB_simu_201_sum , 1 , median)
  expect_equal(res$tree_summary$AGBD_median , AGB_simu_201_sum[res$tree_summary$subplot_ID], ignore_attr = T)
  
  
  # With values : 
  res_w_values <- suppressMessages(subplot_summary(subplots = subplots, value = c("D","x_rel"), fun = list(D=sum,x_rel=mean), per_ha = c(T,F), AGB_simu = error_prop_201, draw_plot = F))
  # check $tree_summary
  expect_equal(names(res_w_values$tree_summary), c("subplot_ID","x_rel_mean","D_sum_per_ha","AGBD_median","AGBD_cred_2.5","AGBD_cred_97.5"))
  # check $plot_design
  expect_equal(length(res_w_values$plot_design), 3) # plot AGBD_median and the 2 metrics
  
  ### multiple plots:
  res_mult <- suppressMessages(subplot_summary(subplots = multiple_subplots, AGB_simu = error_prop$AGB_simu, draw_plot = FALSE))
  # check $tree_summary
  expect_equal(res_mult$tree_summary[grepl("201",subplot_ID),c(2,3,4)], res$tree_summary[,c(2,3,4)], tolerance = 0.1) #not exactly the same because of the number of digits of the area
  # check $plot_design
  expect_equal(length(res_mult$plot_design), 4) # the 4 plots
  expect_equal(length(res_mult$plot_design[[1]]), 1) # AGBD_median and that's it
  
  ### with raster
  res_mult_rast <- suppressMessages(subplot_summary(subplots = multiple_subplots, AGB_simu = error_prop$AGB_simu, draw_plot = FALSE, ref_raster = nouragues_raster, raster_fun = median))
  expect_equal(names(res_mult_rast$tree_summary), c("subplot_ID","z2012_median","AGBD_median","AGBD_cred_2.5","AGBD_cred_97.5") )
  expect_equal(length(res_mult_rast$plot_design[[1]]), 2 ) # z2012_median and AGBD median
  
  ### with raster and value
  res_mult_rast_value <- suppressMessages(subplot_summary(subplots = multiple_subplots, AGB_simu = error_prop$AGB_simu, value="D", fun = mean, draw_plot = FALSE, ref_raster = nouragues_raster))
  expect_equal(names(res_mult_rast_value$tree_summary), c("subplot_ID","D_mean_per_ha","z2012_mean","AGBD_median","AGBD_cred_2.5","AGBD_cred_97.5") )
  expect_equal(length(res_mult_rast_value$plot_design[[1]]), 3 ) # D_mean_per_ha, z2012_median and AGBD median 
  
})

test_that("subplot_summary_coord_uncertainties", {
  
  # One plot
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 50,
                          tree_data = NouraguesTrees201, tree_coords = c("Xfield","Yfield"),
                          sd_coord = 5, n = 10))
  expect_warning(res_coord <- subplot_summary(subplots = subplots, value = "D", draw_plot = FALSE), "the following results will not take into account the uncertainty of corner coordinates.")
  
  res_coord <- subplot_summary(subplots = subplots, value = "D", ref_raster = nouragues_raster, draw_plot = FALSE)
  expect_equal(names(res_coord$tree_summary), c("subplot_ID","D_sum_per_ha","z2012_mean_median","z2012_mean_cred_2.5","z2012_mean_cred_97.5") )
  expect_equal(length(res_coord$plot_design), 2 ) # D_sum_per_ha and z2012_median
  
  # multiple plot
  multiple_subplots <- suppressWarnings(divide_plot(corner_data = NouraguesCoords,
                                                    rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), 
                                                    grid_size = 50, corner_plot_ID = "Plot",
                                                    tree_data = NouraguesTrees , tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot",
                                                    sd_coord = 3, n = 10))
  res_coord_mult <- subplot_summary(subplots = multiple_subplots, value = "D", ref_raster = nouragues_raster, draw_plot = FALSE)
  expect_equal(names(res_coord_mult$tree_summary), c("subplot_ID","D_sum_per_ha","z2012_mean_median","z2012_mean_cred_2.5","z2012_mean_cred_97.5") )
  expect_equal(length(res_coord_mult$plot_design), 4 ) # 4 plots
  expect_equal(length(res_coord_mult$plot_design[[1]]), 2 ) # D_mean_per_ha and z2012_median
  
  
  # With AGB uncertainties
  # one plot:
  expect_message(
    res_coord <- subplot_summary(subplots = subplots, AGB_simu = error_prop_201, ref_raster = nouragues_raster, draw_plot = FALSE),
    "40 simulations will be resampled in 'subplots'.")
  
  expect_equal(names(res_coord$tree_summary), c("subplot_ID","AGBD_median","AGBD_cred_2.5","AGBD_cred_97.5","z2012_mean_median","z2012_mean_cred_2.5","z2012_mean_cred_97.5") )
  expect_equal(dim(res_coord$long_AGB_simu), c(50*4,7))
  expect_equal(names(res_coord$long_AGB_simu), c("plot_ID","subplot_ID","N_simu","x_center","y_center","AGBD","raster_metric"))
  expect_equal(length(unique(res_coord$long_AGB_simu$raster_metric)) , 4 * 10)
  
  # multiple plots: 
  multiple_subplots <- suppressWarnings(divide_plot(NouraguesCoords, rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), grid_size = 50,
                                           tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"),
                                           corner_plot_ID = "Plot", tree_plot_ID = "Plot",
                                           sd_coord = 5, n = 5))
  expect_message(
    res_coord_mult <- subplot_summary(subplots = multiple_subplots, AGB_simu = error_prop$AGB_simu[,1:4], ref_raster = nouragues_raster, draw_plot = FALSE),
    "the first 4 simulations contained in 'subplots$simu_coord' will be considered.", fixed=TRUE)
  
  expect_equal(names(res_coord_mult$tree_summary), c("subplot_ID","AGBD_median","AGBD_cred_2.5","AGBD_cred_97.5","z2012_mean_median","z2012_mean_cred_2.5","z2012_mean_cred_97.5") )
  expect_equal(dim(res_coord_mult$long_AGB_simu), c(4*4*4,7))
  expect_equal(names(res_coord_mult$long_AGB_simu), c("plot_ID","subplot_ID","N_simu","x_center","y_center","AGBD","raster_metric"))
  expect_equal(length(unique(res_coord_mult$long_AGB_simu$raster_metric)) , 4*4*4)
  
  
})



