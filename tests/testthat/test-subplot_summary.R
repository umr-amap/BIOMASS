# context("subplot_summary")
# 
# test_that("subplot_summary error", {
#   rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 200, 200))
#   subplots <- divide_plot(rel_coord, grid_size = 50)
#   
#   expect_error(subplot_summary(subplots), "subplots argument does'nt contain any tree data frame. Use the divide_plot function with a non-null tree_df argument")
#   expect_error(subplot_summary(subplots = list(subplots)), "subplots argument must be the output of the divide_plot_function, with a non-null tree_df argument")
#   expect_error(subplot_summary(subplots = list(sub_corner_coord = subplots)), "subplots argument must be the output of the divide_plot_function, with a non-null tree_df argument")
#   
#   tree_df <- data.frame(x_tree = runif(50,0,200), y_tree = runif(50,0,200), metric = rnorm(50,10,5))
#   subplots <- divide_plot(rel_coord, grid_size = 50, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#   expect_message(subplot_summary(subplots, value = "metric", draw_plot = F), "projected coordinates are not found in sub_corner_coord$subplots, tree metric will be summarised in the relative coordinate system", fixed=TRUE)
#   
#   proj_coord <- data.frame(x_proj = c(210, 383, 110, 283), y_proj = c(210, 310, 383, 483))
#   subplots <- divide_plot(rel_coord, proj_coord, grid_size = 50, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#   expect_error(subplot_summary(subplots, draw_plot = F) , "value is not a column name of subplots$tree_df", fixed=TRUE)
#   expect_error(subplot_summary(subplots, value = "toto", draw_plot = F) , "value is not a column name of subplots$tree_df", fixed=TRUE)
#   
#   expect_error(subplot_summary(subplots, value = "metric", draw_plot = F, fun = "quantile") , "the function supplied using `fun =` is not a function", fixed=TRUE)  
#   expect_error(subplot_summary(subplots, value = "metric", draw_plot = F, fun = quantile) , "the function supplied using `fun` must return a single value", fixed=TRUE)  
# })
# 
# test_that("subplot_summary", {
#   set.seed(52)
#   rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 200, 200))
#   subplots <- divide_plot(rel_coord, grid_size = 50)
#   tree_df <- data.frame(x_tree = runif(50,0,200), y_tree = runif(50,0,200), metric = rnorm(50,10,5))
#   subplots <- divide_plot(rel_coord, grid_size = 50, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#   
#   # Test without proj_coord
#   res <- suppressMessages(subplot_summary(subplots, value = "metric", draw_plot = F))
#   expect_equivalent(res$tree_summary[1:2,] , data.frame(subplot_id = c("subplot_0_0","subplot_0_1"),
#                                                         metric_summary = c(12.460178,43.79396), 
#                                                         metric_summary_per_ha = c(49.84071, 175.175823)) , tol=1e-5)
#   expect_equal(res$tree_summary, data.frame(subplot_id=res$polygon[[1]], metric_summary = res$polygon[[2]], metric_summary_per_ha = res$polygon[[3]]))
#   expect_equivalent(sf::st_polygon(list(matrix(c(0, 0, 50, 0,50, 50, 0, 50, 0, 0), ncol = 2, byrow = T))), res$polygon[[4]][[1]])
#   #subplot_summary(subplots, value = "metric", draw_plot = T)
# 
#   # Test with proj_coord
#   proj_coord <- data.frame(x_proj = c(210, 383, 110, 283), y_proj = c(210, 310, 383, 483))
#   subplots <- divide_plot(rel_coord, proj_coord, grid_size = 50, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#   res <- subplot_summary(subplots, value = "metric", draw_plot = F)
#   expect_equivalent(res$tree_summary[1:2,] , data.frame(subplot_id = c("subplot_0_0","subplot_0_1"),
#                                                         metric_summary = c(12.460178,43.793956), 
#                                                         metric_summary_per_ha = c(49.929337, 175.487313)))
#   expect_equivalent(sf::st_polygon(list(matrix(c(210,210,253.25,235,228.25,278.25,185,253.25,210,210), ncol = 2, byrow = T))), res$polygon[[4]][[1]])
#   #subplot_summary(subplots, value = "metric", draw_plot = T)
#   
#   # Test when there isn't a tree in a subplot
#   tree_df <- tree_df[-6,]
#   subplots_less_tree <- divide_plot(rel_coord, proj_coord, grid_size = 50, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#   res_less_tree <- subplot_summary(subplots_less_tree, value = "metric", draw_plot = F)
#   expect_equivalent(res$tree_summary[-7,] , res_less_tree$tree_summary)
#   expect_equal(res$polygon$sf_subplot_polygon, res_less_tree$polygon$sf_subplot_polygon)
#   
#   # Test with quantile function 
#   res_quantile <- subplot_summary(subplots_less_tree, value = "metric", draw_plot = F, fun = quantile, probs=0.75)
#   expect_equal(res_quantile$tree_summary[res_quantile$tree_summary$subplot_id=="subplot_3_2","metric_summary"],16.255414,tol=1e-5)
#   
#   # Test multiple plots 
#   multiple_rel_coord <- rbind(rel_coord , rel_coord)
#   multiple_proj_coord <- rbind(proj_coord,proj_coord)
#   multiple_rel_coord$plotID <- rep(c("plot1","plot2"),e=4)
#   multiple_tree_df <- rbind(tree_df,tree_df)
#   multiple_tree_df$plot_id <- rep(c("plot1","plot2"),e=nrow(tree_df))
#   multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x_rel","y_rel")], grid_size = 50, proj_coord = multiple_proj_coord, corner_plot_ID = multiple_rel_coord$plotID,
#                                    tree_df = multiple_tree_df , tree_coords = c("x_tree","y_tree"), tree_plot_ID = multiple_tree_df$plot_id)
#   res_multiple <- subplot_summary(multiple_subplots, value = "metric", draw_plot = F)
#   expect_equivalent(res_multiple$tree_summary[1:15,c("metric_summary","metric_summary_per_ha")] , res_less_tree$tree_summary[c("metric_summary","metric_summary_per_ha")])
#   expect_equivalent(res_multiple$tree_summary$subplot_id[1:5],c("plot1_0_0","plot1_0_1","plot1_0_2","plot1_0_3","plot1_1_0"))
# })
# 
# 
# test_that("ggplot_subplot_summary", {
#   set.seed(52)
#   rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 200, 200))
#   proj_coord <- data.frame(x_proj = c(210, 383, 110, 283), y_proj = c(210, 310, 383, 483))
#   subplots <- divide_plot(rel_coord, grid_size = 50)
#   tree_df <- data.frame(x_tree = runif(50,0,200), y_tree = runif(50,0,200), metric = rnorm(50,10,5))
#   subplots <- divide_plot(rel_coord, proj_coord = proj_coord, grid_size = 100, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#   vdiffr::expect_doppelganger("disp-subplot-summary", subplot_summary(subplots, value = "metric", draw_plot = T)$plot_design)
# })
