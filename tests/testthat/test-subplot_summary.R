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
  expect_error(subplot_summary(subplots, draw_plot = F) , "You must supply the tree variable to be summarised via the value argument.", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = "a", draw_plot = F) , "a is not a column name of subplots$tree_data", fixed=TRUE)

  expect_error(subplot_summary(subplots, value = "D", draw_plot = F, fun = "quantile") , "the function supplied using `fun =` is not a function", fixed=TRUE)
  expect_error(subplot_summary(subplots, value = "D", draw_plot = F, fun = quantile) , "the function supplied using `fun` must return a single value", fixed=TRUE)
})

test_that("subplot_summary", {

  # Test without proj_coord
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 25, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  vdiffr::expect_doppelganger("subplot-summary-rel-coords", suppressMessages(subplot_summary(subplots, value = "D", draw_plot = F)$plot_design))
  
  # Test with proj_coord
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 25, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  res <- suppressMessages(subplot_summary(subplots, value = "D", draw_plot = F))
  vdiffr::expect_doppelganger("subplot-summary-proj-coords", res$plot_design)

  # Test when there isn't a tree in a subplot
  subplots_less_trees <- subplots
  subplots_less_trees$tree_data <- subplots_less_trees$tree_data[subplots_less_trees$tree_data$subplot_ID != "subplot_0_1",]
  res_less <- subplot_summary(subplots_less_trees, value = "D", draw_plot = F)
  expect_equivalent(res$tree_summary[-2,] , res_less$tree_summary)
  vdiffr::expect_doppelganger("subplot-summary-proj-coords", suppressMessages(subplot_summary(subplots, value = "D", draw_plot = F)$plot_design))

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
})
