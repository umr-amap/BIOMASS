context("divide Plot")

data("NouraguesCoords")
data("NouraguesPlot201")
data("NouraguesTrees")

corner_data <- suppressWarnings(check_plot_coord(NouraguesPlot201, proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"), trust_GPS_corners = T, draw_plot = F))$corner_coord

test_that("divide_plot error", {
  
  expect_error(divide_plot(rel_coord = corner_data[c("x_rel","y_rel")]),
               "The way in which arguments are provided to the function has changed since version 2.2.1. You now have to provide corner_data data frame and its associated coordinates variable names.")
  expect_error(divide_plot(as.matrix(corner_data), c("x_rel","y_rel"), grid_size = 25), "corner_data must a data frame or a data frame extension")
  expect_error(divide_plot(corner_data, rel_coord = c("Xfield","Yfield")), "column names provided by rel_coord are not found in corner_data")
  expect_error(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("Xutm","Yutm")), "column names provided by proj_coord are not found in corner_data")
  expect_error(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), longlat = c("Xutm","Yutm")), "column names provided by longlat are not found in corner_data")
  expect_error(divide_plot(corner_data = NouraguesCoords, rel_coord = c("Xfield","Yfield"), grid_size = c(25,25,25)), "you must apply yourself the function for each plot")
  
  expect_error(divide_plot(corner_data = NouraguesCoords, c("Xfield","Yfield"), grid_size = 25), "You must provide corner_plot_ID if you have more than one plot in your data")
  expect_error(divide_plot(corner_data = NouraguesCoords, c("Xfield","Yfield"), grid_size = 25, corner_plot_ID = "a"), "is not found in corner_data column names.")
  expect_error(divide_plot(NouraguesPlot201, c("Xfield","Yfield"), grid_size = 25, corner_plot_ID = "Plot"), "corner_data does'nt contain exactly 4 corners by plot")
  
  expect_error(divide_plot(corner_data, c("x_rel","y_rel"), grid_size = 25, tree_data = matrix()),"tree_data must be a data frame or a data frame extension")
  expect_error(divide_plot(corner_data, c("x_rel","y_rel"), grid_size = 25, tree_data = NouraguesTrees), "You must provide the column names of the relative coordinates of the trees using the tree_coords argument")
  expect_error(divide_plot(corner_data, c("x_rel","y_rel"), grid_size = 25, tree_data = NouraguesTrees, tree_coords = c("x_rel","y_rel")), "column names provided by tree_coords are not found in tree_data colunm names")
  expect_error(divide_plot(NouraguesCoords, c("Xfield","Yfield"), grid_size = 25, corner_plot_ID = "Plot", tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield")), 
               "You must provide tree_plot_ID if you have more than one plot in your data")
  expect_error(divide_plot(NouraguesCoords, c("Xfield","Yfield"), grid_size = 25, corner_plot_ID = "Plot", tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "a"),
               "is not found in tree_data column names.")
  

  expect_warning(divide_plot(corner_data, c("x_rel","y_rel"), grid_size = c(30,25)) , "The x-dimension of the plot is not a multiple of the x-dimension of the grid size")
  expect_warning(divide_plot(corner_data, c("x_rel","y_rel"), grid_size = c(25,30)) , "The y-dimension of the plot is not a multiple of the y-dimension of the grid size")
  expect_error(suppressWarnings(divide_plot(corner_data, c("x_rel","y_rel"), grid_size = c(40,40))) , "If you still want to divide the plot, please increase the value of the grid_tol argument.")

  wrong_NouraguesTrees <- NouraguesTrees ; wrong_NouraguesTrees[1,"Plot"] <- 200
  expect_warning(divide_plot(NouraguesCoords, c("Xfield","Yfield"), grid_size = 25, corner_plot_ID = "Plot", tree_data = wrong_NouraguesTrees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"), "(not in a subplot area)")

  # when the plot is not a rectangle
  rect_plot <- data.frame(x_rel=c(0,100,0,110),y_rel=c(0,0,100,100))
  expect_error(divide_plot(rect_plot, c("x_rel","y_rel"), grid_size = 25) , "BIOMASS package can't deal with non-rectangular plot")
})

test_that("divide_plot on relative coordinates only", {

  # Test when corner_data is a data.table
  subplots <- divide_plot(as.data.table(corner_data), rel_coord = c("x_rel","y_rel"), grid_size = 50)
  expect_equal(subplots[1:4,] , data.frame(subplot_ID=rep("subplot_0_0",4),x_rel=c(0,50,50,0), y_rel=c(0,0,50,50)))

  # Test rectangular division
  rect_subplots <- divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = c(25,50))
  expect_equivalent(rect_subplots[29:32,] , data.frame(subplot_ID=rep("subplot_3_1",4),x_rel=c(75,100,100,75), y_rel=c(50,50,100,100)))

  # Test when the origin is not (0;0)
  subplots <- divide_plot(NouraguesCoords[13:16,], rel_coord = c("Xfield","Yfield"), grid_size = 50)
  expect_equivalent(subplots[13:16,] , data.frame(subplot_ID=rep("subplot_1_1",4),x=c(250,300,300,250), y=c(250,250,300,300)))

  # Test multiple plots
  multiple_subplots <- divide_plot(NouraguesCoords, rel_coord = c("Xfield","Yfield"), grid_size = 50, corner_plot_ID = "Plot")
  expect_equivalent(multiple_subplots[49:64,3:4] , subplots[,2:3])
  expect_equivalent(multiple_subplots[61:64,1:2] , data.frame(corner_plot_ID = rep(223,4), subplot_ID = rep("223_1_1",4)))

  
  # Test rectangular plot
  subplots <- divide_plot(expand.grid(x = c(0, 100), y = c(0, 50)), rel_coord = c("x","y"), grid_size = c(50,25))
  expect_equivalent(subplots[5:8,] , data.frame(subplot_ID=rep("subplot_1_0",4),x=c(50,100,100,50), y=c(0,0,25,25)))

  # Test non-adjusted grid
  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = c(45,20), grid_tol = 0.3, centred_grid = T))
  expect_equivalent(subplots[5:8,] , data.frame(subplot_ID=rep("subplot_1_0",4),x=c(50,95,95,50), y=c(0,0,20,20)))
})

test_that("divide_plot with projected coordinates", {

  subplots <- divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"), grid_size = 50)
  #ggplot(subplots,aes(x=x_proj,y=y_proj,label=subplot_ID)) + geom_point()
  expect_equivalent(subplots[13:16,c("subplot_ID","x_proj","y_proj")] ,
                    data.frame(subplot_ID="subplot_1_1",
                               x_proj=c(313028.4,313003.6,313050.2,313075.4),
                               y_proj=c(451650.5,451606.4,451582.6,451624.2)),
                    tol = 1e-5)

  # Test with longlat :
  corner_data[c("long","lat")] <- as.data.frame( proj4::project(corner_data[c("x_proj","y_proj")], proj = "+proj=utm +zone=22 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs", inverse = TRUE) )
  subplots_longlat <- divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), longlat = c("long","lat"), grid_size = 50)
  expect_equal(subplots, subplots_longlat$sub_corner_coord[,1:5])
  expect_equal(subplots_longlat$sub_corner_coord[1,6:7] , data.frame(long = -52.68448, lat = 4.08504), tolerance = 1e-06)
  
  # Test multiple plots
  multiple_subplots <- divide_plot(NouraguesCoords, rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), grid_size = 50, corner_plot_ID = "Plot")
  #ggplot(multiple_subplots,aes(x=x_proj,y=y_proj,label=subplot_ID)) + geom_point() + geom_text(position = position_jitter()) + coord_equal()
  expect_equivalent(multiple_subplots[64,] ,
                    data.frame(corner_plot_ID=223, subplot_ID="223_1_1", x_rel=250, y_rel=300, x_proj=c(313152.2), y_proj=c(451354.3)), tol=1e-5)

})

test_that("divide_plot with tree coordinates", {

  # Test warning when a tree is not in any subplot
  expect_warning(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 50, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))

  subplots <- suppressWarnings(divide_plot(corner_data, rel_coord = c("x_rel","y_rel"), grid_size = 50, tree_data = NouraguesTrees[NouraguesTrees$Plot==201,], tree_coords = c("Xfield","Yfield")))
  
  expect_equal(subplots$tree_data$subplot_ID[4:6] , c(NA,"subplot_0_0",NA))
  
  # Test with multiple plots
  multiple_subplots <- suppressWarnings(
    divide_plot(corner_data = NouraguesCoords, rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"), grid_size = 50, corner_plot_ID = "Plot",
                tree_data = NouraguesTrees, tree_coords = c("Xfield","Yfield"), tree_plot_ID = "Plot"))

  expect_equal(multiple_subplots$tree_data$subplot_ID[c(100,101)], c("201_0_1","201_0_0"))


})

