context("divide Plot")

test_that("divide_plot error", {
  
  rel_coord <- expand.grid(X = c(0, 100), Y = c(0, 100))
  
  expect_error(divide_plot(rel_coord, grid_size = 25, plot_ID_corner = "plot"), "The length of plot_ID_corner and the number of rows of rel_coord are different")
  expect_error(divide_plot(rel_coord, grid_size = c(25,25,25)), "you must apply yourself the function for each plot")
  expect_error(divide_plot(rbind(rel_coord,c(0,0)),grid_size = 25), "rel_coord does'nt contain exactly 4 corners by plot")
  expect_error(divide_plot(rbind(rel_coord,rel_coord),grid_size = 25), "You must supply plot_ID_corner if you have more than one plot in your data")
  expect_error(divide_plot(rbind(rel_coord,rel_coord), grid_size = 25, plot_ID_corner = rep(1:4,2)), "plot_ID_corner must contain the same number of plots as rel_coord")
  
  corner_proj_coord <- sweep(rel_coord[1:3,], 2, c(150,150), "+")
  expect_error(divide_plot(rel_coord, proj_coord = corner_proj_coord ,grid_size = 25), "rel_coord and proj_coord are not of the same dimension")
  
  expect_error(divide_plot(rel_coord, grid_size = 25, tree_coord = c(10,10)), "tree_coord must be a matrix or a data frame")
  
  expect_error(divide_plot(rel_coord, grid_size = c(30,25)) , "The x-dimension of the plot is not a multiple of the x-dimension of the grid size")
  expect_error(divide_plot(rel_coord, grid_size = c(25,30)) , "The y-dimension of the plot is not a multiple of the y-dimension of the grid size")
  
  tree_coord <- data.frame(x =runif(100,0,100), y=runif(100,0,100))
  expect_error(divide_plot(rbind(rel_coord,rel_coord), grid_size = 25, plot_ID_corner = rep(1:2,e=4) , tree_coord = tree_coord), "You must supply plot_ID_tree if you have more than one plot in your data")
  
  # when the plot is not a rectangle
  rel_coord <- data.frame(X=c(0,100,0,110),y=c(0,0,100,100))
  expect_error(divide_plot(rel_coord, grid_size = 25) , "BIOMASS package can't deal with non-rectangular plot")
  
})


test_that("divide_plot on relative coordinates only", {
  
  # Test when rel_coord is a matrix (colnames aren't supplied)
  rel_coord <- matrix(c(0,100,0,100,0,0,100,100), ncol=2)
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
  expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x=c(0,50,0,50), y=c(0,0,50,50)))
  
  # Test when rel_coord is a data.table
  rel_coord <- data.table(expand.grid(x_field = c(0, 100), y_field = c(0, 100)))
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
  expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x_field=c(0,50,0,50), y_field=c(0,0,50,50)))
  
  # Test when rel_coord is a data.frame 
  rel_coord <- expand.grid(x_field = c(0, 100), y_field = c(0, 100))
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
  expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x_field=c(0,50,0,50), y_field=c(0,0,50,50)))
  
  # Test rectangular division
  rect_subplots <- divide_plot(rel_coord, grid_size = c(25,50))
  expect_equivalent(rect_subplots[29:32,] , data.frame(subplot_id=rep("subplot_3_1",4),x_field=c(75,100,75,100), y_field=c(50,50,100,100)))
  
  # Test when the origin is not (0;0)
  rel_coord_2 <- expand.grid(x_field = c(10, 110), y_field = c(10, 110))
  subplots <- divide_plot(rel_coord = rel_coord_2, grid_size = 50)
  expect_equivalent(subplots[13:16,] , data.frame(subplot_id=rep("subplot_1_1",4),x=c(60,110,60,110), y=c(60,60,110,110)))
  
  # Test multiple plots
  multiple_rel_coord <- rbind(rel_coord , rel_coord_2)
  multiple_rel_coord$plotID <- rep(c("plot1","plot2"),e=4)
  multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x_field","y_field")], grid_size = 50, plot_ID_corner = multiple_rel_coord$plotID)
  expect_equivalent(multiple_subplots[29:32,] , data.frame(plotID=rep("plot2",4),subplot_id=rep("plot2_1_1",4),x_field=c(60,110,60,110), y_field=c(60,60,110,110)))
  
  # Test rectangular plot
  rel_coord <- expand.grid(x = c(0, 100), y = c(0, 50))
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = c(50,25))
  #ggplot(subplots,aes(x=x,y=y,label=subplot_id)) + geom_point() + geom_text(position = position_jitter()) + coord_equal()
  expect_equivalent(subplots[5:8,] , data.frame(subplot_id=rep("subplot_1_0",4),x=c(50,100,50,100), y=c(0,0,25,25)))
})
  
test_that("divide_plot with projected coordinates", {
  
  # Test when proj_coord is a matrix (colnames aren't supplied)
  rel_coord <- expand.grid(x = c(0, 100), y = c(0, 100))
  rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
  proj_coord <- as.matrix(rel_coord) %*% rot_mat
  proj_coord <- sweep(proj_coord, 2, c(110,110), FUN = "+")
  
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
  #ggplot(subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x,y=y)) + geom_text(position = position_jitter()) + coord_equal()
  expect_equivalent(subplots[13:16,c("subplot_id","x_proj","y_proj")] ,
                    data.frame(subplot_id="subplot_1_1",
                               x_proj=c(128.3013,171.6025,103.3013,146.6025),
                               y_proj=c(178.3013,203.3013,221.6025,246.6025)),
                    tol = 1e-5)
  
  # Test when proj_coord is a data.table
  proj_coord <- as.data.table(proj_coord)
  colnames(proj_coord) <- c("Xproj","Yproj")
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
  expect_equivalent(subplots[13:16,c("subplot_id","Xproj","Yproj")] ,
                    data.frame(subplot_id="subplot_1_1",
                               Xproj=c(128.3013,171.6025,103.3013,146.6025),
                               Yproj=c(178.3013,203.3013,221.6025,246.6025)),
                    tol = 1e-5)
  
  # Test when proj_coord is a data.frame
  proj_coord <- as.data.frame(proj_coord)
  colnames(proj_coord) <- c("X_proj","Y_proj")
  subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
  expect_equal(names(subplots) , c("subplot_id", "x", "y", "X_proj", "Y_proj"))
  
  # Test rectangular plot with rectangular division
  rel_coord <- expand.grid(x = c(0, 100), y = c(0, 75))
  proj_coord <- as.matrix(rel_coord) %*% rot_mat
  proj_coord <- sweep(proj_coord, 2, c(110,110), FUN = "+")
  rect_subplots <- divide_plot(rel_coord, grid_size = c(50,25), proj_coord = proj_coord)
  #ggplot(rect_subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x,y=y)) + geom_text(position = position_jitter()) + coord_equal()
  expect_equivalent(rect_subplots[21:24,c(1,4,5)],
                    data.frame(subplot_id=rep("subplot_1_2",4), x_proj=c(128.3013,171.6025,115.8013,159.1025), y_proj=c(178.3013,203.3013,199.9519,224.9519)),
                    tol=1e-5)
  
  # Test when the origin is the NE corner :
  inv_proj_coord <- proj_coord[c(4,3,2,1),]
  rect_subplots <- divide_plot(rel_coord, grid_size = c(50,25), proj_coord = inv_proj_coord)
  #ggplot(rect_subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x,y=y)) + geom_text(position = position_jitter()) + coord_equal()
  expect_equivalent(rect_subplots[1:4,c(1,4,5)],
                    data.frame(subplot_id=rep("subplot_0_0",4),
                               x_proj=c(128.3013,171.6025,115.8013,159.1025)[c(4,3,2,1)],
                               y_proj=c(178.3013,203.3013,199.9519,224.9519)[c(4,3,2,1)]),
                    tol=1e-5)
  
  # Test multiple plots
  rel_coord_1 <- expand.grid(x = c(0, 150), y = c(0, 100))
  proj_coord_1 <- as.matrix(rel_coord_1) %*% rot_mat
  proj_coord_1 <- sweep(proj_coord_1, 2, c(250,110), FUN = "+")
  rel_coord_2 <- expand.grid(x = c(0, 100), y = c(0, 100))
  proj_coord_2 <- as.matrix(rel_coord_2) %*% rot_mat
  proj_coord_2 <- sweep(proj_coord_2, 2, c(60,110), FUN = "+")
  proj_coord_2 <- proj_coord_2[c(4,3,2,1),]
  
  multiple_rel_coord <- rbind(rel_coord_1 , rel_coord_2)
  multiple_proj_coord <- rbind(proj_coord_1,proj_coord_2)
  multiple_rel_coord$plotID <- rep(c("plot1","plot2"),e=4)
  multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x","y")], grid_size = 50, proj_coord = multiple_proj_coord, plot_ID_corner = multiple_rel_coord$plotID)
  #ggplot(multiple_subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x,y=y)) + geom_text(position = position_jitter()) + coord_equal()
  expect_equivalent(multiple_subplots[c(24,40),] ,
                    data.frame(plotID=c("plot1","plot2"),subplot_id=c("plot1_2_1","plot2_1_1"),
                               x=c(150,100), y=c(100,100), x_proj=c(329.9038,60), y_proj=c(271.6025,110)),
                    tol=1e-5)
  
})

test_that("divide_plot with tree coordinates", {
  
  rel_coord_1 <- expand.grid(x = c(0, 100), y = c(0, 100))
  
  # Test warning when a tree is not in any subplot
  expect_warning(divide_plot(rel_coord = rel_coord_1, grid_size = 50, tree_coord = data.frame(x=110,y=110)))
  
  tree_coord_1 <- expand.grid(X = seq(0,100,10), Y = seq(0,100,10))
  subplots <- divide_plot(rel_coord = rel_coord_1, grid_size = 50, tree_coord = tree_coord_1)
  expect_equal(subplots$tree_coord$subplot_id[c(1,6,56,61)],c("subplot_0_0","subplot_1_0","subplot_0_1","subplot_1_1"))
  
  # Test with multiple columns for tree_coord
  tree_coord_1[c("H","AGB")] <- data.frame(H = rnorm(nrow(tree_coord_1),10,3), AGB = runif(nrow(tree_coord_1),0,2))
  subplots <- divide_plot(rel_coord = rel_coord_1, grid_size = 50, tree_coord = tree_coord_1)
  expect_equal(names(subplots$tree_coord) , c("X","Y","subplot_id","H","AGB"))
  
  # Test with multiple plots
  rel_coord_2 <- expand.grid(x = c(0, 100), y = c(0, 100))
  multiple_rel_coord <- rbind(rel_coord_1 , rel_coord_2)
  plot_ID_corner <- rep(c("plot1","plot2"),e=4)
  tree_coord_2 <- expand.grid(X = seq(5,95,20), Y = seq(5,95,20))
  tree_coord_2[c("H","AGB")] <- data.frame(H = rnorm(nrow(tree_coord_2),10,3), AGB = runif(nrow(tree_coord_2),0,2))
  multiple_tree_coord <- rbind(tree_coord_1,tree_coord_2)
  plot_ID_tree <- c(rep("plot1",nrow(tree_coord_1)),rep("plot3",nrow(tree_coord_2)))
  
  # Test error
  expect_error(
    divide_plot(rel_coord = multiple_rel_coord[,c("x","y")], grid_size = 50, plot_ID_corner = plot_ID_corner,
                tree_coord = multiple_tree_coord, plot_ID_tree = plot_ID_tree) , 
               "Some plot_ID_tree are not found in plot_ID_corner")
  
  plot_ID_tree <- c(rep("plot1",nrow(tree_coord_1)),rep("plot2",nrow(tree_coord_2)))
  
  multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x","y")], grid_size = 50,
                                   plot_ID_corner = plot_ID_corner,
                                   tree_coord = multiple_tree_coord, plot_ID_tree = plot_ID_tree)
  
  expect_equal(multiple_subplots$tree_coord$subplot_id[c(122,125,137,140)],
               c("plot2_0_0","plot2_1_0","plot2_0_1","plot2_1_1"))
  expect_equal(names(multiple_subplots$tree_coord) , c("X","Y","plot_id","subplot_id","H","AGB"))
  
  
})