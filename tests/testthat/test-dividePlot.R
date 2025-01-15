# context("divide Plot")
# 
# test_that("divide_plot error", {
#   
#   rel_coord <- expand.grid(X = c(0, 100), Y = c(0, 100))
#   
#   expect_error(divide_plot(rel_coord, grid_size = 25, corner_plot_ID = "plot"), "The length of corner_plot_ID and the number of rows of rel_coord are different")
#   expect_error(divide_plot(rel_coord, grid_size = c(25,25,25)), "you must apply yourself the function for each plot")
#   expect_error(divide_plot(rbind(rel_coord,c(0,0)),grid_size = 25), "rel_coord does'nt contain exactly 4 corners by plot")
#   expect_error(divide_plot(rbind(rel_coord,rel_coord),grid_size = 25), "You must supply corner_plot_ID if you have more than one plot in your data")
#   expect_error(divide_plot(rbind(rel_coord,rel_coord), grid_size = 25, corner_plot_ID = rep(1:4,2)), "corner_plot_ID must contain the same number of plots as rel_coord")
#   
#   corner_proj_coord <- sweep(rel_coord[1:3,], 2, c(150,150), "+")
#   expect_error(divide_plot(rel_coord, proj_coord = corner_proj_coord ,grid_size = 25), "rel_coord and proj_coord don't have the same number of rows")
#   
#   expect_error(divide_plot(rel_coord, grid_size = 25, tree_df = c(10,10)), "tree_df must be a matrix or a data frame")
#   tree_df <- data.frame(x =runif(100,0,100), y=runif(100,0,100))
#   expect_error(divide_plot(rel_coord, grid_size = 25, tree_df = tree_df), "You must supply the column names of relative coordinates of the trees using the tree_coords argument")
#   expect_error(divide_plot(rel_coord, grid_size = 25, tree_df = tree_df, tree_coords = c("x_rel","y_rel")), "tree_coords are not found in tree_df colunm names")
#   
#   expect_warning(divide_plot(rel_coord, grid_size = c(30,25)) , "The x-dimension of the plot is not a multiple of the x-dimension of the grid size")
#   expect_warning(divide_plot(rel_coord, grid_size = c(25,30)) , "The y-dimension of the plot is not a multiple of the y-dimension of the grid size")
#   expect_error(suppressWarnings(divide_plot(rel_coord, grid_size = c(40,40))) , "If you still want to divide the plot, please increase the value of the grid_tol argument.")
#   
#   expect_error(divide_plot(rbind(rel_coord,rel_coord), grid_size = 25, corner_plot_ID = rep(1:2,e=4) , tree_df = tree_df, tree_coords = c("x","y")), "You must supply tree_plot_ID if you have more than one plot in your data")
#   
#   # when the plot is not a rectangle
#   rel_coord <- data.frame(X=c(0,100,0,110),y=c(0,0,100,100))
#   expect_error(divide_plot(rel_coord, grid_size = 25) , "BIOMASS package can't deal with non-rectangular plot")
# })
# 
# test_that("divide_plot on relative coordinates only", {
#   
#   # Test when rel_coord is a matrix (colnames aren't supplied)
#   rel_coord <- matrix(c(0,100,0,100,0,0,100,100), ncol=2)
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
#   expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x_rel=c(0,50,50,0), y_rel=c(0,0,50,50)))
#   
#   # Test when rel_coord is a data.table
#   rel_coord <- data.table(expand.grid(x_field = c(0, 100), y_field = c(0, 100)))
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
#   expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x_rel=c(0,50,50,0), y_rel=c(0,0,50,50)))
#   
#   # Test when rel_coord is a data.frame 
#   rel_coord <- expand.grid(x_field = c(0, 100), y_field = c(0, 100))
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
#   expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x_rel=c(0,50,50,0), y_rel=c(0,0,50,50)))
# 
#   # Test when rel_coord contains more than 2 columns 
#   rel_coord$other_col <- rnorm(ncol(rel_coord))
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50)
#   expect_equal(subplots[1:4,] , data.frame(subplot_id=rep("subplot_0_0",4),x_rel=c(0,50,50,0), y_rel=c(0,0,50,50)))
# 
#   # Test rectangular division
#   rect_subplots <- divide_plot(rel_coord, grid_size = c(25,50))
#   expect_equivalent(rect_subplots[29:32,] , data.frame(subplot_id=rep("subplot_3_1",4),x_rel=c(75,100,100,75), y_rel=c(50,50,100,100)))
#   
#   # Test when the origin is not (0;0)
#   rel_coord_2 <- expand.grid(x_field = c(10, 110), y_field = c(10, 110))
#   subplots <- divide_plot(rel_coord = rel_coord_2, grid_size = 50)
#   expect_equivalent(subplots[13:16,] , data.frame(subplot_id=rep("subplot_1_1",4),x=c(60,110,110,60), y=c(60,60,110,110)))
#   
#   # Test multiple plots
#   rel_coord$other_col <- NULL
#   multiple_rel_coord <- rbind(rel_coord , rel_coord_2)
#   multiple_rel_coord$plotID <- rep(c("plot1","plot2"),e=4)
#   multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x_field","y_field")], grid_size = 50, corner_plot_ID = multiple_rel_coord$plotID)
#   expect_equivalent(multiple_subplots[29:32,] , data.frame(plotID=rep("plot2",4),subplot_id=rep("plot2_1_1",4),x_rel=c(60,110,110,60), y_field=c(60,60,110,110)))
#   
#   # Test rectangular plot
#   rel_coord <- expand.grid(x = c(0, 100), y = c(0, 50))
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = c(50,25))
#   #ggplot(subplots,aes(x=x_rel,y=y,label=subplot_id)) + geom_point() + geom_text(position = position_jitter()) + coord_equal()
#   expect_equivalent(subplots[5:8,] , data.frame(subplot_id=rep("subplot_1_0",4),x=c(50,100,100,50), y=c(0,0,25,25)))
# 
#   # Test non-adjusted grid 
#   subplots <- suppressWarnings(divide_plot(rel_coord = rel_coord, grid_size = c(45,20), grid_tol = 0.3, centred_grid = T))
#   expect_equivalent(subplots[5:8,] , data.frame(subplot_id=rep("subplot_1_0",4),x=c(50,95,95,50), y=c(5,5,25,25)))
# })
#   
# test_that("divide_plot with projected coordinates", {
#   
#   # Test when proj_coord is a matrix (colnames aren't supplied)
#   rel_coord <- expand.grid(x = c(0, 100), y = c(0, 100))
#   rot_mat <- matrix(c(cos(-pi/6),sin(-pi/6),-sin(-pi/6),cos(-pi/6)),nrow=2)
#   proj_coord <- as.matrix(rel_coord) %*% rot_mat
#   proj_coord <- sweep(proj_coord, 2, c(110,110), FUN = "+")
#   
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
#   #ggplot(subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x_rel,y=y_rel)) + geom_text(position = position_jitter()) + coord_equal()
#   expect_equivalent(subplots[13:16,c("subplot_id","x_proj","y_proj")] ,
#                     data.frame(subplot_id="subplot_1_1",
#                                x_proj=c(128.3013,171.6025,146.6025,103.3013),
#                                y_proj=c(178.3013,203.3013,246.6025,221.6025)),
#                     tol = 1e-5)
#   
#   # Test when proj_coord is a data.table
#   proj_coord <- as.data.table(proj_coord)
#   colnames(proj_coord) <- c("Xproj","Yproj")
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
#   expect_equivalent(subplots[13:16,c("subplot_id","x_proj","y_proj")] ,
#                     data.frame(subplot_id="subplot_1_1",
#                                x_proj=c(128.3013,171.6025,146.6025,103.3013),
#                                y_proj=c(178.3013,203.3013,246.6025,221.6025)),
#                     tol = 1e-5)
#   
#   # Test when proj_coord is a data.frame
#   proj_coord <- as.data.frame(proj_coord)
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
#   expect_equal(names(subplots) , c("subplot_id", "x_rel", "y_rel", "x_proj", "y_proj"))
# 
#   # Test when proj_coord contains more than 2 columns 
#   proj_coord$other_col <- rnorm(ncol(proj_coord))
#   subplots <- divide_plot(rel_coord = rel_coord, grid_size = 50, proj_coord = proj_coord)
#   expect_equal(names(subplots) , c("subplot_id", "x_rel", "y_rel", "x_proj", "y_proj"))
# 
#   # Test rectangular plot with rectangular division
#   rel_coord <- expand.grid(x = c(0, 100), y = c(0, 75))
#   proj_coord <- as.matrix(rel_coord) %*% rot_mat
#   proj_coord <- sweep(proj_coord, 2, c(110,110), FUN = "+")
#   rect_subplots <- divide_plot(rel_coord, grid_size = c(50,25), proj_coord = proj_coord)
#   #ggplot(rect_subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x_rel,y=y_rel)) + geom_text(position = position_jitter()) + coord_equal()
#   expect_equivalent(rect_subplots[21:24,c(1,4,5)],
#                     data.frame(subplot_id=rep("subplot_1_2",4), x_proj=c(128.3013,171.6025,159.1025,115.8013), y_proj=c(178.3013,203.3013,224.9519,199.9519)),
#                     tol=1e-5)
#   
#   # Test when the origin is the NE corner :
#   inv_proj_coord <- proj_coord[c(4,3,2,1),]
#   rect_subplots <- divide_plot(rel_coord, grid_size = c(50,25), proj_coord = inv_proj_coord)
#   #ggplot(rect_subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x_rel,y=y_rel)) + geom_text(position = position_jitter()) + coord_equal()
#   expect_equivalent(rect_subplots[1:4,c(1,4,5)],
#                     data.frame(subplot_id=rep("subplot_0_0",4),
#                                x_proj=c(128.3013,171.6025,159.1025,115.8013)[c(3,4,1,2)],
#                                y_proj=c(178.3013,203.3013,224.9519,199.9519)[c(3,4,1,2)]),
#                     tol=1e-5)
#   
#   # Test multiple plots
#   rel_coord_1 <- expand.grid(x = c(0, 150), y = c(0, 100))
#   proj_coord_1 <- as.matrix(rel_coord_1) %*% rot_mat
#   proj_coord_1 <- sweep(proj_coord_1, 2, c(250,110), FUN = "+")
#   rel_coord_2 <- expand.grid(x = c(0, 100), y = c(0, 100))
#   proj_coord_2 <- as.matrix(rel_coord_2) %*% rot_mat
#   proj_coord_2 <- sweep(proj_coord_2, 2, c(60,110), FUN = "+")
#   proj_coord_2 <- proj_coord_2[c(4,3,2,1),]
#   
#   multiple_rel_coord <- rbind(rel_coord_1 , rel_coord_2)
#   multiple_proj_coord <- rbind(proj_coord_1,proj_coord_2)
#   multiple_rel_coord$plotID <- rep(c("plot1","plot2"),e=4)
#   multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x","y")], grid_size = 50, proj_coord = multiple_proj_coord, corner_plot_ID = multiple_rel_coord$plotID)
#   #ggplot(multiple_subplots,aes(x=x_proj,y=y_proj,label=subplot_id)) + geom_point() + geom_point(aes(x=x_rel,y=y_rel)) + geom_text(position = position_jitter()) + coord_equal()
#   expect_equivalent(multiple_subplots[c(24,40),] ,
#                     data.frame(plotID=c("plot1","plot2"),subplot_id=c("plot1_2_1","plot2_1_1"),
#                                x_rel=c(100,50), y_rel=c(100,100),
#                                x_proj=c(286.6025,103.3013), y_proj=c(246.6025,135.0000)),
#                     tol=1e-5)
#   
# })
# test_that("divide_plot with tree coordinates", {
#   
#   rel_coord_1 <- expand.grid(x = c(0, 100), y = c(0, 100))
#   
#   # Test warning when a tree is not in any subplot
#   expect_warning(divide_plot(rel_coord = rel_coord_1, grid_size = 50, tree_df = data.frame(x=110,y=110), tree_coords = c("x","y")))
#   
#   tree_df_1 <- expand.grid(X = seq(0,100,10), Y = seq(0,100,10))
#   subplots <- divide_plot(rel_coord = rel_coord_1, grid_size = 50, tree_df = tree_df_1, tree_coords = c("X","Y"))
#   expect_equal(subplots$tree_df$subplot_id[c(1,6,56,61)],c("subplot_0_0","subplot_1_0","subplot_0_1","subplot_1_1"))
#   
#   # Test with multiple columns for tree_df
#   tree_df_1[c("H","AGB")] <- data.frame(H = rnorm(nrow(tree_df_1),10,3), AGB = runif(nrow(tree_df_1),0,2))
#   subplots <- divide_plot(rel_coord = rel_coord_1, grid_size = 50, tree_df = tree_df_1, tree_coords = c("X","Y"))
#   expect_equal(names(subplots$tree_df) , c("X","Y","H","AGB","subplot_id"))
#   
#   # Test with multiple plots
#   rel_coord_2 <- expand.grid(x = c(0, 100), y = c(0, 100))
#   multiple_rel_coord <- rbind(rel_coord_1 , rel_coord_2)
#   corner_plot_ID <- rep(c("plot1","plot2"),e=4)
#   tree_df_2 <- expand.grid(X = seq(5,95,20), Y = seq(5,95,20))
#   tree_df_2[c("H","AGB")] <- data.frame(H = rnorm(nrow(tree_df_2),10,3), AGB = runif(nrow(tree_df_2),0,2))
#   multiple_tree_df <- rbind(tree_df_1,tree_df_2)
#   tree_plot_ID <- c(rep("plot1",nrow(tree_df_1)),rep("plot3",nrow(tree_df_2)))
#   
#   # Test error
#   expect_error(
#     divide_plot(rel_coord = multiple_rel_coord[,c("x","y")], grid_size = 50,
#                 tree_df = multiple_tree_df , tree_coords = c("X","Y"), 
#                 corner_plot_ID = corner_plot_ID, tree_plot_ID = tree_plot_ID) , 
#                "Some tree_plot_ID are not found in corner_plot_ID")
#   
#   tree_plot_ID <- c(rep("plot1",nrow(tree_df_1)),rep("plot2",nrow(tree_df_2)))
#   
#   multiple_subplots <- divide_plot(rel_coord = multiple_rel_coord[,c("x","y")], grid_size = 50,
#                                    tree_df = multiple_tree_df , tree_coords = c("X","Y"), 
#                                    corner_plot_ID = corner_plot_ID, tree_plot_ID = tree_plot_ID) 
#   
#   expect_equal(multiple_subplots$tree_df$subplot_id[c(122,125,137,140)],
#                c("plot2_0_0","plot2_1_0","plot2_0_1","plot2_1_1"))
#   expect_equal(names(multiple_subplots$tree_df) , c("X","Y","H","AGB","plot_id","subplot_id"))
#   
#   
# })
