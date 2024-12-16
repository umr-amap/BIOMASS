set.seed(2)

proj_coord <- data.frame(
  X = c(runif(5, min = 9, max = 11), runif(5, min = 8, max = 12), runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)),
  Y = c(runif(5, min = 9, max = 11), runif(5, min = 80, max = 120), runif(5, min = 8, max = 12), runif(5, min = 90, max = 110))
)
proj_coord$X <- proj_coord$X + 200000
proj_coord$Y <- proj_coord$Y + 9000000

rel_coord <- data.frame(
  X = c(rep(0, 10), rep(100, 10)),
  Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
)
corner_ID <- rep(c("SW","NW","SE","NE"),e=5)

context("Check plot coordinates")
test_that("check_plot_coord error", {
  
  corner_data <- data.frame(
    x_proj = c( rnorm(5, 0, 5), rnorm(5, 0, 5),
                rnorm(5, 100, 5), rnorm(5, 100, 5)) + 1000,
    y_proj = c( rnorm(5, 0, 5), rnorm(5, 100, 5),
                rnorm(5, 0, 5), rnorm(5, 100, 5)) + 1000,
    x_rel = c(rep(0, 10), rep(100, 10)),
    y_rel = c(rep(c(rep(0, 5), rep(100, 5)), 2)),
    corner_ID = rep(c("SW","NW","SE","NE"),e=5) )
  
  expect_error(check_plot_coord(proj_coord=corner_data[c("x_proj","y_proj")], rel_coord=corner_data[c("x_rel","y_rel")]),
               "The way in which arguments are supplied to the function has changed since version 2.2.1. You now have to supply corner_data data frame and it associated coordinates variable names.")
  expect_error(check_plot_coord(corner_data = as.matrix(corner_data), rel_coord = c("x_rel","y_rel")),
               "corner_data must a data frame or a data frame extension")
  expect_error(check_plot_coord(corner_data = corner_data, rel_coord = c("x_rel","y_rel")),
               "You must supply the name of at least one set of coordinates : longlat or proj_coord")
  expect_error(check_plot_coord(corner_data = corner_data, proj_coord = c("xproj","yproj"), rel_coord = c("x_rel","y_rel")),
               "column names supplied by proj_coord are not found in corner_data")
  expect_error(check_plot_coord(corner_data = corner_data, longlat = c("xproj","yproj"), rel_coord = c("x_rel","y_rel")),
               "column names supplied by longlat are not found in corner_data")
  
  
  
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=c(0,0)),"rel_coord must be a matrix or a data frame")
  expect_error(check_plot_coord(corner_data, longlat=c(0,0), rel_coord=rel_coord),"longlat must be a matrix or a data frame")
  expect_error(check_plot_coord(corner_data, proj_coord=c(0,0), rel_coord=rel_coord),"proj_coord must be a matrix or a data frame")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=F, tree_df = c(1,1)),"tree_df must be a matrix or a data frame")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=F, tree_df = data.frame(x=1,y=1)),"You must supply the column names corresponding to the relative coordinates of tree_df using the argument `tree_coords`")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=F,tree_df = data.frame(x=1,y=1), tree_coords = c("a","b")),"column names supplied by tree_coords are not found in tree_df")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=NULL),"The trust_GPS_corners argument must be TRUE or FALSE")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=T, max_dist=c(10,20) ),"The max_dist argument must be of length 1")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rbind(rel_coord, c(40, 40)),trust_GPS_corners=T),"same dimension")
  wrong_rel_coord <- rel_coord ; wrong_rel_coord[1,1] <- NA
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=wrong_rel_coord, trust_GPS_corners=T,corner_ID = corner_ID),"Missing values are detected in rel_coord. Please remove them and call the function again")
  wrong_proj_coord <- proj_coord ; wrong_proj_coord[1,1] <- NA
  expect_error(check_plot_coord(corner_data, proj_coord=wrong_proj_coord, rel_coord=rel_coord, trust_GPS_corners=T,corner_ID = corner_ID),"Missing values are detected in proj_coord. Please remove them and call the function again")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=T),"The argument corner_ID is needed if trust_GPS_corners is TRUE and if multiple measurements of each corner have been realized")
  wrong_rel_coord[1,1] <- 10
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=wrong_rel_coord, trust_GPS_corners=T,corner_ID = corner_ID),"rel_coord instead of 4")
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=T,corner_ID = c("a","b")),"corner_ID must be the same length as the number of rows in rel_coord")
  wrong_corner_ID <- corner_ID ; wrong_corner_ID[1] <- "SN"
  expect_error(check_plot_coord(corner_data, proj_coord=proj_coord, rel_coord=rel_coord, trust_GPS_corners=T,corner_ID = wrong_corner_ID),"corner_ID instead of 4")
})

test_that("check_plot_coord outputs and outliers", {
  # with max dist equal 10
  expect_warning(
    check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = F, corner_ID = corner_ID, draw_plot = F, max_dist = 10),"Be carefull, you may have GNSS measurement outliers"
  )
  outputs <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, max_dist = 10)
  expect_is(outputs, "list")
  expect_length(outputs, 3)
  expect_equal(names(outputs), c("corner_coord", "polygon", "outliers"))
  expect_is(outputs$corner_coord, "data.frame")
  expect_is(outputs$polygon, "sfc_POLYGON")
  expect_is(outputs$outliers, "data.frame")
  
  expect_equal(dim(outputs$outliers), c(10,3))
  expect_equal(dim(outputs$corner_coord), c(4, 5))
  
  # with max dist equal 25 there isn't outliers anymore
  expect_failure(expect_warning(
    check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = F, corner_ID = corner_ID, draw_plot = F, max_dist = 25),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  
  # with rm_outliers = TRUE
  expect_failure(expect_warning(
    check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, max_dist = 25),
    "Be carefull, you may have GNSS measurement outliers"
  ))
  outputs_2 <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, max_dist = 15)
  
  expect_failure(expect_equal(outputs$corner_coord, outputs_2$corner_coord))
  expect_failure(expect_equal(outputs$polygon, outputs_2$polygon))
})


test_that("check_plot_coord in long lat", {
  longlat <- as.data.frame(proj4::project(proj_coord,
                                          proj = "+proj=utm +zone=50 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                          inverse = TRUE
  ))
  outputs_longlat <- check_plot_coord(corner_data, longlat = longlat, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, max_dist = 10)
  outputs_proj_coord <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, max_dist = 10)
  expect_is(outputs_longlat, "list")
  expect_length(outputs_longlat, 4)
  expect_equal(names(outputs_longlat), c("corner_coord", "polygon", "outliers","UTM_code"))
  expect_equal(outputs_longlat[1:3],outputs_proj_coord)
})

test_that("check_plot_coord, trust_GPS_corners", {
  proj_coord0 <- data.frame(
    X = rep(c(10,10,100,100), e=7) + 200000,
    Y = rep(c(10,100,100,10), e=7) + 9000000
  )
  proj_coord <- proj_coord0
  proj_coord$X <- proj_coord0$X  + rep(sample(-3:3),4)
  proj_coord$Y <- proj_coord0$Y + rep(sample(-3:3),4)
  rel_coord <- data.frame(
    X = rep(c(0,0,95,95),e=7),
    Y = rep(c(0,95,95,0),e=7)
  )
  corner_ID <- rep(c("SW","NW","SE","NE"),e=7)
  
  expect_warning(
    check_plot_coord(corner_data, proj_coord = proj_coord[-(1:3),], rel_coord = rel_coord[-(1:3),], trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID[-(1:3)], draw_plot = F),"At least one corner has less than 5 measurements. We suggest using the argument trust_GPS_corners = FALSE"
  )
  res_trust_GPS_corners_T <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID, draw_plot = F)
  expect_equivalent(res_trust_GPS_corners_T$corner_coord[,1:2] , unique(proj_coord0)[c(1,4,3,2),])
  
  res_trust_GPS_corners_F <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = F, rm_outliers = T, corner_ID = corner_ID, draw_plot = F)
  expect_equivalent(res_trust_GPS_corners_T$corner_coord[,3:5] , res_trust_GPS_corners_F$corner_coord[,3:5])
  expect_equivalent(res_trust_GPS_corners_T$corner_coord[,1:2], round(res_trust_GPS_corners_F$corner_coord[,1:2],digits = -1))
  
  # Test when there's only 4 measures and trust_GPS_corners = F
  expect_equal(res_trust_GPS_corners_F , check_plot_coord(corner_data, proj_coord = proj_coord0[c(1,8,15,22),], rel_coord = rel_coord[c(1,8,15,22),], trust_GPS_corners = F, rm_outliers = T, draw_plot = F, corner_ID = corner_ID[c(1,8,15,22)]))
})

test_that("check_plot_coord, origin corner", {
  proj_coord <- data.frame(
    X = rep(c(0,0,100,100), e=7) + 200000 + rep(sample(-3:3),4),
    Y = rep(c(0,100,100,0), e=7) + 9000000 + rep(sample(-3:3),4)
  )
  corner_ID <- rep(c("SW","NW","SE","NE"),e=7)
  
  # Test when the origin is not the South-East corner
  rel_coord_NE <- data.frame(
    X = rep(c(100,100,0,0),e=7),
    Y = rep(c(100,0,0,100),e=7)
  )
  rel_coord_SW <- data.frame(
    X = rep(c(0,0,100,100),e=7),
    Y = rep(c(0,100,100,0),e=7)
  )
  res_NE <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord_NE, trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID, draw_plot = F)
  res_SW <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord_SW, trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID, draw_plot = F)
  
  expect_equivalent(res_SW$corner_coord[c("x_rel","y_rel")] , res_NE$corner_coord[c("x_rel","y_rel")])
  expect_equivalent(res_SW$corner_coord[c("x_proj","y_proj","corner_ID")] , res_NE$corner_coord[c(3,4,1,2),c("x_proj","y_proj","corner_ID")])
  
  # Test when the origin of the relative coordinates is not(0;0)
  rel_coord_SW <- rel_coord_SW + 200
  res_SW_200 <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord_SW, trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID, draw_plot = F)
  expect_equal(res_SW$corner_coord[c("x_proj","y_proj","corner_ID")] , res_SW_200$corner_coord[c("x_proj","y_proj","corner_ID")])
  expect_equal(res_SW$corner_coord[c("x_rel","y_rel")]+200 , res_SW_200$corner_coord[c("x_rel","y_rel")])
})


test_that("check_plot_coord, tree coordinates", {
  proj_coord <- data.frame(
    X = rep(c(0,0,100,100), e=7) + 200000 + rep(sample(-3:3),4),
    Y = rep(c(0,100,100,0), e=7) + 9000000 + rep(sample(-3:3),4)
  )
  rel_coord <- data.frame(
    X = rep(c(0,0,100,100),e=7),
    Y = rep(c(0,100,100,0),e=7)
  )
  corner_ID <- rep(c("SW","NW","SE","NE"),e=7)
  tree_df <- data.frame(
    X = c(10,20,30),
    Y = c(10,20,30)
  )
  res <- check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, tree_df = tree_df, tree_coords = c("X","Y"))
  expect_is(res$tree_proj_coord, "data.frame")
  expect_equal(dim(res$tree_proj_coord), c(nrow(tree_df),2))
  expect_equivalent(res$tree_proj_coord , sweep(tree_df, 2, c(200000,9000000), FUN = "+"))
  tree_df$AGB <- c(20,25,30)  
  expect_equal(res, check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = T, rm_outliers = T, corner_ID = corner_ID, draw_plot = F, tree_df = tree_df, tree_coords = c("X","Y")))
})

test_that("check_plot_coord, splashed corners", {
  proj_coord <- data.frame(
    X = rep(c(0,0,100,100), e=4),
    Y = rep(c(0,100,100,0), e=4)
  )
  proj_coord[1:4,] <- matrix(c(25,0,-25,0,0,25,0,-25), ncol=2, byrow = F)
  proj_coord$X = proj_coord$X + 200000
  proj_coord$Y = proj_coord$Y + 9000000
  
  rel_coord <- data.frame(
    X = rep(c(0,0,100,100),e=4),
    Y = rep(c(0,100,100,0),e=4)
  )
  corner_ID <- rep(c("SW","NW","SE","NE"),e=4)
  expect_error(suppressWarnings(check_plot_coord(corner_data, proj_coord = proj_coord, rel_coord = rel_coord, trust_GPS_corners = T, corner_ID = corner_ID, rm_outliers = T, draw_plot = F, max_dist = 5)))
})
