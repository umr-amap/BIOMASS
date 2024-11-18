#' Divides one ore more plots into subplots
#'
#' @description
#' This function divides a plot (or several plots) into subplots in the relative coordinates system, and returns the coordinates of subplot corners.
#'
#' @details
#'  If corner coordinates in the projected coordinate system are supplied (proj_coord), projected coordinates of subplot corners are calculated by a bilinear interpolation in relation with relative coordinates of plot corners. Be aware that this bilinear interpolation only works if the plot in the relative coordinates system is rectangular (ie, has 4 right angles).
#' 
#' @param rel_coord a data frame containing the relative (local) coordinates of plot corners, with X and Y corresponding to the first and second column respectively
#' @param proj_coord a data frame containing the projected coordinates of plot corners, with X and Y corresponding to the first and second column respectively, and with the same row order than rel_coord
#' @param grid_size a vector indicating the dimensions of grid cells (resp. X and Y dimensions). If only one value is given, grid cells will be considered as squares.
#' @param plot_ID_corner if dealing with multiple plots : a vector indicating plot IDs for corners.
#' @param tree_coord a data frame containing at least the relative tree coordinates (field/local coordinates), with X and Y corresponding to the first and second columns respectively
#' @param plot_ID_tree if dealing with multiple plots : a vector indicating tree plot IDs.
#'
#' @return If tree_coord isn't supplied, returns a data-frame containing as many rows as there are corners corresponding to the subplots, and the following columns :
#'   - `plot_ID_corner`: If dealing with multiple plots : the plot code
#'   - `subplot_id`: The automatically generated subplot code, using the following rule : subplot_X_Y 
#'   - `x_rel` and `y_rel` (or the column names provided by rel_coord) : the relative X-axis and Y-axis coordinates of subplots corners. 
#'   - `x_proj` and `y_proj` (or the column names provided by proj_coord) :  if proj_coord is supplied, the projected X-axis and Y-axis coordinates of subplots corners
#'   
#'   If tree_coord is supplied, returns a list containing the previous data-frame and a data-frame containing for each tree : 
#'   - the relative coordinates
#'   - the subplot_id associated
#'   - the rest of tree_coord columns supplied
#'
#' @export
#' @author Arthur PERE, Arthur BAILLY
#' @importFrom data.table data.table := setnames setcolorder 
#' @examples
#'
#' # Rectangular plot and grid cells
#' rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 100, 100))
#' subplots <- divide_plot(rel_coord, grid_size = c(100,50))
#' 
#' # Squared plot
#' rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 200, 200))
#' proj_coord <- data.frame(x_proj = c(110, 190, 60, 145), y_proj = c(110, 160, 196, 245))
#' subplots <- divide_plot(rel_coord, proj_coord = proj_coord, grid_size = 100)
#'
#' tree_coord <- data.frame(x_proj = runif(50,0,200), y_proj = runif(50,0,200))
#' subplots <- divide_plot(rel_coord, proj_coord = proj_coord, grid_size = 100, tree_coord = tree_coord)
#'
#' # Dealing with multiple plots 
#' rel_coord <- rbind(rel_coord, rel_coord)
#' proj_coord <- rbind(proj_coord, proj_coord + 200)
#' tree_coord <- rbind(tree_coord, data.frame(x_proj = runif(50,0,200), y_proj = runif(50,0,200)))
#' plot_ID_corner <- rep(c("plot1","plot2"), e=4)
#' plot_ID_tree <- rep(c("plot1","plot2"), e=50)
#' subplots <- divide_plot(rel_coord, proj_coord, 100, plot_ID_corner, tree_coord, plot_ID_tree)
#' 
divide_plot <- function(rel_coord, proj_coord = NULL, grid_size, plot_ID_corner = NULL, tree_coord = NULL, plot_ID_tree = NULL) {
  
  # Parameters verification ----------------------------------------------------
  if (is.matrix(rel_coord)) {
    rel_coord <- data.frame(x_rel=rel_coord[,1],y_rel=rel_coord[,2])
  }
  if (!is.null(proj_coord) && !is.data.frame(proj_coord)) {
    proj_coord <- data.frame(x_proj=proj_coord[,1], y_proj=proj_coord[,2])
  }
  if (!is.null(plot_ID_corner) && nrow(rel_coord) != length(plot_ID_corner)) {
    stop("The length of plot_ID_corner and the number of rows of rel_coord are different")
  }
  if(!length(grid_size) %in% c(1,2)) {
    stop("The length of grid_size must be equal to 1 or 2\nIf you want to divide several plots with different grid sizes, you must apply yourself the function for each plot")
  }
  if(nrow(rel_coord)%% 4 !=0){
    stop("rel_coord does'nt contain exactly 4 corners by plot")
  }
  if(nrow(rel_coord)!=4 & is.null(plot_ID_corner)){
    stop("You must supply plot_ID_corner if you have more than one plot in your data")
  }
  if(!is.null(plot_ID_corner) && nrow(rel_coord)/length(unique(plot_ID_corner)) !=4 ) {
    stop("plot_ID_corner must contain the same number of plots as rel_coord")
  }
  if(!is.null(proj_coord) && any(dim(proj_coord) != dim(rel_coord))) {
      stop("rel_coord and proj_coord are not of the same dimension")
  }
  if(!is.null(tree_coord) && (!is.matrix(tree_coord) & !is.data.frame(tree_coord))){
    stop("tree_coord must be a matrix or a data frame")
  }
  if(nrow(rel_coord)!=4 & !is.null(tree_coord) & is.null(plot_ID_tree)){
    stop("You must supply plot_ID_tree if you have more than one plot in your data")
  }
  if(!is.null(tree_coord) && !is.null(plot_ID_tree) && nrow(tree_coord)!=length(plot_ID_tree)) {
    stop("The length of plot_ID_tree and the number of rows of tree_coord are different")
  }
  if(nrow(rel_coord)!=4 && !is.null(plot_ID_corner) && !is.null(plot_ID_tree) && any(! unique(plot_ID_tree) %in% unique(plot_ID_corner))) {
    stop("Some plot_ID_tree are not found in plot_ID_corner")
  }
  
  
  # Formatting data  -----------------------------------------------------------
  
  if(length(grid_size)!=2) grid_size = rep(grid_size,2)

  rel_coord <- as.data.table(rel_coord)

  if(!is.null(plot_ID_corner)){
    rel_coord[,plot_ID_corner:=plot_ID_corner]
  } else{
    rel_coord[,plot_ID_corner:=""]
  }
  setcolorder(rel_coord , "plot_ID_corner" , after = 2) # if rel_coord has more than 2 col

  # Sorting rows in a counter-clockwise direction and check for non-rectangular plot
  sort_rows <- function(dat) {
    centroid <- colMeans(dat[,1:2])
    angles <- atan2(dat[[2]] - centroid[2], dat[[1]] - centroid[1])
    dat <- dat[order(angles), ]
    # check for non-rectangular plot : distances between centroid and corners must be equals
    if(!all(abs(dist(rbind(dat[,1:2],as.data.frame.list(centroid)))[c(4,7,9,10)] - mean(dist(rbind(dat[,1:2],as.data.frame.list(centroid)))[c(4,7,9,10)]))<0.1)) {
      stop("The plot in the relative coordinate system is not a rectangle (or a square). BIOMASS package can't deal with non-rectangular plot")
    }
    return(dat)
  }
  
  if(!is.null(proj_coord)) {
    combine_coord <-  cbind(rel_coord[,1:3] , proj_coord)
    combine_coord <- combine_coord[, sort_rows(.SD), by = plot_ID_corner]
  } else {
    combine_coord <- rel_coord[, sort_rows(.SD), by = plot_ID_corner]
  }
  
  # Dividing plots   -----------------------------------------------------------
  
  # Grids the plot from the relative coordinates and calculates the projected coordinates of the grid points.
  divide_plot_fct <- function(dat, grid_size) {
    
    # Controlling for a regular grid
    if((max(dat[[2]])-min(dat[[2]])) %% grid_size[1] != 0) {
      stop("The x-dimension of the plot is not a multiple of the x-dimension of the grid size")
    }
    if((max(dat[[3]])-min(dat[[3]])) %% grid_size[2] != 0) {
      stop("The y-dimension of the plot is not a multiple of the y-dimension of the grid size")
    }
    
    plot_grid <- data.table(as.matrix(expand.grid(
      Xrel = seq(min(dat[[2]]), max(dat[[2]]), by = grid_size[1]),
      Yrel = seq(min(dat[[3]]), max(dat[[3]]), by = grid_size[2])
    )))
    plot_grid[,plot_ID_corner:=unique(dat$plot_ID_corner)]
    
    # Attributing subplots names to each corner and adding shared subplot corners
    plot_grid <- rbindlist(apply(plot_grid[Xrel < max(Xrel) & Yrel < max(Yrel),], 1, function(grid_dat) {
      X <- as.numeric(grid_dat[["Xrel"]])
      Y <- as.numeric(grid_dat[["Yrel"]])
      plot_grid[
        (Xrel == X & Yrel == Y) | (Xrel == X + grid_size[1] & Yrel == Y) | (Xrel == X + grid_size[1] & Yrel == Y + grid_size[2]) | (Xrel == X & Yrel == Y + grid_size[2]),
        .(subplot_id = paste(plot_ID_corner, (X-min(plot_grid$Xrel)) / grid_size[1], (Y-min(plot_grid$Yrel)) / grid_size[2], sep = "_"),Xrel, Yrel)]
    }))
    setnames(plot_grid,2:3,names(combine_coord)[2:3])
    
    # Transformation of relative grid coordinates into projected coordinates if supplied
    if(!is.null(proj_coord)) {
      plot_grid <- cbind(plot_grid,bilinear_interpolation(coord = plot_grid[,2:3] , from_corner_coord = dat[,2:3] , to_corner_coord = dat[,4:5], ordered_corner = T))
    }
    return(plot_grid)
  }
  
  # Apply divide_plot_fct to all plots
  sub_corner_coord <- combine_coord[, divide_plot_fct(.SD, grid_size), by = plot_ID_corner, .SDcols = colnames(combine_coord)]
  
  # Assigning trees to subplots ------------------------------------------------
  if(!is.null(tree_coord)) {
    tree_coord <- as.data.table(tree_coord)
    if(!is.null(plot_ID_tree)){
      tree_coord[,plot_id:=plot_ID_tree]
      setcolorder(tree_coord , "plot_id" , after = 2) # if tree_coord has more than 2 col
    } else{
      tree_coord[,plot_id:=""]
      setcolorder(tree_coord , "plot_id" , after = 2) # if tree_coord has more than 2 col
    } 
    
    invisible(lapply(split(sub_corner_coord, by = "subplot_id", keep.by = TRUE), function(dat) {
      tree_coord[ plot_id == dat$plot_ID_corner[1] &
                   get(names(tree_coord)[1]) %between% range(dat[[3]]) &
                   get(names(tree_coord)[2]) %between% range(dat[[4]]),
                 subplot_id := dat$subplot_id[1] ]
    }))
    
    if (anyNA(tree_coord[, subplot_id])) {
      warning("One or more trees could not be assigned to a subplot (not in a subplot area)")
    }
    if(is.null(plot_ID_tree)) {
      tree_coord[ , c("subplot_id","plot_id") := list(paste0("subplot",subplot_id),NULL)]
      setcolorder(tree_coord , "subplot_id" , after = 2)
    } else {
      setcolorder(tree_coord , "subplot_id" , after = 3)
    }
  }
  
  
  # returns --------------------------------------------------------------------
  
  if(is.null(plot_ID_corner)) {
    sub_corner_coord[ , c("subplot_id","plot_ID_corner") := list(paste0("subplot",subplot_id),NULL)]
  }
  
  if(is.null(tree_coord)) {
    output <- data.frame(sub_corner_coord)
  } else {
    output <- list(sub_corner_coord = data.frame(sub_corner_coord), 
                   tree_coord = data.frame(tree_coord))
  }

  return(output)
}

