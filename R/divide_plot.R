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
#' @param tree_df a data frame containing tree relative coordinates and other tree metrics (one row per tree).
#' @param tree_coords a character vector of size 2 containing the column names of the relative coordinates of the trees.
#' @param corner_plot_ID if dealing with multiple plots : a vector indicating plot IDs for corners.
#' @param tree_plot_ID if dealing with multiple plots : a vector indicating tree plot IDs.
#' @param grid_tol a numeric between (0;1) corresponding to the percentage of the plot area allowed to be excluded from the plot division (when grid_size doesn't match exactly plot dimensions).
#' @param centred_grid when grid_size doesn't match exactly plot dimensions, a logical indicating if the subplot grid should be centred on the plot.
#'
#' @return If tree_df isn't supplied, returns a data-frame containing as many rows as there are corners corresponding to the subplots, and the following columns :
#'   - `corner_plot_ID`: If dealing with multiple plots : the plot code
#'   - `subplot_id`: The automatically generated subplot code, using the following rule : subplot_X_Y 
#'   - `x_rel` and `y_rel` : the relative X-axis and Y-axis coordinates of subplots corners. 
#'   - `x_proj` and `y_proj` :  if proj_coord is supplied, the projected X-axis and Y-axis coordinates of subplots corners
#'   
#'   If tree_df is supplied, returns a list containing : 
#'   - the previous data-frame 
#'   - the tree_df data-frame with the subplot_id of each tree in the last column 
#'
#' @export
#' @author Arthur PERE, Arthur BAILLY
#' @importFrom data.table data.table := setcolorder 
#' @examples
#'
#' # Rectangular plot and grid cells
#' rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 100, 100))
#' subplots <- divide_plot(rel_coord = rel_coord, grid_size = c(100,50))
#' 
#' # Squared plot and projected coordinates associated
#' rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 200, 200))
#' proj_coord <- data.frame(x_proj = c(110, 190, 60, 145), y_proj = c(110, 160, 196, 245))
#' subplots <- divide_plot(rel_coord, proj_coord = proj_coord, grid_size = 100)
#' 
#' # Assigning trees to subplots
#' tree_df <- data.frame(x_tree = runif(50,0,200), y_tree = runif(50,0,200))
#' subplots <- divide_plot(rel_coord, proj_coord, 100, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#' subplots$sub_corner_coord
#' subplots$tree_df
#' 
#' # When grid dimensions don't fit perfectly plot dimensions
#' subplots <- divide_plot(rel_coord, grid_size = c(45,50))
#' subplots <- divide_plot(rel_coord, grid_size = c(41,41) , grid_tol = 0.4, centred_grid = TRUE)
#' 
#' # Dealing with multiple plots
#' rel_coord <- rbind(rel_coord, rel_coord)
#' proj_coord <- rbind(proj_coord, proj_coord + 200)
#' tree_df <- rbind(tree_df, data.frame(x_tree = runif(50,0,200), y_tree = runif(50,0,200)))
#' corner_plot_ID <- rep(c("plot1","plot2"), e=4)
#' tree_plot_ID <- rep(c("plot1","plot2"), e=50)
#' subplots <- divide_plot(rel_coord, proj_coord, 100, tree_df, c("x_tree","y_tree"), corner_plot_ID, tree_plot_ID)
#'
 
divide_plot <- function(rel_coord, proj_coord = NULL, grid_size, tree_df = NULL, tree_coords = NULL, corner_plot_ID = NULL, tree_plot_ID = NULL, grid_tol = 0.1, centred_grid = F) {
  
  # Parameters verification ----------------------------------------------------
  if (is.matrix(rel_coord)) {
    rel_coord <- data.frame(x_rel=rel_coord[,1],y_rel=rel_coord[,2])
  }
  if (!is.null(proj_coord) && !is.data.frame(proj_coord)) {
    proj_coord <- data.frame(x_proj=proj_coord[,1], y_proj=proj_coord[,2])
  }
  if (!is.null(corner_plot_ID) && nrow(rel_coord) != length(corner_plot_ID)) {
    stop("The length of corner_plot_ID and the number of rows of rel_coord are different")
  }
  if(!length(grid_size) %in% c(1,2)) {
    stop("The length of grid_size must be equal to 1 or 2\nIf you want to divide several plots with different grid sizes, you must apply yourself the function for each plot")
  }
  if(nrow(rel_coord)%% 4 !=0){
    stop("rel_coord does'nt contain exactly 4 corners by plot")
  }
  if(nrow(rel_coord)!=4 & is.null(corner_plot_ID)){
    stop("You must supply corner_plot_ID if you have more than one plot in your data")
  }
  if(!is.null(corner_plot_ID) && nrow(rel_coord)/length(unique(corner_plot_ID)) !=4 ) {
    stop("corner_plot_ID must contain the same number of plots as rel_coord")
  }
  if(!is.null(proj_coord) && nrow(proj_coord) != nrow(rel_coord)) {
      stop("rel_coord and proj_coord don't have the same number of rows")
  }
  if(!is.null(tree_df) && (!is.matrix(tree_df) & !is.data.frame(tree_df))){
    stop("tree_df must be a matrix or a data frame")
  }
  if(!is.null(tree_df) && is.null(tree_coords)) {
    stop("You must supply the column names of relative coordinates of the trees using the tree_coords argument")
  }
  if(!is.null(tree_df) && !any(tree_coords %in% names(tree_df))) {
    stop("tree_coords are not found in tree_df colunm names")
  }
  if(nrow(rel_coord)!=4 & !is.null(tree_df) & is.null(tree_plot_ID)){
    stop("You must supply tree_plot_ID if you have more than one plot in your data")
  }
  if(!is.null(tree_df) && !is.null(tree_plot_ID) && nrow(tree_df)!=length(tree_plot_ID)) {
    stop("The length of tree_plot_ID and the number of rows of tree_df are different")
  }
  if(nrow(rel_coord)!=4 && !is.null(corner_plot_ID) && !is.null(tree_plot_ID) && any(! unique(tree_plot_ID) %in% unique(corner_plot_ID))) {
    stop("Some tree_plot_ID are not found in corner_plot_ID")
  }
  
  
  # Formatting data  -----------------------------------------------------------
  
  if(length(grid_size)!=2) grid_size = rep(grid_size,2)

  rel_coord <- data.table(x_rel = rel_coord[[1]], y_rel = rel_coord[[2]])

  if(!is.null(corner_plot_ID)){
    rel_coord[,corner_plot_ID:=corner_plot_ID]
  } else{
    rel_coord[,corner_plot_ID:=""]
  }
  setcolorder(rel_coord , "corner_plot_ID" , after = 2) # if rel_coord has more than 2 col

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
    combine_coord <-  cbind(rel_coord,
                            data.table(x_proj = proj_coord[[1]], y_proj = proj_coord[[2]])
    )
    combine_coord <- combine_coord[, sort_rows(.SD), by = corner_plot_ID]
  } else {
    combine_coord <- rel_coord[, sort_rows(.SD), by = corner_plot_ID]
  }
  
  # Dividing plots   -----------------------------------------------------------
  
  # Grids the plot from the relative coordinates and calculates the projected coordinates of the grid points.
  divide_plot_fct <- function(dat, grid_size) {
    
    # Check that grid dimensions match plot dimensions
    x_plot_length <- diff(range(dat[[2]]))
    y_plot_length <- diff(range(dat[[3]]))
    x_not_in_grid <- x_plot_length %% grid_size[1]
    y_not_in_grid <- y_plot_length %% grid_size[2]
    if( x_not_in_grid != 0 ) warning("The x-dimension of the plot is not a multiple of the x-dimension of the grid size")
    if( y_not_in_grid != 0 ) warning("The y-dimension of the plot is not a multiple of the y-dimension of the grid size")
    if( x_not_in_grid * y_plot_length + y_not_in_grid * x_plot_length - x_not_in_grid * y_not_in_grid > grid_tol * x_plot_length * y_plot_length ) {
      stop(paste("More than",grid_tol*100,"% of the plot area is not included in the sub-plot grid. If you still want to divide the plot, please increase the value of the grid_tol argument."))
    }
    x_not_in_grid <- ifelse(centred_grid, x_not_in_grid, 0)
    y_not_in_grid <- ifelse(centred_grid, y_not_in_grid, 0)
    
    plot_grid <- data.table(as.matrix(expand.grid(
      x_rel = seq(min(dat[["x_rel"]]) + x_not_in_grid/2, max(dat[["x_rel"]]), by = grid_size[1]),
      y_rel = seq(min(dat[["y_rel"]]) + y_not_in_grid/2, max(dat[["y_rel"]]), by = grid_size[2])
    )))
    plot_grid[,corner_plot_ID:=unique(dat$corner_plot_ID)]
    
    # Attributing subplots names to each corner and adding shared subplot corners
    plot_grid <- rbindlist(apply(plot_grid[x_rel < max(x_rel) & y_rel < max(y_rel),], 1, function(grid_dat) {
      X <- as.numeric(grid_dat[["x_rel"]])
      Y <- as.numeric(grid_dat[["y_rel"]])
      plot_grid[
        (x_rel == X & y_rel == Y) | (x_rel == X + grid_size[1] & y_rel == Y) | (x_rel == X + grid_size[1] & y_rel == Y + grid_size[2]) | (x_rel == X & y_rel == Y + grid_size[2]),
        .(subplot_id = paste(corner_plot_ID, (X-min(plot_grid$x_rel)) / grid_size[1], (Y-min(plot_grid$y_rel)) / grid_size[2], sep = "_"),x_rel, y_rel)]
    }))
    
    # Sorting rows 
    plot_grid <- plot_grid[, sort_rows(.SD), by=subplot_id]
    
    # Transformation of relative grid coordinates into projected coordinates if supplied
    if(!is.null(proj_coord)) {
      plot_grid <- cbind(plot_grid,bilinear_interpolation(coord = plot_grid[,c("x_rel","y_rel")] , from_corner_coord = dat[,c("x_rel","y_rel")] , to_corner_coord = dat[,c("x_proj","y_proj")], ordered_corner = T))
    }
    return(plot_grid)
  }
  
  # Apply divide_plot_fct to all plots
  sub_corner_coord <- combine_coord[, divide_plot_fct(.SD, grid_size), by = corner_plot_ID, .SDcols = colnames(combine_coord)]
  
  # Assigning trees to subplots -------------------------------------------------------------
  if(!is.null(tree_df)) {
    tree_df <- as.data.table(tree_df)
    
    if(!is.null(tree_plot_ID)){
      tree_df[,plot_id:=tree_plot_ID]
    } else{
      tree_df[,plot_id:=""]
    } 
    invisible(lapply(split(sub_corner_coord, by = "subplot_id", keep.by = TRUE), function(dat) {
      tree_df[ plot_id == dat$corner_plot_ID[1] &
                 get(tree_coords[1]) %between% range(dat[["x_rel"]]) &
                 get(tree_coords[2]) %between% range(dat[["y_rel"]]),
               subplot_id := dat$subplot_id[1]]
    }))
    
    if (anyNA(tree_df[, subplot_id])) {
      warning("One or more trees could not be assigned to a subplot (not in a subplot area)")
    }
    if(is.null(tree_plot_ID)) {
      tree_df[ , c("subplot_id","plot_id") := list(paste0("subplot",subplot_id),NULL)]
    } 
  }
  
  
  # returns --------------------------------------------------------------------
  
  if(is.null(corner_plot_ID)) {
    sub_corner_coord[ , c("subplot_id","corner_plot_ID") := list(paste0("subplot",subplot_id),NULL)]
  }
  
  if(is.null(tree_df)) {
    output <- data.frame(sub_corner_coord)
  } else {
    output <- list(sub_corner_coord = data.frame(sub_corner_coord), 
                   tree_df = data.frame(tree_df))
  }

  return(output)
}

