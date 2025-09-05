#' Divides one ore more plots into subplots
#'
#' @description
#' This function divides a plot (or several plots) into subplots in the relative coordinates system, and returns the coordinates of subplot corners.
#'
#' @details
#'  If corner coordinates in the projected coordinate system are provided (proj_coord), projected coordinates of subplot corners are calculated by a bilinear interpolation in relation with relative coordinates of plot corners. Be aware that this bilinear interpolation only works if the plot in the relative coordinates system is rectangular (ie, has 4 right angles).
#'  
#'  In order to propagate GPS measurement uncertainties, the `sd_coord` argument has to be provided and must contains the average standard deviation of the GPS measurements for each corner on the X and Y axes (typically, the output $sd_coord of the [check_plot_coord()] function). If corner_data contains only one plot, `sd_coord` must be a numeric. If dealing with several plot, `sd_coord` must be a data frame of two columns named 'plot_ID' and 'sd_coord' containing respectively the plot IDs and the previous metric (again, see the output $sd_coord of the [check_plot_coord()] function).
#'  
#' @param corner_data A data frame, data frame extension, containing the plot corner coordinates. Typically, the output `$corner_coord` of the [check_plot_coord()] function.
#' @param rel_coord A character vector of length 2,  specifying the column names (resp. x, y) of the corner relative coordinates.
#' @param proj_coord (optional, if longlat is not provided) A character vector of length 2, specifying the column names (resp. x, y) of the corner projected coordinates.
#' @param longlat (optional, if proj_coord is not provided) A character vector of length 2, specifying the column names of the corner geographic coordinates (long,lat).
#' @param grid_size A vector indicating the dimensions of grid cells (resp. X and Y dimensions). If only one value is given, grid cells will be considered as squares.
#' @param tree_data A data frame containing tree relative coordinates and other optional tree metrics (one row per tree).
#' @param tree_coords A character vector of length 2, specifying the column names of the relative coordinates of the trees.
#' @param corner_plot_ID If dealing with several plots: a vector indicating plot IDs for corners.
#' @param tree_plot_ID If dealing with several plots: a vector indicating tree plot IDs.
#' @param grid_tol A numeric between (0;1) corresponding to the percentage of the plot area allowed to be excluded from the plot division (when grid_size doesn't match exactly plot dimensions).
#' @param centred_grid When grid_size doesn't match exactly plot dimensions, a logical indicating if the subplot grid should be centered on the plot.
#' @param sd_coord used to propagate GPS measurements uncertainties to the subplot polygon areas and the ref_raster footprint in [subplot_summary()]. See Details.
#' @param n used to propagate GPS measurements uncertainties: the number of iterations to be used (as in [AGBmonteCarlo()]). Cannot be smaller than 50 or larger than 1000.
#'
#' @return Returns a list containing:
#'   - $sub_corner_coord: a data-frame (or a list of data.table if sd_coord is provided) containing as many rows as there are corners corresponding to the subplots, and the following columns :
#'     - `plot_ID`: If dealing with multiple plots: the plot code, else, a column containing an empty character
#'     - `subplot_ID`: The automatically generated subplot code, using the following rule : subplot_X_Y 
#'     - `x_rel` and `y_rel` : the relative X-axis and Y-axis coordinates of subplots corners. 
#'     - `x_proj` and `y_proj` : if proj_coord is provided, the projected X-axis and Y-axis coordinates of subplots corners
#'     
#'  - $tree_data: the tree_data argument with the subplot_ID of each tree in the last column 
#'   
#'  - $UTM_code: if 'longlat' is provided, a data.frame containing the UTM code of the corner GPS coordinates for each plot
#'   
#'
#' @export
#' @author Arthur PERE, Arthur BAILLY
#' @importFrom data.table data.table := setcolorder 
#' @importFrom stats dist
#' @examples
#' # One plot with repeated measurements of each corner
#' data("NouraguesPlot201")
#' check_plot201 <- check_plot_coord(
#'   corner_data = NouraguesPlot201,
#'   proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
#'   trust_GPS_corners = TRUE, draw_plot = FALSE)
#' subplots_201 <- divide_plot(
#'   corner_data = check_plot201$corner_coord, 
#'   rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"),
#'   grid_size = 50)
#' subplots_201
#' 
#' # Assigning trees to subplots
#' data("NouraguesTrees")
#' plot201_trees <- NouraguesTrees[NouraguesTrees$Plot == 201,]
#' subplots_201 <- suppressWarnings(
#'   divide_plot(
#'     corner_data = check_plot201$corner_coord, 
#'     rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"),
#'     grid_size = 50,
#'     tree_data = plot201_trees, tree_coords = c("Xfield","Yfield")))
#' head(subplots_201$sub_corner_coord)
#' head(subplots_201$tree_data)
#' 
#' # When grid dimensions don't fit perfectly plot dimensions
#' \donttest{
#'   divide_plot(
#'     corner_data = check_plot201$corner_coord, 
#'     rel_coord = c("x_rel","y_rel"),
#'     grid_size = c(41,41),
#'     grid_tol = 0.4, centred_grid = TRUE)
#' }
#' 
#' # Dealing with multiple plots
#' data("NouraguesCoords")
#' nouragues_subplots <- suppressWarnings(
#'   divide_plot(
#'     corner_data = NouraguesCoords,
#'     rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"),
#'     corner_plot_ID = "Plot",
#'     grid_size = 50,
#'     tree_data = NouraguesTrees, tree_coords =  c("Xfield","Yfield"),
#'     tree_plot_ID = "Plot"))
#' head(nouragues_subplots$sub_corner_coord)
#' head(nouragues_subplots$tree_data)
 
divide_plot <- function(corner_data, rel_coord, proj_coord = NULL, longlat = NULL, grid_size, tree_data = NULL, tree_coords = NULL, corner_plot_ID = NULL, tree_plot_ID = NULL, grid_tol = 0.1, centred_grid = FALSE, sd_coord = NULL, n = 100) {
  
  # Checking arguments ---------------------------------------------------------
  
  if(missing(corner_data)) {
    stop("The way in which arguments are provided to the function has changed since version 2.2.1. You now have to provide corner_data data frame and its associated coordinates variable names.")
  }
  if(!is.data.frame(corner_data)){
    stop("corner_data must be a data frame or a data frame extension")
  }
  if (!any(rel_coord %in% names(corner_data))) {
    stop("column names provided by rel_coord are not found in corner_data")
  }
  if (!is.null(proj_coord) && !any(proj_coord %in% names(corner_data))) {
    stop("column names provided by proj_coord are not found in corner_data")
  }
  if (!is.null(longlat) && !any(longlat %in% names(corner_data))) {
    stop("column names provided by longlat are not found in corner_data")
  }
  if(!length(grid_size) %in% c(1,2)) {
    stop("The length of grid_size must be equal to 1 or 2\nIf you want to divide several plots with different grid sizes, you must apply yourself the function for each plot")
  }
  if(nrow(corner_data)!=4 & is.null(corner_plot_ID)){
    stop("You must provide corner_plot_ID if you have more than one plot in your data")
  }
  if (!is.null(corner_plot_ID) && !any(corner_plot_ID==names(corner_data))) {
    stop(paste(corner_plot_ID,"is not found in corner_data column names."))
  }
  if(!is.null(corner_plot_ID) && !all(sapply(split(corner_data,corner_data[[corner_plot_ID]]) , nrow) == 4)){
    stop("corner_data does'nt contain exactly 4 corners by plot")
  }
  if(!is.null(tree_data) && !is.data.frame(tree_data)){
    stop("tree_data must be a data frame or a data frame extension")
  }
  if(!is.null(tree_data) && is.null(tree_coords)) {
    stop("You must provide the column names of the relative coordinates of the trees using the tree_coords argument")
  }
  if(!is.null(tree_data) && !any(tree_coords %in% names(tree_data))) {
    stop("column names provided by tree_coords are not found in tree_data colunm names")
  }
  if(nrow(corner_data)!=4 & !is.null(tree_data) & is.null(tree_plot_ID)){
    stop("You must provide tree_plot_ID if you have more than one plot in your data")
  }
  if (!is.null(tree_plot_ID) && !any(tree_plot_ID==names(tree_data))) {
    stop(paste(tree_plot_ID,"is not found in tree_data column names."))
  }
  if(!is.null(sd_coord)) {
    # if sd_coord is a single value
    if(!is.data.frame(sd_coord)) {
      if(!is.numeric(sd_coord)) stop("sd_coord must be a numeric")
      if(!is.null(corner_plot_ID)) warning("The value of sd_coord will be used for every plot contained in 'corner_data'.")
      
    } else { # if sd_coord is a data frame
      if(all.equal(names(sd_coord),c("plot_ID","sd_coord")) != TRUE) stop("Column names of sd_coord must be 'plot_ID' and 'sd_coord'")
      if( sum(!unique(corner_data[,corner_plot_ID]) %in% sort(sd_coord$plot_ID)) != 0 ) stop("Plot IDs in corner_data and sd_coord don't match.") 
      if(sum(is.na(sd_coord$sd_coord))!=0) stop("sd_coord must not contain NA value.")
    } 
  }
  
  # Data processing ------------------------------------------------------------
  
  if(length(grid_size)!=2) grid_size = rep(grid_size,2)
  
  corner_dt <- data.table(corner_data)
  
  setnames(corner_dt, old = rel_coord, new = c("x_rel","y_rel"))

  if(!is.null(proj_coord)) {
    setnames(corner_dt, old = proj_coord, new = c("x_proj","y_proj"))
  }
  if(!is.null(longlat)) {
    if(!is.null(proj_coord)) {
      message("As the 'proj_coord' argument has been provided, the 'longlat' argument will be ignored.")
      longlat <- NULL
    } else {
      setnames(corner_dt, old = longlat, new = c("long","lat"))
    }
  }
  
  if(!is.null(corner_plot_ID)) {
    setnames(corner_dt, old = corner_plot_ID, new = "plot_ID")
  } else {
    corner_dt[, plot_ID := "subplot" ]
  }
  
  if(!is.null(sd_coord) && !is.data.frame(sd_coord)) {
    sd_coord <- data.frame(plot_ID = unique(corner_dt$plot_ID), sd_coord = sd_coord)
  }

  # Sorting rows in a counter-clockwise direction and check for non-rectangular plot
  sort_rows <- function(dat) { # dat = corner_dt
    centroid <- colMeans(dat[,c("x_rel","y_rel")])
    angles <- atan2(dat[["y_rel"]] - centroid[2], dat[["x_rel"]] - centroid[1])
    dat <- dat[order(angles), ]
    # check for non-rectangular plot : distances between centroid and corners must be equals
    if(!all(abs(dist(rbind(dat[,c("x_rel","y_rel")],as.data.frame.list(centroid)))[c(4,7,9,10)] - mean(dist(rbind(dat[,c("x_rel","y_rel")],as.data.frame.list(centroid)))[c(4,7,9,10)]))<0.1)) {
      stop("The plot in the relative coordinate system is not a rectangle (or a square). BIOMASS package can't deal with non-rectangular plot")
    }
    return(dat)
  }
  
  corner_dt <- corner_dt[, sort_rows(.SD), by = plot_ID]

  
  # Transform the geographic coordinates into UTM coordinates ------------------
  
  latlong2UTM_fct <- function(dat) { # dat = corner_dt
    proj_coord <- latlong2UTM(dat[, c("long","lat")])
    UTM_code <- unique(proj_coord[, "codeUTM"])
    if(length(UTM_code)>1) {
      stop(paste(unique(dat$plot_ID), "More than one UTM zone are detected. This may be due to an error in the long/lat coordinates, or if the parcel is located right between two UTM zones. In this case, please convert yourself your long/lat coordinates into any projected coordinates which have the same dimension than your local coordinates"))
    }
    corner_dt[plot_ID %in% dat$plot_ID, c("x_proj", "y_proj") := list(x_proj = proj_coord$X, y_proj = proj_coord$Y)]
    return(data.frame(UTM_code = UTM_code))
  }
  # Apply latlong2UTM_fct to all plots if necessary
  if(!is.null(longlat)) {
    UTM_code <- corner_dt[, latlong2UTM_fct(.SD), by = plot_ID, .SDcols = colnames(corner_dt)]
    if(length(UTM_code$plot_ID) == 1) UTM_code$plot_ID <- ""
  }
  
  
  # Dividing plots   -----------------------------------------------------------
  
  ### Grids the plot in the relative coordinates system
  divide_plot_rel_coord_fct <- function(dat, grid_size) { #dat = corner_dt[plot_ID==201,]
    
    # Check that grid dimensions match plot dimensions
    x_plot_length <- diff(range(dat[["x_rel"]]))
    y_plot_length <- diff(range(dat[["y_rel"]]))
    x_not_in_grid <- x_plot_length %% grid_size[1]
    y_not_in_grid <- y_plot_length %% grid_size[2]
    if( x_not_in_grid != 0 ) warning("\nThe x-dimension of the plot is not a multiple of the x-dimension of the grid size")
    if( y_not_in_grid != 0 ) warning("\nThe y-dimension of the plot is not a multiple of the y-dimension of the grid size")
    if( x_not_in_grid * y_plot_length + y_not_in_grid * x_plot_length - x_not_in_grid * y_not_in_grid > grid_tol * x_plot_length * y_plot_length ) {
      stop(paste("More than",grid_tol*100,"% of the plot area is not included in the sub-plot grid. If you still want to divide the plot, please increase the value of the grid_tol argument."))
    }
    x_not_in_grid <- ifelse(centred_grid, x_not_in_grid, 0)
    y_not_in_grid <- ifelse(centred_grid, y_not_in_grid, 0)
    
    plot_grid <- data.table(as.matrix(expand.grid(
      x_rel = seq(min(dat[["x_rel"]]) + x_not_in_grid/2, max(dat[["x_rel"]]), by = grid_size[1]),
      y_rel = seq(min(dat[["y_rel"]]) + y_not_in_grid/2, max(dat[["y_rel"]]), by = grid_size[2])
    )))
    plot_grid[,plot_ID:=unique(dat$plot_ID)]
    
    # Attributing subplots names to each corner and adding shared subplot corners
    plot_grid <- rbindlist(apply(plot_grid[x_rel < max(x_rel) & y_rel < max(y_rel),], 1, function(grid_dat) {
      X <- as.numeric(grid_dat[["x_rel"]])
      Y <- as.numeric(grid_dat[["y_rel"]])
      plot_grid[
        (x_rel == X & y_rel == Y) | (x_rel == X + grid_size[1] & y_rel == Y) | (x_rel == X + grid_size[1] & y_rel == Y + grid_size[2]) | (x_rel == X & y_rel == Y + grid_size[2]),
        .(subplot_ID = paste(plot_ID, (X-min(plot_grid$x_rel)) / grid_size[1], (Y-min(plot_grid$y_rel)) / grid_size[2], sep = "_"),x_rel, y_rel)]
    }))
    
    # Sorting rows 
    plot_grid <- plot_grid[, sort_rows(.SD), by=subplot_ID]
  }
  
  # Apply divide_plot_rel_coord_fct to all plots
  plot_grid <- corner_dt[, divide_plot_rel_coord_fct(.SD, grid_size), by = plot_ID, .SDcols = colnames(corner_dt)]
  # if just one plot, replace "subplot" by "" for the plot_ID column (before it goes in a list when coordinates uncertainties)
  if (length(unique(corner_dt$plot_ID)) == 1) {
    corner_dt[, plot_ID := ""]
    plot_grid[, plot_ID := ""]
    if(!is.null(sd_coord)) {
      sd_coord$plot_ID <- ""
    }
  }
  
  
  ### Calculates the projected coordinates of the grid points 
  project_coord_fct <- function(dat, plot_grid) { # dat = corner_dt[plot_ID==201,]
  
    # Adding a random error on GPS measurements:
    if(!is.null(sd_coord)) {
      dat <- merge(dat, sd_coord)
      dat[, c("x_proj","y_proj") := list(x_proj = x_proj + rnorm(4,0,sd_coord), y_proj = y_proj + rnorm(4,0,sd_coord))]
    }
    
    # Transformation of relative grid coordinates into projected coordinates if provided
    plot_grid <- plot_grid[plot_ID == unique(dat$plot_ID)]
    if(!is.null(proj_coord) | !is.null(longlat)) {
      plot_grid <- cbind(plot_grid,bilinear_interpolation(coord = plot_grid[,c("x_rel","y_rel")] , from_corner_coord = dat[,c("x_rel","y_rel")] , to_corner_coord = dat[,c("x_proj","y_proj")], ordered_corner = T))
    }
    
    return(plot_grid)
  }
  
  # Apply project_coord_fct to all plots
  if(is.null(sd_coord)) {
    sub_corner_coord <- corner_dt[, project_coord_fct(.SD, plot_grid), by = plot_ID, .SDcols = colnames(corner_dt)][,-1]
  } else { # if error propagation on coordinates: return a list of length n
    sub_corner_coord <- lapply(1:n, function(x) corner_dt[, project_coord_fct(.SD, plot_grid), by = plot_ID, .SDcols = colnames(corner_dt), ][,-1])
  }
  
  
  # Retrieving geographic coordinates (if no error propagation) ----------------
  if(!is.null(longlat) && is.null(sd_coord)) {
    GPS_coord_fct <- function(dat) { # dat = sub_corner_coord
      gps_coord <- as.data.frame( proj4::project(dat[,c("x_proj","y_proj")], proj = UTM_code$UTM_code[UTM_code$plot_ID == unique(dat$plot_ID)], inverse = TRUE) )
      sub_corner_coord[plot_ID %in% dat$plot_ID, c("long", "lat") := list(long = gps_coord$x, lat = gps_coord$y)]
    }
    sub_corner_coord[, GPS_coord_fct(.SD), by = plot_ID, .SDcols = colnames(sub_corner_coord)]
  }
  
  
  # Assigning trees to subplots ------------------------------------------------
  
  if(!is.null(tree_data)) {
    
    tree_dt <- data.table(tree_data)
    
    setnames(tree_dt, old = tree_coords, new = c("x_rel","y_rel"))
    
    if(!is.null(tree_plot_ID)) {
      setnames(tree_dt, old = tree_plot_ID, new = "plot_ID")
      
      if(any(! unique(tree_dt[["plot_ID"]]) %in% unique(corner_dt[["plot_ID"]]))) {
        warning( paste( "These ID's are found in tree_plot_ID but not in corner_data :" , paste(unique(tree_dt[["plot_ID"]])[! unique(tree_dt[["plot_ID"]]) %in% unique(corner_dt[["plot_ID"]])] , collapse = " "),"\n") )
      }
      
    } else {
      tree_dt[, plot_ID := "" ]
    } 
    
    if(!is.data.frame(sub_corner_coord)) {
      sub_corner_coord_ref <- sub_corner_coord[[1]]
    } else {
      sub_corner_coord_ref <- sub_corner_coord
    }
    invisible(lapply(split(sub_corner_coord_ref, by = "subplot_ID", keep.by = TRUE), function(dat) {
      tree_dt[ plot_ID == dat$plot_ID[1] &
                 x_rel %between% range(dat[["x_rel"]]) &
                 y_rel %between% range(dat[["y_rel"]]),
               subplot_ID := dat$subplot_ID[1]]
    }))
    
    if (anyNA(tree_dt[, subplot_ID])) {
      warning("One or more trees could not be assigned to a subplot (not in a subplot area)")
    }
  }
  
  # Returns --------------------------------------------------------------------
  
  output <- list()
  
  if(is.null(sd_coord)) { # no error propagation
    output$sub_corner_coord <- data.frame(sub_corner_coord)
  } else { # error propagation
    output$sub_corner_coord <- sub_corner_coord
  }
  
  if(!is.null(tree_data)) { # tree_data present
    output$tree_data <- data.frame(tree_dt)
  }
  
  if(!is.null(longlat)) {
    output$UTM_code <- UTM_code
  }
  
  return(output)
}