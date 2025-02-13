#' Check coordinates of plot corners and trees
#'
#' @description
#' Quality check of plot corner and tree coordinates.
#'
#' @details
#' If trust_GPS_corners is TRUE, corner coordinates in the projected coordinate system are averaging by corner (if multiple measures) and outlier corners are identified sequentially using these averages and the max_dist argument. Then, projected coordinates of the trees are calculated from the local coordinates using a bilinear interpolation that follows the correspondence of the corners between these two coordinate systems. Be aware that this projection only works if the plot, in the relative coordinates system, is rectangular (ie, has 4 right angles).
#' 
#' If trust_GPS_corners is FALSE, corner coordinates in the projected coordinate system are calculated by a procrust analysis that preserves the shape and dimensions of the plot in the local coordinate system. Outlier corners are also identified sequentially and projected coordinates of the trees are calculated by applying the resulting procrust analysis.
#' 
#' If longlat is supplied instead of proj_coord, the function will first convert the long/lat coordinates into UTM coordinates. An error may result if the parcel is located right between two UTM zones. In this case, the user has to convert himself his long/lat coordinates into any projected coordinates which have the same dimension than his local coordinates (in meters most of the time).
#' 
#' If longlat and proj_coord are supplied, only longitude/latitude coordinates will be considered.
#' 
#' When ref_raster is supplied, this raster is cropped for every plot contained in corner_data. 
#'
#' @param corner_data A data frame, data frame extension, containing the plot corner coordinates.
#' @param proj_coord (optional, if longlat is not supplied) A character vector of length 2, specifying the column names (resp. x, y) of the corner projected coordinates.
#' @param longlat (optional, if proj_coord is not supplied) A character vector of length 2 specifying the column names of the corner geographic coordinates (long,lat).
#' @param rel_coord A character vector of length 2 specifying the column names (resp. x, y) of the corner relative coordinates (that of the field, ie, the local ones).
#' @param trust_GPS_corners A logical indicating whether or not you trust the GPS coordinates of the plot's corners. See details.
#' @param draw_plot A logical indicating if the plot design should be displayed and returned.
#' @param tree_data A data frame, data frame extension, containing the relative coordinates (field/local coordinates) of the trees and optional other tree metrics.
#' @param tree_coords A character vector specifying the column names of the tree relative coordinates.
#' @param max_dist If dealing with repeated measurements of each corner : the maximum distance (in meters) above which GPS measurements should be considered outliers (default 15 m).
#' @param rm_outliers If TRUE and dealing with repeated measurements of each corner, then outliers are removed from the coordinate calculation of the referenced corners.
#' @param plot_ID If dealing with multiple plots : a character indicating the variable name for corner plot IDs in corner_data.
#' @param tree_plot_ID If dealing with multiple plots : a character indicating the variable name for tree plot IDs in tree_data.
#' @param ref_raster A SpatRaster object from terra package, typically a chm raster created from LiDAR data.
#' @param prop_tree The column name variable of tree_data for which the tree visualization will be proportional.
#' @param ask If TRUE and dealing with multiple plots, then prompt user before displaying each plot. 
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN, Arthur BAILLY
#'
#' @return Returns a list including :
#'    - `corner_coord`: a data frame containing the projected coordinates (x_proj and y_proj) and the relative coordinates (x_rel and y_rel) of the 4 corners of the plot 
#'    - `polygon`: a sf object containing plot's polygon(s)
#'    - `tree_data`: if `tree_data` is supplied in the arguments of the function, a data frame corresponding to tree_data for which the projected coordinates of the trees (x_proj and y_proj) are added, and also a variable telling if the trees are inside the plot (is_in_plot). The name of the relative tree coordinates are also standardised and renamed to (x_rel and y_rel).
#'    - `outliers`: a data frame containing the projected coordinates and the row number of GPS measurements considered outliers 
#'    - `plot_design`: if `draw_plot` is TRUE, a ggplot object corresponding to the design of the plot
#'    - `UTM_code`: if `longlat` is supplied, a character containing the UTM code of the GPS coordinates
#'
#' @export
#'
#' @importFrom data.table data.table := setnames %between% copy
#' @importFrom sf st_multipoint st_polygon st_sfc st_as_sf
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_polygon geom_text geom_raster scale_shape_manual scale_color_manual ggtitle theme_minimal theme coord_equal arrow unit element_blank guides guide_legend scale_alpha scale_alpha_manual scale_size
#' @importFrom terra vect crop as.data.frame
#'
#' @author  Arthur BAILLY, Arthur PERE, Maxime REJOU-MECHAIN
#'
#' @examples
#' # One plot with repeated measurements of each corner
#' data("NouraguesPlot201")
#' check_plot201 <- check_plot_coord(
#'   corner_data = NouraguesPlot201,
#'   proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
#'   trust_GPS_corners = TRUE, draw_plot = FALSE)
#' check_plot201$corner_coord
#' \donttest{
#'   check_plot201$plot_design
#' }
#' 
#' # 4 plots with one measurement of each corner
#' data("NouraguesCoords")
#' check_plots <- check_plot_coord(
#'   corner_data = NouraguesCoords,
#'   proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
#'   trust_GPS_corners = TRUE, plot_ID = "Plot", draw_plot = FALSE)
#' check_plots$corner_coord
#' \donttest{
#'   check_plots$plot_design
#' }
#' 
#' # Displaying the associated CHM raster and representing trees proportionally to their diameter
#' plot_204_coords <- NouraguesCoords[NouraguesCoords$Plot==204,]
#' data("NouraguesTrees")
#' plot_204_trees <- NouraguesTrees[NouraguesTrees$Plot == 204, ]
#' nouragues_raster <- terra::rast(
#'   system.file("extdata", "NouraguesRaster.tif",
#'               package = "BIOMASS", mustWork = TRUE)
#'   )
#' check_plot_204 <- check_plot_coord(
#'  corner_data = plot_204_coords,
#'  proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
#'  trust_GPS_corners = TRUE, draw_plot = FALSE,
#'  tree_data = plot_204_trees, tree_coords = c("Xfield","Yfield"),
#'  ref_raster = nouragues_raster, prop_tree = "D"
#' )
#' \donttest{
#'   check_plot_204$plot_design
#' }

check_plot_coord <- function(corner_data, proj_coord = NULL, longlat = NULL, rel_coord, trust_GPS_corners, draw_plot = TRUE, tree_data = NULL, tree_coords = NULL, max_dist = 10, rm_outliers = TRUE, plot_ID = NULL, tree_plot_ID = NULL, ref_raster = NULL, prop_tree = NULL, ask = T) {
  
  ##### Checking arguments -----------------------------------------------------
  
  if(missing(corner_data)) {
    stop("The way in which arguments are supplied to the function has changed since version 2.2.1. You now have to supply corner_data data frame and its associated coordinates variable names.")
  }
  if(!is.data.frame(corner_data)){
    stop("corner_data must a data frame or a data frame extension")
  }
  
  corner_dt <- data.table(corner_data)
  
  if (is.null(longlat) & is.null(proj_coord)) {
    stop("You must supply the name of at least one set of coordinates : longlat or proj_coord")
  }
  if (!is.null(proj_coord) && !any(proj_coord %in% names(corner_dt))) {
    stop("column names supplied by proj_coord are not found in corner_data")
  }
  if (!is.null(longlat) && !any(longlat %in% names(corner_dt))) {
    stop("column names supplied by longlat are not found in corner_data")
  }
  if (!any(rel_coord %in% names(corner_dt))) {
    stop("column names supplied by rel_coord are not found in corner_data")
  }
  if (missing(trust_GPS_corners)) {
    stop("The trust_GPS_corners argument must be set to TRUE or FALSE")
  }
  if(!is.null(tree_data) && !is.data.frame(tree_data)){
    stop("tree_data must a data frame or a data frame extension")
  }
  if (!is.null(tree_data) && is.null(tree_coords)) {
    stop("You must supply the column names corresponding to the relative coordinates of tree_data using the argument `tree_coords`")
  }
  if (!is.null(tree_data) && !any(tree_coords %in% names(tree_data))) {
    stop("column names supplied by tree_coords are not found in tree_data")
  }
  if (!is.null(prop_tree) && !any(prop_tree == names(tree_data))) {
    stop("column name supplied by prop_tree is not found in tree_data")
  }
  if (length(max_dist) != 1) {
    stop("The max_dist argument must be of length 1")
  }
  if(sum(is.na(corner_dt[,..rel_coord]))!= 0) {
    stop("Missing values are detected in corner relative coordinates. Please remove them and call the function again")
  }
  if(!is.null(proj_coord) && sum(is.na(corner_dt[,..proj_coord]))!= 0) {
    stop("Missing values are detected in corner projected coordinates. Please remove them and call the function again")
  }
  if(!is.null(longlat) && sum(is.na(corner_dt[,..longlat]))!= 0) {
    stop("Missing values are detected in corner longitude/latitude coordinates. Please remove them and call the function again")
  }
  if (nrow(unique(corner_dt[,..rel_coord])) !=4 & is.null(plot_ID)) {
    stop("If multiple plots are present in corner_data, then the argument plot_ID is required.")
  }
  if (!is.null(plot_ID) && !any(plot_ID==names(corner_dt))) {
    stop(paste(plot_ID,"is not found in corner_data column names."))
  }
  if(!is.null(tree_data) && !is.null(plot_ID) && is.null(tree_plot_ID)) {
    stop("The argument tree_plot_ID is required if plot_ID is supplied.")
  }
  if(!is.null(tree_data) && !is.null(tree_plot_ID) && any(! unique(tree_data[[tree_plot_ID]]) %in% unique(corner_dt[[plot_ID]])) ) {
    warning( paste( "These ID's are found in tree_plot_ID but not in plot_ID :" , paste(unique(tree_data[[tree_plot_ID]])[! unique(tree_data[[tree_plot_ID]]) %in% unique(corner_dt[[plot_ID]])] , collapse = " "),"\n") )
  }
  
  
  ##### Data processing --------------------------------------------------------
  
  # Formatting corner data
  setnames(corner_dt, old = rel_coord, new = c("x_rel","y_rel"))
  
  if(!is.null(proj_coord)) {
    setnames(corner_dt, old = proj_coord, new = c("x_proj","y_proj"))
  }
  if(!is.null(longlat)) {
    setnames(corner_dt, old = longlat, new = c("long","lat"))
  }
  
  if(!is.null(plot_ID)) {
    setnames(corner_dt, old = plot_ID, new = "plot_ID")
  } else {
    corner_dt[, plot_ID := "" ]
  }
  
  # Formatting tree data 
  if(!is.null(tree_data)) {
    if(!is.data.table(tree_data)) tree_data <- data.table(tree_data)
      
    setnames(tree_data, old = tree_coords, new = c("x_rel","y_rel"))
    
    if(!is.null(tree_plot_ID)) {
      setnames(tree_data, old = tree_plot_ID, new = "plot_ID")
    } else {
      tree_data[, plot_ID := "" ]
    }
    if(sum(is.na(tree_data[, c("x_rel","y_rel")])) != 0) {
      warning("Missing values are detected in the relative coordinates of the trees. These trees will be removed from the dataset.\n")
      tree_data <- tree_data[ ! (is.na(x_rel) | is.na(y_rel))  , ]
    }
  }
  
  outliers <- data.table("plot_ID" = character(), "x_proj" = numeric(), "y_proj" = numeric(), "row_number" = integer())
  
  ##### Functions --------------------------------------------------------------
  
  ### Transform the geographic coordinates into UTM coordinates ----------------
  
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
  }
  
  ### Check corner coordinates and calculate tree projected coordinates --------
  
  check_corner_fct <- function(dat) { # dat = corner_dt
    
    corner_dat <- copy(dat) # create a copy to enable the use of :=
    
    # Checking for unexpected corner
    if (nrow(unique(corner_dat[,c("x_rel","y_rel")])) != 4) {
      stop(paste(nrow(unique(corner_dat[,c("x_rel","y_rel")])), "unique corners are detected in",unique(corner_dat$plot_ID),"rel_coord (instead of 4)"))
    }
    
    # Removing outliers when repeated corner measurements and formatting corner data 
    
    if(nrow(corner_dat) != 4) { # if multiple measurements of each corner
      
      if (trust_GPS_corners && any(table(corner_dat[,c("x_rel","y_rel")]) < 5)) {
        warning(
          paste( ifelse(unique(corner_dat$plot_ID)=="", "", paste("In plot", unique(corner_dat$plot_ID), ":")) ,
                 "At least one corner has less than 5 measurements. We suggest using the argument trust_GPS_corners = FALSE\n"))
      }
      
      if(trust_GPS_corners == TRUE) { # if GPS corner locations are trusted, just average their locations
        
        corner_dat[ , c("x_proj_mean", "y_proj_mean", "row_number") := list(mean(x_proj), mean(y_proj), .I) , by=list(x_rel,y_rel)] 
        
      } else { # otherwise, apply the procrust analysis
        
        procrust_res <- procrust(corner_dat[, c("x_proj","y_proj")], corner_dat[, c("x_rel","y_rel")])
        procrust_coord <- as.matrix(corner_dat[, c("x_rel","y_rel")]) %*% procrust_res$rotation
        procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
        
        corner_dat[ , c("x_proj_mean","y_proj_mean","row_number") := list(procrust_coord[,1], procrust_coord[,2], .I)] 
      }
      
      # Set corner outliers based on the max_dist argument
      corner_dat[ , outlier := ifelse( sqrt((x_proj - x_proj_mean)^2 + (y_proj-y_proj_mean)^2) > max_dist, T, F)]
      
      
      if( all(corner_dat$outlier) ) {
        stop(
          paste( ifelse(unique(corner_dat$plot_ID)=="", "", paste("In plot", unique(corner_dat$plot_ID), ":")) ,
                 "all measurements for at least one corner are considered as outliers.\n
                 This may be because some coordinates have very large error associated.\n
                 Try to remove these very large error or reconsider the max_dist parameter by increasing the distance"))
      }
      
      if( !rm_outliers & sum(corner_dat$outlier)!=0) {
        warning(call. = F,
          paste( ifelse(unique(corner_dat$plot_ID)=="", "", paste("In plot", unique(corner_dat$plot_ID), ":")) ,
                 "Be carefull, you may have GNSS measurement outliers. \n",
                 "Removing them may improve the georeferencing of your plot (see rm_outliers and max_dist arguments).\n"))
      }
      
      if( rm_outliers & sum(corner_dat$outlier)!=0 ) {
        
        # Remove the outlier corners and repeat the previous procedure
        outliers_dat <- corner_dat[outlier==T , c("plot_ID","x_proj","y_proj","row_number")]
        corner_dat <- corner_dat[outlier==F ,]
        exist_outliers <- TRUE
        while(exist_outliers) {
          
          if(trust_GPS_corners == TRUE) {
            corner_dat[ , c("x_proj_mean", "y_proj_mean") := list(mean(x_proj), mean(y_proj)) , by=list(x_rel,y_rel)] 
          } else {
            procrust_res <- procrust(corner_dat[, c("x_proj","y_proj")], corner_dat[, c("x_rel","y_rel")])
            procrust_coord <- as.matrix(corner_dat[, c("x_rel","y_rel")]) %*% procrust_res$rotation
            procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
            corner_dat[ , c("x_proj_mean","y_proj_mean") := list(procrust_coord[,1], procrust_coord[,2])] 
          }
          
          corner_dat[ , outlier := ifelse( sqrt((x_proj - x_proj_mean)^2 + (y_proj-y_proj_mean)^2) > max_dist, T, F)]
          
          if(sum(corner_dat$outlier) == 0) {
            exist_outliers <- FALSE
          } else {
            outliers_dat <- rbind(outliers_dat , corner_dat[outlier==T , c("plot_ID","x_proj","y_proj","row_number")])
            corner_dat <- corner_dat[outlier==F ,]
          }
        }
        outliers <<- rbindlist(list(outliers, outliers_dat))
      }
        
      corner_dat <- unique(corner_dat[ , c("plot_ID","x_rel","y_rel","x_proj_mean","y_proj_mean")])
      setnames(corner_dat, old = c("x_proj_mean","y_proj_mean") , new = c("x_proj", "y_proj"))
      
       
      # End if multiple measurements of each corner 
      
    } else { # if only 1 measurements of each corners 
      
      if(trust_GPS_corners == FALSE) {
        
        # Transformation of relative to projected coordinates by a procrust analyses
        procrust_res <- procrust(corner_dat[, c("x_proj","y_proj")], corner_dat[, c("x_rel","y_rel")])
        
        procrust_coord <- as.matrix(corner_dat[, c("x_rel","y_rel")]) %*% procrust_res$rotation
        procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
        
        corner_dat[ , c("x_proj","y_proj") := list(procrust_coord[,1], procrust_coord[,2]) ]
      } # if GPS corner locations are trusted, don't do anything
      
      corner_dat <- corner_dat[ , c("plot_ID","x_rel","y_rel","x_proj","y_proj")]
    }
    
    # Sort corner_coord rows in a counter-clockwise direction
    centroid <- colMeans(corner_dat[,c("x_rel","y_rel")])
    angles <- base::atan2(corner_dat[["y_rel"]] - centroid[2], corner_dat[["x_rel"]] - centroid[1])
    corner_dat <- corner_dat[order(angles), ]
    
    # Calculate projected tree coordinates from relative tree coordinates
    if(!is.null(tree_data)) {
      if(trust_GPS_corners) {
        tree_data[plot_ID %in% unique(corner_dat$plot_ID), 
                  c("x_proj","y_proj") := as.list(bilinear_interpolation(
                    coord = .SD[, c("x_rel","y_rel")],
                    from_corner_coord = corner_dat[,c("x_rel","y_rel")],
                    to_corner_coord = corner_dat[,c("x_proj","y_proj")], 
                    ordered_corner = T)) ]
      } else {
        tree_data[plot_ID %in% unique(corner_dat$plot_ID),
                  c("x_proj","y_proj") := as.list(
                    as.data.frame(
                      sweep(as.matrix(.SD[, c("x_rel","y_rel")]) %*% procrust_res$rotation, 2, procrust_res$translation, FUN = "+")))]
      }
      
      # Add a column telling if a tree is inside the plot or not 
      tree_data[plot_ID %in% unique(corner_dat$plot_ID), is_in_plot := x_rel %between% range(corner_dat$x_rel) & y_rel %between% range(corner_dat$y_rel)]
      if(any(! tree_data[plot_ID %in% unique(corner_dat$plot_ID) , ][["is_in_plot"]])) { 
        warning(
          paste( ifelse(unique(corner_dat$plot_ID)=="", "", paste("In plot", unique(corner_dat$plot_ID), ":")) , 
                 "Be careful, one or more trees are not inside the plot defined by rel_coord (see is_in_plot column of tree_data output)\n"))
      }
    }
    
    # Return
    corner_dat[, plot_ID := NULL]
    return(corner_dat)
    
  }
  
  # Apply check_corner_fct to all plots
  corner_checked <- corner_dt[, check_corner_fct(.SD), by = plot_ID, .SDcols = colnames(corner_dt)]
  
  ### Create polygon -----------------------------------------------------------
  create_polygon <- function(dat) {
    corner_polygon <- st_multipoint(as.matrix(rbind(dat[,c("x_proj","y_proj")], dat[1,c("x_proj","y_proj")])))
    corner_polygon <- st_polygon(x = list(corner_polygon), dim = "XY")
  }
  
  # Apply create_polygons to all plots and convert into simple feature geometry list column
  corner_polygon <- st_sfc( lapply( split(corner_checked,corner_checked$plot_ID) , create_polygon ), 
                            crs = ifelse(!is.null(longlat), UTM_code$UTM_code, ""))
    
  ### Draw the plot ------------------------------------------------------------
  draw_plot_fct <-  function(corner_dat) { # corner_dat = corner_dt
    
    current_plot_ID <- unique(corner_dat$plot_ID)
    corner_dat <- corner_dat[, c("plot_ID","x_proj","y_proj","x_rel","y_rel")]
    
    # Raster CHM
    if(!is.null(ref_raster)) {
      # Convert the polygon into a terra::SpatVector
      terra_polygon <- vect(corner_polygon[[match(current_plot_ID,names(corner_polygon))]])
      # Crop raster to represent the plot
      plot_raster <- crop(ref_raster, terra_polygon, mask=T )
      # Convert the SpatRaster into a data frame
      plot_raster <- as.data.frame(plot_raster, xy=TRUE)
      # plot the raster
      plot_design <- ggplot() +
        geom_raster(data = plot_raster, mapping = aes(x = x, y = y, fill = .data[[names(plot_raster)[3]]] ) ) +
        scale_fill_gradientn(colours = rev(terrain.colors(10)))
    } else {
      plot_design <- ggplot()
    }
    
    # All GPS measurements :
    corner_dat[ , whatpoint := "GPS measurements"]
    
    # Outliers 
    corner_dat[outliers[plot_ID == current_plot_ID] , whatpoint :=  "GPS outliers (discarded)" , on = c("plot_ID","x_proj","y_proj")]
    
    # Reference corners :
    corner_checked[plot_ID == current_plot_ID, whatpoint := "Reference corners"]
    corner_dat <- rbind(corner_dat, corner_checked[plot_ID == current_plot_ID, c("plot_ID","x_proj","y_proj","x_rel","y_rel","whatpoint")])
    corner_checked[, whatpoint := NULL]
    
    # x_rel and y_rel arrows :
    x0 <- corner_dat[whatpoint == "Reference corners" , x_proj][1]
    y0 <- corner_dat[whatpoint == "Reference corners" , y_proj][1]
    x1 <- corner_dat[whatpoint == "Reference corners" , x_proj][2]
    y1 <- corner_dat[whatpoint == "Reference corners" , y_proj][2]
    x2 <- corner_dat[whatpoint == "Reference corners" , x_proj][4]
    y2 <- corner_dat[whatpoint == "Reference corners" , y_proj][4]
    arrow_plot <- data.frame(x = x0, y = y0,
                             x_end = c(x0 + (x1-x0) / 4, x0 + (x2-x0) / 4),
                             y_end = c(y0 + (y1-y0) / 4, y0 + (y2-y0) / 4))
    x0_rel <- corner_dat[whatpoint == "Reference corners" , x_rel][1]
    y0_rel <- corner_dat[whatpoint == "Reference corners" , y_rel][1]
    x1_rel <- corner_dat[whatpoint == "Reference corners" , x_rel][2]
    y1_rel <- corner_dat[whatpoint == "Reference corners" , y_rel][2]
    x2_rel <- corner_dat[whatpoint == "Reference corners" , x_rel][4]
    y2_rel <- corner_dat[whatpoint == "Reference corners" , y_rel][4]
    pos_text_arrow <- bilinear_interpolation( 
      coord = data.frame(
        x = c(x0_rel + (x1_rel-x0_rel) / 4 , x0_rel + (x2_rel-x0_rel) / 4 - (x1_rel-x0_rel) / 8) ,
        y = c(y0_rel + (y1_rel-y0_rel) / 4 - (y2_rel-y0_rel) / 8, y0_rel + (y2_rel-y0_rel) / 4) ) ,
      from_corner_coord =  corner_dat[whatpoint == "Reference corners" , c("x_rel","y_rel")],
      to_corner_coord = corner_dat[whatpoint == "Reference corners" , c("x_proj","y_proj")])
    
    # Plot Corners
    plot_design <- plot_design + 
      geom_point(data = corner_dat, mapping = aes(x = x_proj, y = y_proj, col = whatpoint, shape = whatpoint), size=2) + 
      geom_polygon(data = corner_polygon[[match(current_plot_ID,names(corner_polygon))]][[1]][,] , mapping = aes(x=x_proj,y=y_proj), colour="black",fill=NA,linewidth=1.2)+
      geom_segment(data=arrow_plot, mapping=aes(x=x,y=y,xend=x_end,yend=y_end), arrow=arrow(length=unit(0.4,"cm")),linewidth=1, col="blue") +
      geom_text(data = pos_text_arrow, mapping = aes(x=x_proj, y=y_proj, label=c("x_rel","y_rel")), col="blue" ) +
      ggtitle(paste("Plot",current_plot_ID)) +
      theme_minimal() + 
      coord_equal()
    
    cols <- c("GPS measurements"="black", "GPS outliers (discarded)"="red", "Reference corners"="black")
    shapes <- c("GPS measurements"=2, "GPS outliers (discarded)"=4, "Reference corners"=15, "Trees"=1,"Trees outside the plot"=13)
    
    plot_design <- plot_design +
      scale_color_manual(values=cols, guide = guide_legend("Corners", order=1)) + 
      scale_shape_manual(values=shapes, guide = guide_legend("Corners", order=1))
    
    # Plot Trees :
    if(!is.null(tree_data)) {
      
      if(is.null(prop_tree)) { # if there is no proportional variable to display
        
        tree_data[ plot_ID == current_plot_ID, tree_shape := ifelse(is_in_plot,1,13)]
        tree_data[ plot_ID == current_plot_ID, tree_label := ifelse(is_in_plot," ","outside the plot")] # Tree labels for the legend
        
        plot_design <- plot_design +
          geom_point(data = tree_data[plot_ID == current_plot_ID, ],
                     mapping = aes(x = x_proj, y = y_proj, alpha = tree_label), # as color and shape are already in corner aes, we fake an alpha aes to get a proper legend
                     shape = tree_data[plot_ID == current_plot_ID, ][["tree_shape"]],
                     col="grey25", size = 2) +
          scale_alpha_manual(values = c(" "=1,"outside the plot"=1)) + # set the alpha to 1
          guides( alpha = guide_legend(title = 'Trees', 
                                       override.aes = list(shape = if(sum(!tree_data[plot_ID == current_plot_ID, "is_in_plot"])==0) c(1) else c(1,13) ) ) )
        
        tree_data[, tree_label := NULL]
        tree_data[, tree_shape := NULL]
        
      } else { # if there is a proportional variable to display
        
        plot_design <- plot_design +
          # Display the trees inside the plot
          geom_point(data = tree_data[plot_ID == current_plot_ID & is_in_plot==T, ], 
                     mapping = aes(x = x_proj, y = y_proj,
                                   size = .data[[prop_tree]],
                                   alpha = .data[[prop_tree]]), 
                     shape = 1) +
          scale_alpha(range = c(0.2,1)) +
          scale_size(range = c(0.1,6)) + 
          guides( alpha = guide_legend(title = paste('Trees :', prop_tree), order = 2),
                  size = guide_legend(title = paste('Trees :', prop_tree), order = 2, override.aes = list(shape = 1) )) +
          # Display the trees outside the plot (but don't display them in the legend)
          geom_point(data = tree_data[plot_ID == current_plot_ID & is_in_plot==F, ],
                     mapping = aes(x = x_proj, y = y_proj),
                     shape = 13, size = 2)
      }
    }
      
    if(draw_plot) print(plot_design)
    
    return(invisible(plot_design))
  }
  
  # Apply draw_plot_fct to all plots
  if(nrow(corner_checked) == 4) ask <- FALSE
  oldpar <- par(ask = ask)
  ggplot_list <- lapply(split(corner_dt, corner_dt$plot_ID), draw_plot_fct)
  on.exit(par(oldpar))
  
  
  ##### Returns ----------------------------------------------------------------
  
  if(all(unique(corner_checked$plot_ID) == "")) corner_checked[, plot_ID := NULL] 
  if(length(ggplot_list) == 1) ggplot_list <- ggplot_list[[1]] 
  
  output <- list(
    corner_coord = as.data.frame(corner_checked),
    polygon = st_as_sf(corner_polygon),
    plot_design = ggplot_list
    )
  
  if (nrow(outliers) != 0) {
    if(all(outliers[,plot_ID]=="")) {
      outliers[, plot_ID := NULL]
    }
    output$outlier_corners <- as.data.frame(outliers)
  }

  if (!is.null(longlat)) {
    if(all(UTM_code[,plot_ID]=="")) {
      UTM_code$plot_ID <- NULL
    }
    output$UTM_code <- UTM_code
  }
  
  if (!is.null(tree_data)) {
    if(all(tree_data[,plot_ID]=="")) {
      tree_data[, plot_ID := NULL]
    }
    output$tree_data <- as.data.frame(tree_data)
  }
  
  return(output)
}
