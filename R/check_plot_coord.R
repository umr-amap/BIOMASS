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
#' @param corner_data A data frame, data frame extension, containing the plot corner coordinates.
#' @param proj_coord (optional, if longlat is not supplied) A character vector specifying the column names of the corner projected coordinates (x,y)
#' @param longlat (optional, if proj_coord is not supplied) A character vector specifying the column names of the corner geographic coordinates (long,lat).
#' @param rel_coord A character vector specifying the column names of the corner relative coordinates (that of the field, ie, the local ones).
#' @param trust_GPS_corners A logical indicating whether or not you trust the GPS coordinates of the plot's corners. See details.
#' @param draw_plot A logical indicating if the plot design should be displayed and returned.
#' @param tree_data A data frame, data frame extension, containing the relative coordinates (field/local coordinates) of the trees and optional other tree metrics.
#' @param tree_coords A character vector specifying the column names of the tree relative coordinates.
#' @param corner_ID If dealing with repeated measurements of each corner : a character specifying the ID of the corners.
#' @param max_dist If dealing with repeated measurements of each corner : the maximum distance (in meters) above which GPS measurements should be considered outliers (default 15 m).
#' @param rm_outliers If TRUE and dealing with repeated measurements of each corner, then outliers are removed from the coordinate calculation of the referenced corners.
#' @param plot_ID If dealing with multiple plots : a character indicating the variable name for corner plot IDs in corner_data.
#' @param tree_plot_ID If dealing with multiple plots : a character indicating the variable name for tree plot IDs in tree_data.
#' @param ask If TRUE and dealing with multiple plots, then prompt user before displaying each plot. 
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN, Arthur BAILLY
#'
#' @return Returns a list including :
#'    - `corner_coord`: a data frame containing the projected coordinates (x_proj and y_proj), the relative coordinates (x_rel and y_rel), and the ID (if corner_ID is supplied) of the 4 corners of the plot 
#'    - `polygon`: a spatial polygon
#'    - `tree_proj_coord`: if `tree_data` is supplied, a data frame containing the coordinates of the trees in the projected coordinate system (x_proj and y_proj)
#'    - `outliers`: a data frame containing the projected coordinates, the ID (if corner_ID is supplied) and the row number of GPS measurements considered outliers 
#'    - `plot_design`: if `draw_plot` is TRUE, a ggplot object corresponding to the design of the plot
#'    - `UTM_code`: if `longlat` is supplied, a character containing the UTM code of the GPS coordinates
#'
#' @export
#'
#' @importFrom data.table data.table := setnames %between% copy
#' @importFrom sf st_multipoint st_polygon st_sfc
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_polygon geom_text scale_shape_manual scale_color_manual ggtitle theme_minimal theme coord_equal arrow unit element_blank
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN, Arthur BAILLY
#'
#' @examples
#' 
#' set.seed(52)
#' corner_data <- data.frame(
#'   x_proj = c( rnorm(5, 0, 5), rnorm(5, 0, 5),
#'               rnorm(5, 100, 5), rnorm(5, 100, 5)) + 1000,
#'   y_proj = c( rnorm(5, 0, 5), rnorm(5, 100, 5),
#'               rnorm(5, 0, 5), rnorm(5, 100, 5)) + 1000,
#'   x_rel = c(rep(0, 10), rep(100, 10)),
#'   y_rel = c(rep(c(rep(0, 5), rep(100, 5)), 2)),
#'   corner_ID = rep(c("SW","NW","SE","NE"),e=5) )
#' 
#' aa <- check_plot_coord(
#'  corner_data = corner_data,
#'  proj_coord = c("x_proj","y_proj"), rel_coord = c("x_rel","y_rel"),
#'  trust_GPS_corners = TRUE,
#'  corner_ID = "corner_ID",
#'  draw_plot = FALSE, rm_outliers = F
#' )
#' 
#' aa$corner_coord
#' \donttest{
#'   aa$plot_design
#' }

check_plot_coord <- function(corner_data, proj_coord = NULL, longlat = NULL, rel_coord, trust_GPS_corners, draw_plot = TRUE, tree_data = NULL, tree_coords=NULL, corner_ID=NULL, max_dist = 10, rm_outliers = TRUE, plot_ID = NULL, tree_plot_ID = NULL, ask = T) {
  
  ##### Checking arguments -----------------------------------------------------
  
  if(is.data.frame(rel_coord)) {
    stop("The way in which arguments are supplied to the function has changed since version 2.2.1. You now have to supply corner_data data frame and it associated coordinates variable names.")
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
  if(!is.null(tree_data) && !is.data.frame(tree_data)){
    stop("tree_data must a data frame or a data frame extension")
  }
  if (!is.null(tree_data) && is.null(tree_coords)) {
    stop("You must supply the column names corresponding to the relative coordinates of tree_data using the argument `tree_coords`")
  }
  if (!is.null(tree_data) && !any(tree_coords %in% names(tree_data))) {
    stop("column names supplied by tree_coords are not found in tree_data")
  }
  if (is.null(trust_GPS_corners) | !is.logical(trust_GPS_corners)) {
    stop("The trust_GPS_corners argument must be TRUE or FALSE")
  }
  if (length(max_dist) != 1) {
    stop("The max_dist argument must be of length 1")
  }
  if(sum(is.na(corner_dt[,..rel_coord]))!= 0) {
    stop("Missing values are detected in corner relative coordinates. Please remove them and call the function again")
  }
  if(!is.null(longlat) && sum(is.na(corner_dt[,..longlat]))!= 0) {
    stop("Missing values are detected in longitude/latitude coordinates. Please remove them and call the function again")
  }
  if(!is.null(proj_coord) && sum(is.na(corner_dt[,..proj_coord]))!= 0) {
    stop("Missing values are detected in projected coordinates. Please remove them and call the function again")
  }
  if (nrow(corner_dt)!=4 & !(!is.null(plot_ID) | !is.null(corner_ID))) {
    stop("The argument plot_ID is required to handle multiple plots. If multiple measurements of each corner have been recorded, then the argument corner_ID is required.")
  }
  if (!is.null(plot_ID) && !any(plot_ID==names(corner_dt))) {
    stop(paste(plot_ID,"is not found in corner_data column names."))
  }
  if (!is.null(corner_ID) && !any(corner_ID==names(corner_dt))) {
    stop(paste(corner_ID,"is not found in corner_data column names."))
  }
  if(!is.null(tree_data) && !is.null(plot_ID) && is.null(tree_plot_ID)) {
    stop("The argument tree_plot_ID is required if plot_ID is supplied.")
  }
  if(!is.null(tree_data) && !is.null(tree_plot_ID) && any(! unique(tree_data[[tree_plot_ID]]) %in% unique(corner_dt[[plot_ID]])) ) {
    warning( paste( "These ID's are found in tree_plot_ID but not in plot_ID :" , paste(unique(tree_data[[tree_plot_ID]])[! unique(tree_data[[tree_plot_ID]]) %in% unique(corner_dt[[plot_ID]])] , collapse = " ")) )
  }
  
  
  ##### Data processing --------------------------------------------------------
  
  # Formatting corner data
  setnames(corner_dt, old = rel_coord, new = c("x_rel","y_rel"))
  
  if(!is.null(proj_coord)) {
    setnames(corner_dt, old = proj_coord, new = c("x_proj","y_proj"))
  } else{
    setnames(corner_dt, old = longlat, new = c("long","lat"))
  }
  
  if( !is.null(corner_ID) ) setnames(corner_dt, old = corner_ID, new = "corner_ID")
  
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
  }
  
  outliers <- data.table("plot_ID" = character(), "x_proj" = numeric(), "y_proj" = numeric(), "corner_ID" = character(), "row_number" = integer())
  
  ##### Functions --------------------------------------------------------------
  
  ### Transform the geographic coordinates into UTM coordinates ----------------
  
  latlong2UTM_fct <- function(dat) { # dat = corner_dt[plot_ID == "NB1" , ]
    proj_coord <- latlong2UTM(dat[, c("long","lat")])
    UTM_code <- unique(proj_coord[, "codeUTM"])
    if(length(UTM_code)>1) {
      stop(paste(unique(dat$plot_ID), "More than one UTM zone are detected. This may be due to an error in the long/lat coordinates, or if the parcel is located right between two UTM zones. In this case, please convert yourself your long/lat coordinates into any projected coordinates which have the same dimension than your local coordinates"))
    }
    corner_dt[plot_ID %in% dat$plot_ID, c("x_proj", "y_proj") := list(x_proj = proj_coord$X, y_proj = proj_coord$Y)]
    return(data.frame(UTM_code = UTM_code))
  }
  
  # Apply latlong2UTM_fct to all plots if necessary
  if(is.null(proj_coord)) {
    UTM_code <- corner_dt[, latlong2UTM_fct(.SD), by = plot_ID, .SDcols = colnames(corner_dt)]
  }
  
  ### Check corner coordinates and calculate tree projected coordinates --------
  
  check_corner_fct <- function(dat) { # dat = corner_dt
    
    corner_dat <- copy(dat) # create a copy to enable the use of :=
    
    # Checking for unexpected corner
    if (nrow(unique(corner_dat[,c("x_rel","y_rel")])) != 4) {
      stop(paste(nrow(unique(corner_dat[,c("x_rel","y_rel")])), "unique corners are detected in",unique(corner_dat$plot_ID),"rel_coord (instead of 4)"))
    }
    if (!is.null(corner_ID) && length(unique(corner_dat$corner_ID)) !=4) {
      stop(paste(length((unique(corner_dat$corner_ID))),"unique corners are detected in",unique(corner_dat$plot_ID),"corner_ID (instead of 4)"))
    }
    
    # Removing outliers when repeated corner measurements and formatting corner data 
    
    if(nrow(corner_dat) != 4) { # if multiple measurements of each corner
      
      if (any(table(corner_dat$corner_ID) < 5)) {
        warning(
          paste( ifelse(unique(corner_dat$plot_ID)=="", "", paste("In plot", unique(corner_dat$plot_ID), ":")) ,
                 "At least one corner has less than 5 measurements. We suggest using the argument trust_GPS_corners = FALSE"))
      }
      
      if(trust_GPS_corners == TRUE) {
        
        corner_dat[ , c("x_proj_mean", "y_proj_mean", "row_number") := list(mean(x_proj), mean(y_proj), .I) , by=corner_ID] 
        
      } else {
        
        procrust_res <- procrust(corner_dat[, c("x_proj","y_proj")], corner_dat[, c("x_rel","y_rel")])
        procrust_coord <- as.matrix(corner_dat[, c("x_rel","y_rel")]) %*% procrust_res$rotation
        procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
        
        corner_dat[ , c("x_proj_mean","y_proj_mean","row_number") := list(procrust_coord[,1], procrust_coord[,2], .I)] 
      }
      
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
                "Removing them may improve the georeferencing of your plot (see rm_outliers and max_dist arguments)."))
      }
      
      if( rm_outliers & sum(corner_dat$outlier)!=0 ) {
        
        outliers_dat <- corner_dat[outlier==T , c("plot_ID","x_proj","y_proj","corner_ID","row_number")]
        corner_dat <- corner_dat[outlier==F ,]
        exist_outliers <- TRUE
        while(exist_outliers) {
          
          if(trust_GPS_corners == TRUE) {
            corner_dat[ , c("x_proj_mean", "y_proj_mean") := list(mean(x_proj), mean(y_proj)) , by=corner_ID] 
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
            outliers_dat <- rbind(outliers_dat , corner_dat[outlier==T , c("plot_ID","x_proj","y_proj","corner_ID","row_number")])
            corner_dat <- corner_dat[outlier==F ,]
          }
        }
        outliers <<- rbindlist(list(outliers, outliers_dat))
      }
        
      corner_dat <- unique(corner_dat[ , c("plot_ID","x_rel","y_rel","x_proj_mean","y_proj_mean","corner_ID")])
      setnames(corner_dat, old = c("x_proj_mean","y_proj_mean") , new = c("x_proj", "y_proj"))
      
       
      # End if multiple measurements of each corner 
      
    } else { # if only 1 measurements of each corners 
      
      if(trust_GPS_corners == FALSE) {
        
        # Transformation of relative to projected coordinates by a procrust analyses
        procrust_res <- procrust(corner_dat[, c("x_proj","y_proj")], corner_dat[, c("x_rel","y_rel")])
        
        procrust_coord <- as.matrix(corner_dat[, c("x_rel","y_rel")]) %*% procrust_res$rotation
        procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
        
        corner_dat[ , c("x_proj","y_proj") := list(procrust_coord[,1], procrust_coord[,2]) ]
      }
      
      if(!is.null(corner_ID)) {
        corner_dat <- corner_dat[ , c("plot_ID","x_rel","y_rel","x_proj","y_proj","corner_ID")]
      } else {
        corner_dat <- corner_dat[ , c("plot_ID","x_rel","y_rel","x_proj","y_proj")]
      }
      
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
                 "Be careful, one or more trees are not inside the plot defined by rel_coord (see is_in_plot column of tree_proj_coord output)"))
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
  corner_polygon <- st_sfc( lapply( split(corner_checked,corner_checked$plot_ID) , create_polygon ) )
    
  ### Draw the plot ------------------------------------------------------------
  draw_plot_fct <-  function(corner_dat) { # corner_dat = corner_dt[plot_ID=="",]
    
    current_plot_ID <- unique(corner_dat$plot_ID)
    corner_dat <- corner_dat[, c("plot_ID","x_proj","y_proj")]
    
    # All GPS measurements :
    corner_dat[ , whatpoint := "GPS measurements"]
    
    # Outliers 
    corner_dat[outliers[plot_ID == current_plot_ID] , whatpoint :=  "Outliers (discarded)" , on = c("plot_ID","x_proj","y_proj")]
    
    # Reference corners :
    corner_checked[plot_ID == current_plot_ID, whatpoint := "Reference corners"]
    corner_dat <- rbind(corner_dat, corner_checked[plot_ID == current_plot_ID, c("plot_ID","x_proj","y_proj","whatpoint")])
    corner_checked[, whatpoint := NULL]
    
    # Trees :
    if(!is.null(tree_data)) {
      tree_data[ plot_ID == current_plot_ID, whatpoint := ifelse(is_in_plot,"Trees","Trees outside the plot")]
      corner_dat <- rbind(corner_dat,tree_data[plot_ID == current_plot_ID, c("plot_ID","x_proj","y_proj","whatpoint")])
      tree_data[, whatpoint := NULL]
    }
    
    corner_dat[, whatpoint := factor(whatpoint, levels = c("GPS measurements","Outliers (discarded)","Reference corners","Trees","Trees outside the plot"))]
    
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
    
    plot_design <- ggplot2::ggplot(data = corner_dat) +
      geom_point(aes(x = x_proj, y = y_proj, col = whatpoint, shape = whatpoint), size=2) + 
        scale_shape_manual(values=c(2,4,15,1,1), drop=FALSE) +
        scale_color_manual(values=c('black','red',"black","#994F00","#006CD1"), drop = FALSE) +
        geom_polygon(data = corner_polygon[[match(current_plot_ID,names(corner_polygon))]][[1]][,] , mapping = aes(x=x_proj,y=y_proj), colour="black",fill=NA,linewidth=1.2)+
        geom_segment(data=arrow_plot, mapping=aes(x=x,y=y,xend=x_end,yend=y_end), arrow=arrow(length=unit(0.4,"cm")),linewidth=1, col="blue") +
        geom_text(data = arrow_plot, mapping = aes(x=x_end,y=y_end,label=c("x_rel","y_rel")), hjust=c(0,-0.3), vjust=c(-0.5,0), col="blue" ) +
        ggtitle(paste("Plot",current_plot_ID)) +
        theme_minimal() + 
        theme(legend.title=element_blank())+
        coord_equal()
    
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
    polygon = as.data.frame(corner_polygon),
    plot_design = ggplot_list
    )
  
  if (nrow(outliers) != 0) {
    output$outlier_corners <- as.data.frame(outliers)
  }

  if (!is.null(longlat)) {
    output$UTM_code <- UTM_code
  }
  
  if (!is.null(tree_data)) {
    output$tree_proj_coord <- as.data.frame(tree_data)
  }
  
  return(output)
}
