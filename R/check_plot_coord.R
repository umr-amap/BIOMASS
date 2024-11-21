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
#' @param proj_coord (optional, if longlat is not supplied) a data frame containing the plot corner coordinates in a projected coordinate system, with X and Y corresponding to the first and second columns respectively.
#' @param longlat (optional, if proj_coord is not supplied) a data frame containing the plot corner coordinates in longitude and latitude, with longitude and latitude corresponding to the first and second columns respectively
#' @param rel_coord a data frame containing the plot corner coordinates in the relative coordinates system (that of the field), with X and Y corresponding to the first and second columns respectively, and with the same row order than longlat or proj_coord
#' @param trust_GPS_corners a logical indicating whether or not you trust the GPS coordinates of the plot's corners. See details.
#' @param corner_ID a vector indicating the ID of the corners (e.g c("SE","SE",...)) in the case you have multiple measurements for each corner
#' @param max_dist a numeric giving the maximum distance (in meters) above which GPS measurements should be considered outliers (default 15 m)
#' @param rm_outliers a logical indicating if detected outliers are removed from the coordinate calculation
#' @param draw_plot a logical indicating if the plot design should be displayed and returned
#' @param tree_df a data frame containing the relative coordinates (field/local coordinates) of the trees and optional other tree metrics
#' @param tree_coords a character vector of length 2 containing the column names of the relative coordinates in tree_df
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN, Arthur BAILLY
#'
#' @return Returns a list including :
#'    - `corner_coord`: a data frame containing the projected coordinates (x_proj and y_proj), the relative coordinates (x_rel and y_rel), and the ID (if corner_ID is supplied) of the 4 corners of the plot 
#'    - `polygon`: a spatial polygon
#'    - `outliers`: a data frame containing the projected coordinates, the ID (if corner_ID is supplied) and the row number of GPS points considered outliers 
#'    - `plot_design`: if `draw_plot` is TRUE, a ggplot object corresponding to the design of the plot
#'    - `UTM_code`: if `longlat` is supplied, a character containing the UTM code of the GPS coordinates
#'    - `tree_proj_coord`: if `tree_df` is supplied, a data frame containing the coordinates of the trees in the projected coordinate system (x_proj and y_proj)
#'
#' @export
#'
#' @importFrom data.table data.table := setnames %between%
#' @importFrom sf st_multipoint st_polygon st_sfc
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_polygon geom_text scale_shape_manual scale_color_manual ggtitle theme_minimal theme coord_equal arrow unit element_blank
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN, Arthur BAILLY
#'
#' @examples
#' proj_coord <- data.frame(
#'   X = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 8, max = 12),
#'     runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)
#'   ),
#'   Y = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 80, max = 120),
#'     runif(5, min = 8, max = 12), runif(5, min = 90, max = 110)
#'   )
#' )
#' proj_coord <- proj_coord + 1000
#' rel_coord <- data.frame(
#'   X = c(rep(0, 10), rep(100, 10)),
#'   Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
#' )
#' corner_ID <- rep(c("SW","NW","SE","NE"),e=5)
#'
#' aa <- check_plot_coord(
#'   proj_coord = proj_coord, rel_coord = rel_coord,
#'   trust_GPS_corners = TRUE, corner_ID = corner_ID,
#'   rm_outliers = FALSE , draw_plot = FALSE
#' )
#' bb <- check_plot_coord(
#'   proj_coord = proj_coord, rel_coord = rel_coord,
#'   trust_GPS_corners = FALSE, 
#'   rm_outliers = TRUE, max_dist = 10,
#'   draw_plot = FALSE
#' )
#' \donttest{
#' check_plot_coord(
#'   proj_coord = proj_coord, rel_coord = rel_coord,
#'   trust_GPS_corners = TRUE, corner_ID = corner_ID,
#'   rm_outliers = TRUE , max_dist = 10,
#'   draw_plot = TRUE
#' )
#' }
#'
check_plot_coord <- function(proj_coord = NULL, longlat = NULL, rel_coord, trust_GPS_corners, corner_ID=NULL, max_dist = 15, rm_outliers = TRUE,  draw_plot = TRUE, tree_df = NULL, tree_coords=NULL) {
  
  # Checking arguments -------------------------------------------------
  
  if (is.null(longlat) && is.null(proj_coord)) {
    stop("Give at least one set of coordinates: longlat or proj_coord")
  }
  if (!is.null(longlat) & !is.null(proj_coord)) {
    stop("Give only one set of coordinates: longlat or proj_coord")
  }
  if((!is.matrix(rel_coord) & !is.data.frame(rel_coord))){
    stop("rel_coord must be a matrix or a data frame")
  }
  if(!is.null(longlat) && (!is.matrix(longlat) & !is.data.frame(longlat))){
    stop("longlat must be a matrix or a data frame")
  }
  if(!is.null(proj_coord) && (!is.matrix(proj_coord) & !is.data.frame(proj_coord))){
    stop("proj_coord must be a matrix or a data frame")
  }
  if(!is.null(tree_df) && (!is.matrix(tree_df) & !is.data.frame(tree_df))){
    stop("tree_df must be a matrix or a data frame")
  }
  if (!is.null(tree_df) && is.null(tree_coords)) {
    stop("You must supply the column names corresponding to the relative coordinates of tree_df using the argument `tree_coords`")
  }
  if (!is.null(tree_df) && !any(tree_coords %in% names(tree_df))) {
    stop("column names supplied by tree_coords are not found in tree_df")
  }
  if (is.null(trust_GPS_corners) | !is.logical(trust_GPS_corners)) {
    stop("The trust_GPS_corners argument must be TRUE or FALSE")
  }
  if (length(max_dist) != 1) {
    stop("The max_dist argument must be of length 1")
  }
  if ((!is.null(longlat) && any(dim(longlat) != dim(rel_coord))) || (!is.null(proj_coord) && any(dim(proj_coord) != dim(rel_coord)))) {
    stop("GPS and relative coordinates are not of the same dimension")
  }
  if(sum(is.na(rel_coord))!= 0) {
    stop("Missing values are detected in rel_coord. Please remove them and call the function again")
  }
  if(!is.null(longlat) && sum(is.na(longlat))!= 0) {
    stop("Missing values are detected in longlat. Please remove them and call the function again")
  }
  if(!is.null(proj_coord) && sum(is.na(proj_coord))!= 0) {
    stop("Missing values are detected in proj_coord. Please remove them and call the function again")
  }
  if (trust_GPS_corners==T && (!is.null(proj_coord)) && nrow(proj_coord)!=4 & is.null(corner_ID)) {
    stop("The argument corner_ID is needed if trust_GPS_corners is TRUE and if multiple measurements of each corner have been realized")
  }
  if (!is.null(corner_ID) && length(corner_ID) != nrow(rel_coord)) {
    stop("corner_ID must be the same length as the number of rows in rel_coord")
  }
  
  # function -------------------------------------------------------------------
  
  # Transform the geographic coordinates into UTM coordinates
  if (!is.null(longlat)) {
    proj_coord <- latlong2UTM(longlat)
    UTM_code <- unique(proj_coord[, "codeUTM"])
    if(length(UTM_code)>1) {
      stop("More than one UTM zone are detected. This may be due to an error in the long/lat coordinates, or if the parcel is located right between two UTM zones. In this case, please convert yourself your long/lat coordinates into any projected coordinates which have the same dimension than your local coordinates")
    }
    proj_coord <- proj_coord[, c("X", "Y")]
  }
  
  if(trust_GPS_corners == TRUE) {
    
    if(nrow(proj_coord)!= 4) { # if multiple measures of each corner, then do the mean of coordinates and search for outliers
      
      corner_coord <- data.table(cbind(proj_coord[,1:2], rel_coord[,1:2],corner_ID=corner_ID))
      setnames(corner_coord, c("x_proj","y_proj","x_rel","y_rel","corner_ID"))
      if (any(table(corner_coord$corner_ID) < 5)) {
        warning("At least one corner has less than 5 measurements. We suggest using the argument trust_GPS_corners = FALSE")
      }
      corner_coord[ , c("Xmean","Ymean","row_number") := list(mean(x_proj),mean(y_proj),.I) , by=corner_ID] 
      corner_coord[ , outlier := ifelse( sqrt((x_proj - Xmean)^2 + (y_proj-Ymean)^2) > max_dist, T, F)]
      outliers <- corner_coord[outlier==T , c("x_proj","y_proj","corner_ID","row_number")]
      
      if (any(corner_coord[,sum(!outlier),by=corner_ID][[2]]==0)) {
        stop("All measurements for at least one corner are considered as outliers.\n
       This may be because some coordinates have very large error associated.\n
       Try to remove these very large error or reconsider the max_dist parameter by increasing the distance")
      }
      
      if(rm_outliers & nrow(outliers)!=0) {
        corner_coord <- corner_coord[outlier==F ,]
        exist_outliers <- T
        while(exist_outliers) {
          corner_coord[ , outlier := ifelse( sqrt((x_proj - Xmean)^2 + (y_proj-Ymean)^2) > max_dist, T, F)]
          outliers <- rbind(outliers , corner_coord[outlier==T , c("x_proj","y_proj","corner_ID","row_number")])
          corner_coord <- corner_coord[outlier==F ,]
          if(nrow(corner_coord)+nrow(outliers) == nrow(proj_coord)) exist_outliers <- F
          corner_coord[ , c("Xmean","Ymean") := list(mean(x_proj),mean(y_proj)) , by=corner_ID]
        }
      }
      
      corner_coord <- data.frame(corner_coord[ , c("Xmean","Ymean","x_rel","y_rel","corner_ID")])
      corner_coord <- unique(corner_coord)
      colnames(corner_coord) <- c("x_proj", "y_proj","x_rel","y_rel","corner_ID")
      
    } else { # if exactly 1 measures for each corner
      corner_coord <- data.frame(cbind(proj_coord[,1:2], rel_coord[,1:2]))
      colnames(corner_coord) <- c("x_proj","y_proj","x_rel","y_rel")
      outliers <- data.frame()
      if(!is.null(corner_ID)) corner_coord$corner_ID = corner_ID
    }
    
  } else { # trust_GPS_corners = "FALSE"
    
    corner_coord <- data.table(cbind(proj_coord[,1:2], rel_coord[,1:2]))
    setnames(corner_coord, c("x_proj","y_proj","x_rel","y_rel"))
    corner_coord[, row_number := .I]
    
    # Transformation of rel_coord to projected coordinates by a procrust analyses
    procrust_res <- procrust(proj_coord, rel_coord)
    procrust_coord <- as.matrix(rel_coord) %*% procrust_res$rotation
    procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
    
    # Calculate the distances between the GNSS measurements and the procrust_coord
    corner_coord[ , outlier := ifelse(
      sqrt((procrust_coord[, 1] - proj_coord[, 1])^2 + (procrust_coord[, 2] - proj_coord[, 2])^2) > max_dist, T, F)]
    
    outliers <- corner_coord[outlier==T , c("x_proj","y_proj","row_number")]
    
    if (length(outliers)==nrow(proj_coord)){
      stop("All coordinates points are considered as outliers at the first stage.\n
         This may be because some coordinates have very large error associated.\n
         Try to remove these very large error or reconsider the max_dist parameter by increasing the distance")
    }
    
    # retransform the rel_coord without the outliers
    if (rm_outliers & nrow(outliers)>0) {
      corner_coord <- corner_coord[outlier==F ,]
      refineCoord <- TRUE
      while(refineCoord){
        procrust_res <- procrust(proj_coord[-outliers$row_number, ], rel_coord[-outliers$row_number,])
        procrust_coord <- as.matrix(rel_coord[-outliers$row_number,]) %*% procrust_res$rotation
        procrust_coord <- sweep(procrust_coord, 2, procrust_res$translation, FUN = "+")
        corner_coord[ , outlier := ifelse(
          sqrt((procrust_coord[, 1] - proj_coord[-outliers$row_number, 1])^2 + 
                 (procrust_coord[, 2] - proj_coord[-outliers$row_number, 2])^2) > max_dist, T, F)]
        
        outliers <- rbind(outliers , corner_coord[outlier==T , c("x_proj","y_proj","row_number")])
        corner_coord <- corner_coord[outlier==F ,]
        if(nrow(corner_coord)+nrow(outliers) == nrow(proj_coord)) refineCoord <- F
      }
    }
    
    # Create the data.table of corners to return the projected coordinate of the plot's corner 
    corner_rel_coord <- unique(rel_coord)
    
    # Project corner_rel_coord
    corner_proj_coord <- as.matrix(corner_rel_coord[,1:2]) %*% procrust_res$rotation
    corner_proj_coord <- sweep(corner_proj_coord, 2, procrust_res$translation, FUN = "+")
    
    corner_coord <- data.frame(cbind(corner_proj_coord[,1:2], corner_rel_coord[,1:2]))
    colnames(corner_coord) <- c("x_proj","y_proj","x_rel","y_rel")
    
    if(!is.null(corner_ID)) corner_coord$corner_ID <- unique(corner_ID)
  } # End trust_GPS_corners = "FALSE"
  
  # Sort corner_coord rows in a counter-clockwise direction ---------------------
  centroid <- colMeans(corner_coord[,c("x_rel","y_rel")])
  angles <- base::atan2(corner_coord[["y_rel"]] - centroid[2], corner_coord[["x_rel"]] - centroid[1])
  corner_coord <- corner_coord[order(angles), ]
  
  # Create a polygon -----------------------------------------------------------
  corner_polygon <- st_multipoint(as.matrix(rbind(corner_coord[,c("x_proj","y_proj")], corner_coord[1,c("x_proj","y_proj")])))
  corner_polygon <- st_polygon(x = list(corner_polygon), dim = "XY")
  corner_polygon <- st_sfc(list(corner_polygon))
  
  # Calculate projected tree coordinates from relative tree coordinates --------
  if(!is.null(tree_df)) {
    if(any(! tree_df[,tree_coords[1]] %between% range(corner_coord$x_rel)) | any(! tree_df[,tree_coords[2]] %between% range(corner_coord$y_rel))) {
      warning("Be careful, one or more trees are not inside the plot defined by rel_coord")
    }
    if(trust_GPS_corners) {
      tree_proj_coord <- bilinear_interpolation(coord = tree_df[,tree_coords],
                                                from_corner_coord = corner_coord[,c("x_rel","y_rel")],
                                                to_corner_coord = corner_coord[,c("x_proj","y_proj")], 
                                                ordered_corner = T)
      colnames(tree_proj_coord) <- c("x_proj","y_proj")
    } else {
      tree_proj_coord <- as.matrix(tree_df[,tree_coords]) %*% procrust_res$rotation
      tree_proj_coord <- data.frame(sweep(tree_proj_coord, 2, procrust_res$translation, FUN = "+"))
      colnames(tree_proj_coord) <- c("x_proj","y_proj")
    }
  }
  
  # draw plot ------------------------------------------------------------------
  
  if (draw_plot) {
    proj_coord_plot <- proj_coord[,c(1,2)]
    setnames(proj_coord_plot , c("x_proj","y_proj"))
    proj_coord_plot$whatpoint <- "GPS measurements"
    corner_coord_plot <- corner_coord[,c("x_proj","y_proj")]
    corner_coord_plot$whatpoint <- "Reference corners"
    proj_coord_plot <- rbind(proj_coord_plot,corner_coord_plot)
    if(!is.null(tree_df)) {
      tree_proj_coord$whatpoint <- "Trees"
      proj_coord_plot <- rbind(proj_coord_plot,tree_proj_coord)
    }
    proj_coord_plot$whatpoint <- factor(proj_coord_plot$whatpoint, levels = c("GPS measurements","Outliers (discarded)","Reference corners","Trees"))
    arrow_plot <- data.frame(X = rep(corner_coord$x_proj[1]), Y =rep(corner_coord$y_proj[1]),
                             Xend = c(corner_coord$x_proj[1]+(corner_coord$x_proj[2]-corner_coord$x_proj[1])/4,corner_coord$x_proj[1]+(corner_coord$x_proj[4]-corner_coord$x_proj[1])/4),
                             Yend = c(corner_coord$y_proj[1]+(corner_coord$y_proj[2]-corner_coord$y_proj[1])/4,corner_coord$y_proj[1]+(corner_coord$y_proj[4]-corner_coord$y_proj[1])/4))
    
    if(exists("outliers") && nrow(outliers)!=0) {
      proj_coord_plot$whatpoint[outliers$row_number] <- "Outliers (discarded)"
    }
    plot_design <- ggplot2::ggplot(data = proj_coord_plot) +
      geom_point(aes(x=x_proj,y=y_proj,col=whatpoint,shape = whatpoint),size=2) + 
      scale_shape_manual(values=c(2,4,15,1), drop=FALSE) +
      scale_color_manual(values=c('black','red',"black","darkgreen"), drop = FALSE) +
      geom_polygon(data = corner_polygon[[1]][[1]][,] , mapping = aes(x=x_proj,y=y_proj), colour="black",fill=NA,linewidth=1.2)+
      geom_segment(data=arrow_plot, mapping=aes(x=X,y=Y,xend=Xend,yend=Yend), arrow=arrow(length=unit(0.4,"cm")),linewidth=1, col="blue") +
      geom_text(data = arrow_plot, mapping = aes(x=Xend,y=Yend,label=c("x_rel","y_rel")), hjust=c(0,-0.3), vjust=c(-0.5,0), col="blue" ) +
      ggtitle("Plot display") +
      theme_minimal() + 
      theme(legend.title=element_blank())+
      coord_equal()
    print(plot_design)
  }
  
  # return ---------------------------------------------------------------------
  
  if (nrow(outliers) != 0 & !rm_outliers) {
    warning(
      "Be carefull, you may have GNSS measurement outliers. \n",
      "Removing them may improve the georeferencing of your plot (see  the rm_outliers argument)."
    )
  }
  
  output <- list(
    corner_coord = corner_coord,
    polygon = corner_polygon, outliers = outliers
  )
  if (draw_plot) {
    output$plot_design <- plot_design
  }
  if (!is.null(longlat)) {
    output$UTM_code <- UTM_code
  }
  if (!is.null(tree_df)) {
    output$tree_proj_coord <- tree_proj_coord[,c("x_proj","y_proj")]
  }
  return(output)
}