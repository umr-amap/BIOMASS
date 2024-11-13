#' Check coordinates of plot corners and trees
#'
#' @description
#' Quality check of plot corner and tree coordinates.
#'
#' @details
#' If trustGPScorners is TRUE, corner coordinates in the projected coordinate system are averaging by corner (if multiple measures) and outlier corners are identified sequentially using these averages and the maxDist argument. Then, projected coordinates of the trees are calculated from the local coordinates using a bilinear interpolation that follows the correspondence of the corners between these two coordinate systems. Be aware that this projection only works if the plot, in the relative coordinates system, is rectangular (ie, has 4 right angles).
#' 
#' If trustGPScorners is FALSE, corner coordinates in the projected coordinate system are calculated by a procrust analysis that preserves the shape and dimensions of the plot in the local coordinate system. Outlier corners are also identified sequentially and projected coordinates of the trees are calculated by applying the resulting procrust analysis.
#' 
#' If longlat is supplied instead of projCoord, the function will first convert the long/lat coordinates into UTM coordinates. An error may result if the parcel is located right between two UTM zones. In this case, the user has to convert himself his long/lat coordinates into any projected coordinates which have the same dimension than his local coordinates (in meters most of the time).
#'
#' @param projCoord (optional, if longlat is not supplied) a data frame containing the plot corner coordinates in a projected coordinate system, with X and Y corresponding to the first and second columns respectively.
#' @param longlat (optional, if projCoord is not supplied) a data frame containing the plot corner coordinates in longitude and latitude, with longitude and latitude corresponding to the first and second columns respectively
#' @param relCoord a data frame containing the plot corner coordinates in the relative coordinates system (that of the field), with X and Y corresponding to the first and second columns respectively, and with the same row order than longlat or projCoord
#' @param trustGPScorners a logical indicating whether or not you trust the GPS coordinates of the plot's corners. See details.
#' @param cornerID a vector indicating the ID of the corners (e.g c("SE","SE",...)) in the case you have multiple measurements for each corner
#' @param maxDist a numeric giving the maximum distance (in meters) above which GPS measurements should be considered outliers (default 15 m)
#' @param rmOutliers a logical indicating if detected outliers are removed from the coordinate calculation
#' @param drawPlot a logical indicating if the plot design should be displayed and returned
#' @param treeCoord (optional) a data frame containing at least the relative tree coordinates (field/local coordinates), with X and Y corresponding to the first and second columns respectively
#'
#' @author Arthur PERE, Maxime REJOU-MECHAIN, Arthur BAILLY
#'
#' @return Returns a list including :
#'    - `cornerCoord`: a data frame containing the projected coordinates (x_proj and y_proj), the relative coordinates (x_rel and y_rel), and the ID (if cornerID is supplied) of the 4 corners of the plot 
#'    - `polygon`: a spatial polygon
#'    - `outliers`: a data frame containing the projected coordinates, the ID (if cornerID is supplied) and the row number of GPS points considered outliers 
#'    - `plotDesign`: if `drawPlot` is TRUE, a ggplot object corresponding to the design of the plot
#'    - `codeUTM`: if `longlat` is supplied, a character containing the UTM code of the GPS coordinates
#'    - `treeProjCoord`: if `treeCoord` is supplied, a data frame containing the coordinates of the trees in the projected coordinate system (x_proj and y_proj)
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
#' projCoord <- data.frame(
#'   X = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 8, max = 12),
#'     runif(5, min = 80, max = 120), runif(5, min = 90, max = 110)
#'   ),
#'   Y = c(
#'     runif(5, min = 9, max = 11), runif(5, min = 80, max = 120),
#'     runif(5, min = 8, max = 12), runif(5, min = 90, max = 110)
#'   )
#' )
#' projCoord <- projCoord + 1000
#' relCoord <- data.frame(
#'   X = c(rep(0, 10), rep(100, 10)),
#'   Y = c(rep(c(rep(0, 5), rep(100, 5)), 2))
#' )
#' cornerID <- rep(c("SW","NW","SE","NE"),e=5)
#'
#' aa <- checkPlotCoord(
#'   projCoord = projCoord, relCoord = relCoord,
#'   trustGPScorners = TRUE, cornerID = cornerID,
#'   rmOutliers = FALSE , drawPlot = FALSE
#' )
#' bb <- checkPlotCoord(
#'   projCoord = projCoord, relCoord = relCoord,
#'   trustGPScorners = FALSE, 
#'   rmOutliers = TRUE, maxDist = 10,
#'   drawPlot = FALSE
#' )
#' \donttest{
#' checkPlotCoord(
#'   projCoord = projCoord, relCoord = relCoord,
#'   trustGPScorners = TRUE, cornerID = cornerID,
#'   rmOutliers = TRUE , maxDist = 10,
#'   drawPlot = TRUE
#' )
#' }
#'
checkPlotCoord <- function(projCoord = NULL, longlat = NULL, relCoord, trustGPScorners, cornerID=NULL, maxDist = 15, rmOutliers = TRUE,  drawPlot = TRUE, treeCoord = NULL) {
  
  # Checking arguments -------------------------------------------------
  
  if (is.null(longlat) && is.null(projCoord)) {
    stop("Give at least one set of coordinates: longlat or projCoord")
  }
  if (!is.null(longlat) & !is.null(projCoord)) {
    stop("Give only one set of coordinates: longlat or projCoord")
  }
  if((!is.matrix(relCoord) & !is.data.frame(relCoord))){
    stop("relCoord must be a matrix or a data frame")
  }
  if(!is.null(longlat) && (!is.matrix(longlat) & !is.data.frame(longlat))){
    stop("longlat must be a matrix or a data frame")
  }
  if(!is.null(projCoord) && (!is.matrix(projCoord) & !is.data.frame(projCoord))){
    stop("projCoord must be a matrix or a data frame")
  }
  if(!is.null(treeCoord) && (!is.matrix(treeCoord) & !is.data.frame(treeCoord))){
    stop("treeCoord must be a matrix or a data frame")
  }
  if (is.null(trustGPScorners) | !is.logical(trustGPScorners)) {
    stop("The trustGPScorners argument must be TRUE or FALSE")
  }
  if (length(maxDist) != 1) {
    stop("The maxDist argument must be of length 1")
  }
  if ((!is.null(longlat) && any(dim(longlat) != dim(relCoord))) || (!is.null(projCoord) && any(dim(projCoord) != dim(relCoord)))) {
    stop("GPS and relative coordinates are not of the same dimension")
  }
  if(sum(is.na(relCoord))!= 0) {
    stop("Missing values are detected in relCoord. Please remove them and call the function again")
  }
  if(!is.null(longlat) && sum(is.na(longlat))!= 0) {
    stop("Missing values are detected in longlat. Please remove them and call the function again")
  }
  if(!is.null(projCoord) && sum(is.na(projCoord))!= 0) {
    stop("Missing values are detected in projCoord. Please remove them and call the function again")
  }
  if (trustGPScorners==T && (!is.null(projCoord)) && nrow(projCoord)!=4 & is.null(cornerID)) {
    stop("The argument cornerID is needed if trustGPScorners is TRUE and if multiple measurements of each corner have been realized")
  }
  if (nrow(unique(relCoord)) != 4) {
    stop( paste(nrow(unique(relCoord)),"unique corners are detected in relCoord instead of 4"))
  }
  if (!is.null(cornerID) && length(cornerID) != nrow(relCoord)) {
    stop("cornerID must be the same length as the number of rows in relCoord")
  }
  if (!is.null(cornerID) && length(unique(cornerID)) !=4) {
    stop(paste(length((unique(cornerID))),"unique corners are detected in cornerID instead of 4"))
  }
  
  
  # function -------------------------------------------------------------------
  
  # Transform the geographic coordinates into UTM coordinates
  if (!is.null(longlat)) {
    projCoord <- latlong2UTM(longlat)
    codeUTM <- unique(projCoord[, "codeUTM"])
    if(length(codeUTM)>1) {
      stop("More than one UTM zone are detected. This may be due to an error in the long/lat coordinates, or if the parcel is located right between two UTM zones. In this case, please convert yourself your long/lat coordinates into any projected coordinates which have the same dimension than your local coordinates")
    }
    projCoord <- projCoord[, c("X", "Y")]
  }
  
  if(trustGPScorners == TRUE) {
  
    if(nrow(projCoord)!= 4) { # if multiple measures of each corner, then do the mean of coordinates and search for outliers
      
      cornerCoord <- data.table(cbind(projCoord[,1:2], relCoord[,1:2],cornerID=cornerID))
      setnames(cornerCoord, c("x_proj","y_proj","x_rel","y_rel","cornerID"))
      if (any(table(cornerCoord$cornerID) < 5)) {
        warning("At least one corner has less than 5 measurements. We suggest using the argument trustGPScorners = FALSE")
      }
      cornerCoord[ , c("Xmean","Ymean","nRow") := list(mean(x_proj),mean(y_proj),.I) , by=cornerID] 
      cornerCoord[ , outlier := ifelse( sqrt((x_proj - Xmean)^2 + (y_proj-Ymean)^2) > maxDist, T, F)]
      outliers <- cornerCoord[outlier==T , c("x_proj","y_proj","cornerID","nRow")]
      
      if (any(cornerCoord[,sum(!outlier),by=cornerID][[2]]==0)) {
        stop("All measurements for at least one corner are considered as outliers.\n
       This may be because some coordinates have very large error associated.\n
       Try to remove these very large error or reconsider the maxDist parameter by increasing the distance")
      }
      
      if(rmOutliers & nrow(outliers)!=0) {
        cornerCoord <- cornerCoord[outlier==F ,]
        existOutliers <- T
        while(existOutliers) {
          cornerCoord[ , outlier := ifelse( sqrt((x_proj - Xmean)^2 + (y_proj-Ymean)^2) > maxDist, T, F)]
          outliers <- rbind(outliers , cornerCoord[outlier==T , c("x_proj","y_proj","cornerID","nRow")])
          cornerCoord <- cornerCoord[outlier==F ,]
          if(nrow(cornerCoord)+nrow(outliers) == nrow(projCoord)) existOutliers <- F
          cornerCoord[ , c("Xmean","Ymean") := list(mean(x_proj),mean(y_proj)) , by=cornerID]
        }
      }
      
      cornerCoord <- data.frame(cornerCoord[ , c("Xmean","Ymean","x_rel","y_rel","cornerID")])
      cornerCoord <- unique(cornerCoord)
      colnames(cornerCoord) <- c("x_proj", "y_proj","x_rel","y_rel","cornerID")
      
    } else { # if exactly 1 measures for each corner
      cornerCoord <- data.frame(cbind(projCoord[,1:2], relCoord[,1:2]))
      colnames(cornerCoord) <- c("x_proj","y_proj","x_rel","y_rel")
      outliers <- data.frame()
      if(!is.null(cornerID)) cornerCoord$cornerID = cornerID
    }
    
  } else { # trustGPScorners = "FALSE"
    
    cornerCoord <- data.table(cbind(projCoord[,1:2], relCoord[,1:2]))
    setnames(cornerCoord, c("x_proj","y_proj","x_rel","y_rel"))
    cornerCoord[, nRow := .I]
    
    # Transformation of relCoord to projected coordinates by a procrust analyses
    procrustRes <- procrust(projCoord, relCoord)
    procrustCoord <- as.matrix(relCoord) %*% procrustRes$rotation
    procrustCoord <- sweep(procrustCoord, 2, procrustRes$translation, FUN = "+")
    
    # Calculate the distances between the GNSS measurements and the procrustCoord
    cornerCoord[ , outlier := ifelse(
      sqrt((procrustCoord[, 1] - projCoord[, 1])^2 + (procrustCoord[, 2] - projCoord[, 2])^2) > maxDist, T, F)]
    
    outliers <- cornerCoord[outlier==T , c("x_proj","y_proj","nRow")]
    
    if (length(outliers)==nrow(projCoord)){
      stop("All coordinates points are considered as outliers at the first stage.\n
         This may be because some coordinates have very large error associated.\n
         Try to remove these very large error or reconsider the maxDist parameter by increasing the distance")
    }
    
    # retransform the relCoord without the outliers
    if (rmOutliers & nrow(outliers)>0) {
      cornerCoord <- cornerCoord[outlier==F ,]
      refineCoord <- TRUE
      while(refineCoord){
        procrustRes <- procrust(projCoord[-outliers$nRow, ], relCoord[-outliers$nRow,])
        procrustCoord <- as.matrix(relCoord[-outliers$nRow,]) %*% procrustRes$rotation
        procrustCoord <- sweep(procrustCoord, 2, procrustRes$translation, FUN = "+")
        cornerCoord[ , outlier := ifelse(
          sqrt((procrustCoord[, 1] - projCoord[-outliers$nRow, 1])^2 + 
                 (procrustCoord[, 2] - projCoord[-outliers$nRow, 2])^2) > maxDist, T, F)]
        
        outliers <- rbind(outliers , cornerCoord[outlier==T , c("x_proj","y_proj","nRow")])
        cornerCoord <- cornerCoord[outlier==F ,]
        if(nrow(cornerCoord)+nrow(outliers) == nrow(projCoord)) refineCoord <- F
      }
    }
    
    # Create the data.table of corners to return the projected coordinate of the plot's corner 
    cornerRelCoord <- unique(relCoord)
    
    # Project cornerRelCoord
    cornerProjCoord <- as.matrix(cornerRelCoord[,1:2]) %*% procrustRes$rotation
    cornerProjCoord <- sweep(cornerProjCoord, 2, procrustRes$translation, FUN = "+")
    
    cornerCoord <- data.frame(cbind(cornerProjCoord[,1:2], cornerRelCoord[,1:2]))
    colnames(cornerCoord) <- c("x_proj","y_proj","x_rel","y_rel")

    if(!is.null(cornerID)) cornerCoord$cornerID <- unique(cornerID)
  } # End trustGPScorners = "FALSE"
  
  # Sort cornerCoord rows in a counter-clockwise direction ---------------------
  centroid <- colMeans(cornerCoord[,c("x_rel","y_rel")])
  angles <- base::atan2(cornerCoord[["y_rel"]] - centroid[2], cornerCoord[["x_rel"]] - centroid[1])
  cornerCoord <- cornerCoord[order(angles), ]
  
  # Create a polygon -----------------------------------------------------------
  cornerPolygon <- st_multipoint(as.matrix(rbind(cornerCoord[,c("x_proj","y_proj")], cornerCoord[1,c("x_proj","y_proj")])))
  cornerPolygon <- st_polygon(x = list(cornerPolygon), dim = "XY")
  cornerPolygon <- st_sfc(list(cornerPolygon))
  
  # Calculate projected tree coordinates from relative tree coordinates --------
  if(!is.null(treeCoord)) {
    if(any(! treeCoord[,1] %between% range(cornerCoord$x_rel)) | any(! treeCoord[,2] %between% range(cornerCoord$y_rel))) {
      warning("Be careful, one or more trees are not inside the plot defined by relCoord")
    }
    if(trustGPScorners) {
      treeProjCoord <- bilinear_interpolation(coord = treeCoord[,1:2],
                                              from_corner_coord = cornerCoord[,c("x_rel","y_rel")],
                                              to_corner_coord = cornerCoord[,c("x_proj","y_proj")], 
                                              ordered_corner = T)
      colnames(treeProjCoord) <- c("x_proj","y_proj")
    } else {
      treeProjCoord <- as.matrix(treeCoord[,1:2]) %*% procrustRes$rotation
      treeProjCoord <- data.frame(sweep(treeProjCoord, 2, procrustRes$translation, FUN = "+"))
      colnames(treeProjCoord) <- c("x_proj","y_proj")
    }
  }
  
  # draw plot ------------------------------------------------------------------
  
  if (drawPlot) {
    projCoordPlot <- projCoord[,c(1,2)]
    setnames(projCoordPlot , c("x_proj","y_proj"))
    projCoordPlot$whatpoint <- "GPS measurements"
    cornerCoordPlot <- cornerCoord[,c("x_proj","y_proj")]
    cornerCoordPlot$whatpoint <- "Reference corners"
    projCoordPlot <- rbind(projCoordPlot,cornerCoordPlot)
    if(!is.null(treeCoord)) {
      treeProjCoord$whatpoint <- "Trees"
      projCoordPlot <- rbind(projCoordPlot,treeProjCoord)
    }
    projCoordPlot$whatpoint <- factor(projCoordPlot$whatpoint, levels = c("GPS measurements","Outliers (discarded)","Reference corners","Trees"))
    arrowPlot <- data.frame(X = rep(cornerCoord$x_proj[1]), Y =rep(cornerCoord$y_proj[1]),
                            Xend = c(cornerCoord$x_proj[1]+(cornerCoord$x_proj[4]-cornerCoord$x_proj[1])/4,cornerCoord$x_proj[1]+(cornerCoord$x_proj[2]-cornerCoord$x_proj[1])/4),
                            Yend = c(cornerCoord$y_proj[1]+(cornerCoord$y_proj[4]-cornerCoord$y_proj[1])/4,cornerCoord$y_proj[1]+(cornerCoord$y_proj[2]-cornerCoord$y_proj[1])/4))
    
    if(exists("outliers") && nrow(outliers)!=0) {
      projCoordPlot$whatpoint[outliers$nRow] <- "Outliers (discarded)"
    }
    plotDesign <- ggplot2::ggplot(data = projCoordPlot) +
      geom_point(aes(x=x_proj,y=y_proj,col=whatpoint,shape = whatpoint),size=2) + 
      scale_shape_manual(values=c(2,4,15,1), drop=FALSE) +
      scale_color_manual(values=c('black','red',"black","darkgreen"), drop = FALSE) +
      geom_polygon(data = cornerPolygon[[1]][[1]][,] , mapping = aes(x=x_proj,y=y_proj), colour="black",fill=NA,linewidth=1.2)+
      geom_segment(data=arrowPlot, mapping=aes(x=X,y=Y,xend=Xend,yend=Yend), arrow=arrow(length=unit(0.4,"cm")),linewidth=1, col="blue") +
      geom_text(data = arrowPlot, mapping = aes(x=Xend,y=Yend,label=c("x_rel","y_rel")), hjust=c(0,-0.3), vjust=c(-0.5,0), col="blue" ) +
      ggtitle("Plot display") +
      theme_minimal() + 
      theme(legend.title=element_blank())+
      coord_equal()
    print(plotDesign)
  }
  
  # return ---------------------------------------------------------------------
  
  if (nrow(outliers) != 0 & !rmOutliers) {
    warning(
      "Be carefull, you may have GNSS measurement outliers. \n",
      "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument)."
    )
  }
  
  output <- list(
    cornerCoord = cornerCoord,
    polygon = cornerPolygon, outliers = outliers
  )
  if (drawPlot) {
    output$plotDesign <- plotDesign
  }
  if (!is.null(longlat)) {
    output$codeUTM <- codeUTM
  }
  if (!is.null(treeCoord)) {
    output$treeProjCoord <- treeProjCoord[,c("x_proj","y_proj")]
  }
  return(output)
}
