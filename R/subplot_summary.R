#' Summarise and display tree information by subplot
#'
#' @description
#' After applying the [divide_plot()] function, this function summarises with any defined function the desired tree metric by sub-plot and displays the plot representation.
#'
#' @param subplots output of the [divide_plot()] function
#' @param value (if AGB_simu is not provided) a character indicating the column in subplots$tree_data to be summarised (or character vector to summarise several metrics at once)
#' @param AGB_simu (if value is not provided) a n x m matrix containing individual AGB where n is the number of tree and m is the number of monte carlo simulation. Typically, the output '$AGB_simu' of the AGBmonteCarlo() function.
#' @param draw_plot a logical indicating whether the plot design should be displayed
#' @param per_ha a logical indicating whether the metric summary should be per hectare (or, if summarising several metrics at once: a logical vector corresponding to each metric (see examples))
#' @param fun the function to be applied on tree metric of each subplot (or, if summarising several metrics at once: a list of functions named according to each metric (see examples))
#' @param ref_raster A SpatRaster object from terra package, typically a chm raster created from LiDAR data. Note that in the case of a multiple attributes raster, only the first variable "z" will be summarised.
#' @param raster_fun the function (or a list of functions) to be applied on raster values of each subplot.
#' @param ... optional arguments to fun
#'
#' @return If 'value' is provided, a list containing the following elements:
#'  - `tree_summary` : a summary of the metric per subplot 
#'  - `polygon` : an sf object : simple feature collection of the subplot's polygon
#'  - `plot_design` : a ggplot object (or a list of ggplot objects) that can easily be modified
#'  
#'  If 'AGB_simu' is provided, a data.table containing a summary of the metric(s) per subplot and per simulation.
#'
#' @export
#' 
#' @author Arthur Bailly
#'
#' @importFrom data.table data.table := set
#' @importFrom stats reshape
#' @importFrom grDevices terrain.colors
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal scale_fill_gradientn theme ggtitle lims
#' @importFrom terra extract
#' 
#' @examples
#' # One plot with repeated measurements of each corner
#' data("NouraguesPlot201")
#' data("NouraguesTrees")
#' check_plot201 <- check_plot_coord(
#'   corner_data = NouraguesPlot201,
#'   proj_coord = c("Xutm","Yutm"), rel_coord = c("Xfield","Yfield"),
#'   trust_GPS_corners = TRUE, draw_plot = FALSE)
#' subplots_201 <- suppressWarnings(
#'   divide_plot(
#'     corner_data = check_plot201$corner_coord, 
#'     rel_coord = c("x_rel","y_rel"), proj_coord = c("x_proj","y_proj"),
#'     grid_size = 50,
#'     tree_data =  NouraguesTrees[NouraguesTrees$Plot == 201,],
#'     tree_coords = c("Xfield","Yfield")))
#' # Sum summary (by default) of diameter
#' subplots_201_sum <- subplot_summary(subplots_201 , value = "D", draw_plot = FALSE)
#' subplots_201_sum$tree_summary
#' subplots_201_sum$polygon
#' \donttest{
#'   subplots_201_sum$plot_design
#' }
#' # 9th quantile summary (for example) of diameter
#' subplots_201_quant <- subplot_summary(subplots_201 , value = "D", draw_plot = FALSE,
#'                                       fun = quantile, probs=0.9)
#'   
#' 
#' # Dealing with multiple plots
#' \dontrun{
#'   data("NouraguesCoords")
#'   nouragues_subplots <- suppressWarnings(
#'   divide_plot(
#'      corner_data = NouraguesCoords,
#'     rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"),
#'     corner_plot_ID = "Plot",
#'     grid_size = 50,
#'     tree_data = NouraguesTrees, tree_coords =  c("Xfield","Yfield"),
#'     tree_plot_ID = "Plot"))
#'   # Sum summary (by default)
#'   nouragues_sum <- subplot_summary(nouragues_subplots , value = "D", draw_plot = FALSE)
#'   nouragues_sum$tree_summary
#'   nouragues_sum$plot_design
#' }
#'
# Dealing with multiple plots and metrics
#' \dontrun{
#'   data("NouraguesCoords")
#'   nouragues_subplots <- suppressWarnings(
#'   divide_plot(
#'      corner_data = NouraguesCoords,
#'     rel_coord = c("Xfield","Yfield"), proj_coord = c("Xutm","Yutm"),
#'     corner_plot_ID = "Plot",
#'     grid_size = 50,
#'     tree_data = NouraguesTrees, tree_coords =  c("Xfield","Yfield"),
#'     tree_plot_ID = "Plot"))
#'   # Sum summary (by default)
#'   nouragues_mult <- subplot_summary(nouragues_subplots , 
#'                                    value = c("D","D","x_rel"),
#'                                    fun = list(D=sum,D=mean,x_rel=mean),
#'                                    per_ha = c(T,F,F),
#'                                    draw_plot = FALSE)
#'   nouragues_mult$tree_summary
#'   nouragues_mult$plot_design$`201`[[1]]
#'   nouragues_mult$plot_design$`201`[[2]]
#'   nouragues_mult$plot_design$`201`[[3]]
#' }
#'
subplot_summary <- function(subplots, value = NULL, AGB_simu = NULL, draw_plot = TRUE, per_ha = TRUE, fun = sum, ref_raster = NULL, raster_fun = mean, ...) {
  
  # Checking parameters --------------------------------------------------------
  if(is.null(subplots$tree_data)) {
    stop("subplots argument does'nt contain any tree data frame. Use the divide_plot function with a non-null tree_data argument")
  }
  if ((is.null(value) & is.null(AGB_simu)) | (!is.null(value) & !is.null(AGB_simu)) )  {
    stop("You must provide either 'value' or 'AGB_simu'")
  }
  if(is.data.frame(subplots$sub_corner_coord) && any(!c("x_proj","y_proj") %in% names(subplots$sub_corner_coord))) {
    subplots$sub_corner_coord[,c("x_proj","y_proj")] <- subplots$sub_corner_coord[,c("x_rel","y_rel")]
    message("Projected coordinates are not found in sub_corner_coord$subplots, tree metric will be summarised in the relative coordinate system")
  }
  if(!is.null(value) && !any(value %in% names(subplots$tree_data))) {
    stop(paste(value,"is not a column name of subplots$tree_data"))
  }
  if (!is.null(AGB_simu) && !is.matrix(AGB_simu)) stop("'AGB_simu' must be a matrix containing individual AGB (one row per tree), typically, the output '$AGB_simu' of the AGBmonteCarlo() function.")
  
  if (!is.null(AGB_simu) && nrow(AGB_simu) != nrow(subplots$tree_data) ) {
    stop("The rows in 'subplots$tree_data' must match the rows in 'AGB_simu'")
  }
  if (!is.null(AGB_simu)) {
    if(is.data.frame(subplots$sub_corner_coord)) {
      stop("Propagating AGB uncertainties without propagating GPS measurement uncertainties of corners is not advised. If you want to do, provide 'sd_coord = 0' in divide_plot().")
    } else if (ncol(AGB_simu) != length(subplots$sub_corner_coord)) {
      if( ncol(AGB_simu) < length(subplots$sub_corner_coord) ) {
        message(paste("As the 'AGB_simu' matrix only contains", ncol(AGB_simu),"simulations, the first", ncol(AGB_simu),"simulations contained in 'subplots' will be considered."))
        subplots$sub_corner_coord <- subplots$sub_corner_coord[[1:ncol(AGB_simu)]]
      } else {
        message(paste("As 'AGB_simu' contains", ncol(AGB_simu),"simulations, and 'subplots' contains", length(subplots$sub_corner_coord),"simulations,", ncol(AGB_simu)-length(subplots$sub_corner_coord),"simulations will be resampled in 'subplots'."))
        subplots$sub_corner_coord <- c(subplots$sub_corner_coord ,
                                       lapply(1:(ncol(AGB_simu)-length(subplots$sub_corner_coord)), function(x) {
                                         subplots$sub_corner_coord[[sample(x = 1:length(subplots$sub_corner_coord), size = 1, replace = TRUE)]]
                                       }) )
      }
    }
  }
  
  # Get the name of the function(s) (before the evaluation of fun, otherwise we will get the function and not the name of the argument)
  if( length(value) > 1) {
    fun_name <- as.character(substitute(fun))[-1]
  } else {
    fun_name <- as.character(substitute(fun))
  }
  if( length(raster_fun) > 1) {
    raster_fun_name <- as.character(substitute(raster_fun))[-1]
  } else {
    raster_fun_name <- as.character(substitute(raster_fun))
  }
  
  # Check if fun is a function or a list of functions
  if( !is.list(fun) ) {
    if( !is.function(fun) ) {
      stop("the function provided using `fun =` is not a function")
    } else {
      fun <- match.fun(fun)
    }
  }
  if( is.list(fun) ) {
    if( any(!sapply(fun , is.function)) ) {
      stop(paste("incorrect", fun_name[!sapply(fun , is.function)], "function(s) provided in `fun` (not a function)"))
    } else {
      fun <- lapply(fun , match.fun)
    }
  }
  if(!is.null(value)) {
    if( length(value) > 1 && length(per_ha) == 1 ) {
      per_ha = rep(per_ha, length(value))
    }
    if( is.list(fun) && length(value) != length(fun) ) {
      stop("the lengths of 'value' and 'fun' are not the same")
    }
    if( is.list(fun) && length(value) != length(per_ha) ) {
      stop("the lengths of 'value' and 'per_ha' are not the same")
    }
  }
  
  # Check is ref_raster is a SpatRaster
  if(!is.null(ref_raster) && !is(ref_raster, "SpatRaster") ) {
    stop("ref_raster is not recognised as a SpatRaster of terra package")
  }
  # Check if raster_fun is a function or a list of function
  if( !is.list(raster_fun) ) {
    if( !is.function(raster_fun) ) {
      stop("the function provided using `raster_fun =` is not a function")
    } else {
      raster_fun <- match.fun(raster_fun)
    }
  }
  if( is.list(raster_fun) ) {
    if( any(!sapply(raster_fun , is.function)) ) {
      stop(paste("incorrect", raster_fun_name[!sapply(raster_fun , is.function)], "function(s) provided in raster_fun (not a function)"))
    } else {
      raster_fun <- lapply(raster_fun , match.fun)
    }
  }
  
  # Data processing ------------------------------------------------------------
  value_fun_name <- paste(value, fun_name, sep = "_")
  
  if(is.data.frame(subplots$sub_corner_coord)) {
    corner_dat <- data.table(subplots$sub_corner_coord)
  } else { # if n simulations of corner coordinates (GPS measurement uncertainties)
    if(is.null(AGB_simu)) {
      stop("Propagating GPS measurement uncertainties of corners without propagating AGB uncertainties makes no sense. Use the argument 'AGB_simu' instead of 'value' to provide AGB uncertainties.")
    } else {
      corner_dat <- do.call(rbind,lapply(1:length(subplots$sub_corner_coord), function(n_simu) { # n_simu = 1
        cor_dat <- data.table(subplots$sub_corner_coord[[n_simu]])
        cor_dat[, subplot_ID := paste0(subplot_ID , "_N_" , n_simu)]
        # Adding subplot centre coordinates
        cor_dat[ , c("x_center","y_center") := list(mean(x_proj),mean(y_proj)) , by = subplot_ID]
      }))
    }
  }
  
  tree_summary <- data.table(subplots$tree_data)
  
  # Adding AGB simulated values to tree_summary
  if(!is.null(AGB_simu)) {
    tree_summary <- cbind(tree_summary, AGB_simu)
    value <- paste0("V",1:ncol(AGB_simu))
    value_fun_name <- paste0(value,"_sum")
    fun = lapply(1:ncol(AGB_simu), function(x) sum)
  }
  
  # Adding metric(s) summary to tree_summary at subplot level
  if(!is.null(value)) {
    if(length(value) == 1) {
      tree_summary <- tree_summary[!is.na(subplot_ID), fun(get(value), ...) , by=c("subplot_ID")]
      setnames(tree_summary, "V1", value_fun_name)
    } else { # Apply functions to values by subplot
      tree_summary <- tree_summary[!is.na(subplot_ID), lapply( 1:length(value), function(i) fun[[i]](get(value[i]), ...) ) , by=c("subplot_ID")]
      setnames(tree_summary, 2:(length(value)+1), value_fun_name)
    }
  }
  if(!is.null(AGB_simu)) { # Reshape to a long format tree_summary to include the simulation number in subplot_ID
    tree_summary <- reshape(tree_summary, varying = value_fun_name , direction = "long", v.names = "AGB_sum", ids = NULL)
    tree_summary[ , subplot_ID := paste0(subplot_ID , "_N_" , time)][ , time := NULL]
    value <- "AGB"
    value_fun_name <- "AGB_sum"
  }
  
  # Check function(s) that would returned more than one value
  if(any(duplicated(tree_summary$subplot_ID))) {
    stop("the function provided using `fun` must return a single value")
  }
  
  # Merging corner_dat and tree_summary: the trick is to get a full join of the data.tables (in the case of no tree in a subplot)
  setkey(corner_dat,subplot_ID)
  setkey(tree_summary,subplot_ID)
  unique_keys <- unique(c(corner_dat[,subplot_ID], tree_summary[,subplot_ID]))
  corner_dat <- corner_dat[tree_summary[.(unique_keys), on="subplot_ID"]] # Full Outer Join
  for (j in value_fun_name) data.table::set(corner_dat,which(is.na(corner_dat[[j]])),j,0) # replace NA by 0 (when no tree in a subplot)
  
  # Function to create sf_polygons that will be the $polygon output of the function (a simple feature collection of the subplot's polygon)
  print("Creating sf_polygons for each subplot...")
  create_polygon_fct <- function(dat) { # dat = corner_dat[ 1:4 , ]
    
    # Create the associated polygon
    mat <- dat[, .(x_proj, y_proj)]
    mat <- as.matrix(rbind(mat, mat[1, ]))
    subplot_polygon <- sf::st_polygon(list(mat))
    
    # Get 1 summary row instead of 4 (because there was 4 corners) 
    col_names <- c("plot_ID","subplot_ID",value_fun_name)
    df_polygon <- unique(dat[,..col_names])
    
    # Loop on per_ha vector to divide the metric summary by the polygon's area if necessary
    for(i in 1:length(per_ha)) {
      if (per_ha[i]) {
        df_polygon[ , paste(value[i], fun_name[i], "per_ha", sep = "_") :=  get(value_fun_name[i])/ sf::st_area(subplot_polygon) * 10000]
        df_polygon[ , eval(value_fun_name[i]) :=  NULL]
      }
    }
    
    # Replace projected coordinates with geographic coordinates if exist
    if( !is.null(subplots$UTM_code) ) {
      mat <- dat[, .(long, lat)]
      mat <- as.matrix(rbind(mat, mat[1, ]))
      subplot_polygon <- sf::st_polygon(list(mat))
    }
    
    # Convert subplot_polygon from a POLYGON to a simple feature collection of 1 POLYGON
    sf_polygon <- sf::st_sf(df_polygon,list(subplot_polygon))
    
    # Setting CRS if geographic coordinates
    if( !is.null(subplots$UTM_code) ) {
      sf::st_crs(sf_polygon) <- subplots$UTM_code$UTM_code[subplots$UTM_code$plot_ID == unique(dat$plot_ID)]
    }
    
    sf::st_geometry(sf_polygon) <- "sf_subplot_polygon" #rename last column
    return(sf_polygon)
  }
  
  
  sf_polygons <- corner_dat[ , create_polygon_fct(.SD) , by = "subplot_ID", .SDcols = colnames(corner_dat)][,-1]
  sf_polygons <- sf_polygons[order(sf_polygons$subplot_ID),]
  
  ### Extract raster values of sf_polygons (use of extract just once instead of multiple time if it was done during the sf_polygons creation)
  if( !is.null(ref_raster)) {  
    print("Extracting raster metric for each subplot...")
    extract_rast_val <- extract(x = ref_raster, y = vect(sf_polygons$sf_subplot_polygon), exact = TRUE)
    print("Extracting raster metric done.")
    # keeping raster values whose fraction are > 0.5
    extract_rast_val = data.table(extract_rast_val)[ fraction >0.5,]
    rast_val_name <- names(extract_rast_val)[2]
    raster_value_fun_name <- paste(rast_val_name, raster_fun_name, sep = "_")
    
    # Adding raster metric(s) summary to tree_summary at subplot level
    if(length(raster_fun) == 1) {
      tree_summary[, eval(raster_value_fun_name) := extract_rast_val[, raster_fun(get(rast_val_name), ...) , by=c("ID")][,"V1"]]
    } else {
      for(i in 1:length(raster_fun)) {
        tree_summary[, raster_value_fun_name[i] := extract_rast_val[, raster_fun[[i]](get(rast_val_name), ...) , by=c("ID")][,"V1"]]
      }
    }
    # Check raster_fun(s) that would returned more than one value
    if("list" %in% sapply(tree_summary,typeof)) {
      stop("the function provided using `raster_fun` must return a single value")
    }
    # Adding raster metric(s) summary to sf_polygons
    sf_polygons <- cbind(sf_polygons, tree_summary[,..raster_value_fun_name])
  }
  
  
  # Plot the plot(s) -----------------------------------------------------------
  
  if(is.null(AGB_simu)) {
    
    plot_list <- lapply( split(sf_polygons , sf_polygons$plot_ID) , function(sf_pol) { # sf_pol = split(sf_polygons , sf_polygons$plot_ID)[[1]]
      
      plot_design <- lapply(names(sf_pol)[! names(sf_pol) %in% c("plot_ID","subplot_ID","sf_subplot_polygon")] , function(metric_name) {
        pol_bbox <- sapply(sf_pol$sf_subplot_polygon, sf::st_bbox)
        ggplot(data = sf_pol) +
          geom_sf(mapping = aes(fill=.data[[metric_name]], geometry = sf_subplot_polygon)) +
          theme_minimal() + 
          scale_fill_gradientn(colours = rev(terrain.colors(20))) +
          lims(x = c(min(pol_bbox["xmin",]), max(pol_bbox["xmax",])), 
               y = c(min(pol_bbox["ymin",]), max(pol_bbox["ymax",]))) +
          theme(legend.position="bottom", #legend.title = element_blank(),
                legend.key.size = unit(0.8,"cm"), 
                axis.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust=1)
          ) + 
          ggtitle( paste( ifelse(unique(sf_pol$plot_ID) == "" , "" , paste(unique(sf_pol$plot_ID) , ":")),
                          gsub("_",replacement = " ", metric_name)) )
        
      })
      
      
      
      if(draw_plot) print(plot_design)
      
      return(invisible(plot_design))
      
    })
  }
  
  # Outputs --------------------------------------------------------------------
  
  # Organize the ggplots outputs :
  if(is.null(AGB_simu)) {
    if( length(unique(sf_polygons$plot_ID)) == 1 ) { # If just one plot :
      sf_polygons$plot_ID <- NULL # delete plot_id column
      if(length(value)==1) { # If one metric :
        plot_list <- plot_list[[1]][[1]] # double unlist the output
      } else {
        plot_list <- plot_list[[1]] # unlist the output
      }
    } # If several plots, all is OK
    output <- list(tree_summary = as.data.frame(sf_polygons)[,-match("sf_subplot_polygon",names(sf_polygons))], polygon = sf_polygons, plot_design = plot_list)
  } else {
    if( length(unique(sf_polygons$plot_ID)) == 1 ) { # If just one plot :
      sf_polygons[,plot_ID:=NULL] # delete plot_id column
    }
    sf_polygons[,sf_subplot_polygon:=NULL]
    
    # Joined center coordinates
    sf_polygons <- sf_polygons[unique(corner_dat[,c("subplot_ID","x_center","y_center")]),,on="subplot_ID"]
    
    sf_polygons[,N_simu := as.numeric(tstrsplit(subplot_ID, split = "_N_", keep = 2)[[1]])]
    sf_polygons[,subplot_ID := gsub("_N_\\d+","",subplot_ID)]
    data.table::setorder(sf_polygons,subplot_ID,N_simu)
    output <- sf_polygons
  }
  
  
  return(output)
}

