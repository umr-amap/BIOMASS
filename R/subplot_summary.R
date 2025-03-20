#' Summarise and display tree information by subplot
#'
#' @description
#' After applying the [divide_plot()] function, this function summarises with any defined function the desired tree metric by sub-plot and displays the plot representation.
#'
#' @param subplots output of the [divide_plot()] function
#' @param value a character indicating the column in subplots$tree_data to be summarised (or character vector to summarise several metrics at once)
#' @param draw_plot a logical indicating whether the plot design should be displayed
#' @param per_ha a logical indicating whether the metric summary should be per hectare (or, if summarising several metrics at once: a logical vector corresponding to each metric (see examples))
#' @param fun the function to be applied (or, if summarising several metrics at once: a list of functions named according to each metric (see examples))
#' @param ... optional arguments to fun
#'
#' @return a list containing the following elements :
#'  - `tree_summary` : a summary of the metric per subplot 
#'  - `polygon` : an sf object : simple feature collection of the subplot's polygon
#'  - `plot_design` : a ggplot object (or a list of ggplot objects) that can easily be modified
#'
#' @export
#' 
#' @author Arthur Bailly
#'
#' @importFrom data.table data.table := set
#' @importFrom grDevices terrain.colors
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal scale_fill_gradientn theme ggtitle
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
subplot_summary <- function(subplots, value = NULL, draw_plot = TRUE, per_ha = TRUE, fun = sum, ...) {

  # Checking parameters --------------------------------------------------------
  if(is.data.frame(subplots)) {
    stop("subplots argument does'nt contain any tree data frame. Use the divide_plot function with a non-null tree_data argument")
  }
  if (is.list(subplots) && ( is.null(names(subplots)) || (!"tree_data" %in% names(subplots)) ) ) {
    stop("subplots argument must be the output of the divide_plot_function, with a non-null tree_data argument")
  }
  if(any(!c("x_proj","y_proj") %in% names(subplots$sub_corner_coord))) {
    subplots$sub_corner_coord[,c("x_proj","y_proj")] <- subplots$sub_corner_coord[,c("x_rel","y_rel")]
    message("Projected coordinates are not found in sub_corner_coord$subplots, tree metric will be summarised in the relative coordinate system")
  }
  if(is.null(value)) {
    stop("You must provide the tree variable to be summarised via the value argument.")
  }
  if(!any(value %in% names(subplots$tree_data))) {
    stop(paste(value,"is not a column name of subplots$tree_data"))
  }
  
  # Get the name of the function (before the evaluation of fun, otherwise we will get the function and not the name of the argument)
  if( length(value) > 1) {
    fun_name <- as.character(substitute(fun))[-1]
  } else {
    fun_name <- as.character(substitute(fun))
  }

  # Check if fun is function
  if( !is.list(fun) ) {
    if( !is.function(fun) ) {
      stop("the function provided using `fun =` is not a function")
    } else {
      fun <- match.fun(fun)
    }
  }
  # Check if fun is a list of functions
  if( is.list(fun) ) {
    if( any(!sapply(fun , is.function)) ) {
      stop(paste("incorrect", fun_name[!sapply(fun , is.function)], "function(s) provided (not a function)"))
    } else {
      fun <- lapply(fun , match.fun)
    }
  }
  if( length(value) > 1 && length(per_ha) == 1 ) {
    per_ha = rep(per_ha, length(value))
  }
  if( is.list(fun) && length(value) != length(fun) ) {
    stop("the lengths of 'value' and 'fun' are not the same")
  }
  if( is.list(fun) && length(value) != length(per_ha) ) {
    stop("the lengths of 'value' and 'per_ha' are not the same")
  }

  # Data processing ------------------------------------------------------------
  value_fun_name <- paste(value, fun_name, sep = "_")
  
  corner_dat <- data.table(subplots$sub_corner_coord)
  # If just one plot, add an empty character plot_ID column (used for ggploting)
  if( is.null(corner_dat$corner_plot_ID) ) {
    corner_dat[ , plot_ID := ""]
  } else{ # if several plots, rename corner_plot_ID by plot_ID
    setnames(corner_dat, "corner_plot_ID", "plot_ID")
  }
  
  tree_summary <- data.table(subplots$tree_data)
  
  # Adding metric(s) summary to tree_summary at subplot level
  if(length(value) == 1) {
    tree_summary <- tree_summary[!is.na(subplot_ID), fun(get(value), ...) , by=c("subplot_ID")]
    setnames(tree_summary, "V1", value_fun_name)
  } else { # Apply functions to values by subplot
    tree_summary <- tree_summary[!is.na(subplot_ID), lapply( 1:length(value), function(i) fun[[i]](get(value[i])) ) , by=c("subplot_ID")]
    setnames(tree_summary, 2:(length(value)+1), value_fun_name)
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
  
  
  # Creating sf_polygons that will be the $polygon output of the function (a simple feature collection of the subplot's polygon)
  sf_polygons <- do.call(rbind,lapply(split(corner_dat, by = "subplot_ID"), function(dat) { #dat = split(corner_dat, by = "subplot_ID")[[1]]
    # For each subplot: 
    
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
      if(is.null(subplots$UTM_code$corner_plot_ID) ) { # if one plot with no plot ID
        sf::st_crs(sf_polygon) <- subplots$UTM_code$UTM_code
      } else { # if several plots with IDs
        sf::st_crs(sf_polygon) <- subplots$UTM_code$UTM_code[subplots$UTM_code$corner_plot_ID == unique(dat$plot_ID)]
      }
    }
    
    sf::st_geometry(sf_polygon) <- "sf_subplot_polygon" #rename last column
    sf_polygon
  }))
  sf_polygons <- sf_polygons[order(sf_polygons$subplot_ID),]
  
  
  # Plot the plot(s) -----------------------------------------------------------
  
  plot_list <- lapply( split(sf_polygons , sf_polygons$plot_ID) , function(sf_pol) { # sf_pol = split(sf_polygons , sf_polygons$plot_ID)[[2]]
    
    plot_design <- lapply(names(sf_pol)[! names(sf_pol) %in% c("plot_ID","subplot_ID","sf_subplot_polygon")] , function(metric_name) {
      ggplot(data = sf_pol) +
        geom_sf(mapping = aes(fill=.data[[metric_name]])) +
        theme_minimal() + 
        scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
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

  # Outputs --------------------------------------------------------------------
  
  # Organize the ggplots outputs :
  if( length(unique(sf_polygons$plot_ID)) == 1 ) { # If just one plot :
    sf_polygons$plot_ID <- NULL # delete plot_id column
    if(length(value)==1) { # If one metric :
      plot_list <- plot_list[[1]][[1]] # double unlist the output
    } else {
      plot_list <- plot_list[[1]] # unlist the output
    }
  } # If several plots, all is OK
  
  output <- list(tree_summary = sf::st_drop_geometry(sf_polygons), polygon = sf_polygons, plot_design = plot_list)
  
  return(output)
}

