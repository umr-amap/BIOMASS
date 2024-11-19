#' Summarise and display tree information by subplot
#'
#' @description
#' After applying the [divide_plot()] function, this function summarises with any defined function the desired tree metric by sub-plot and displays the plot representation.
#'
#' @param subplots output of the [divide_plot()] function
#' @param value a character indicating the column in subplots$tree_df to display
#' @param draw_plot a logical indicating whether the plot design should be displayed and returned
#' @param fun the function to be applied
#' @param ... optional arguments to fun
#'
#' @return a list containg the following elements :
#'  - `tree_summary` : a summary of the metric per subplot 
#'  - `polygon` : an sf object : simple feature collection of the subplot's polygon
#'  - `plot_design` : (if draw_plot=T) a ggplot object (or a list of ggplot objects) that can easily be modified
#'
#' @export
#' 
#' @author Arthur Bailly
#'
#' @importFrom data.table data.table :=
#' @importFrom grDevices terrain.colors
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal scale_fill_gradientn theme ggtitle
#' 
#' @examples
#' rel_coord <- data.frame(x_rel = c(0, 200, 0, 200), y_rel = c(0, 0, 200, 200))
#' proj_coord <- data.frame(x_proj = c(210, 383, 110, 283), y_proj = c(210, 310, 383, 483))
#' tree_df <- data.frame(x_tree = runif(50,0,200), y_tree = runif(50,0,200), metric = rnorm(50,10,5))
#' subplots <- BIOMASS::divide_plot(rel_coord, proj_coord = proj_coord, grid_size = 50, tree_df = tree_df, tree_coords = c("x_tree","y_tree"))
#' subplot_summary(subplots , value = "metric", draw_plot = FALSE) # sum summary
#' subplot_summary(subplots , value = "metric", draw_plot = FALSE, fun = median, probs=0.9) # sum summary
#'
#'
subplot_summary <- function(subplots, value = NULL, draw_plot = TRUE, fun = sum, ...) {

  # Checking parameters --------------------------------------------------------
  if(is.data.frame(subplots)) {
    stop("subplots argument does\'nt contain any tree data frame. Use the divide_plot() function with a non-null tree_df argument")
  }
  if (!is.list(subplots) && any(names(subplots)!=c("sub_corner_coord","tree_df"))) {
    stop("subplots argument must be the output of the divide_plot_function(), with a non-null tree_df argument")
  }
  if(any(!c("x_proj","y_proj") %in% names(subplots$sub_corner_coord))) {
    subplots$sub_corner_coord[,c("x_proj","y_proj")] <- subplots$sub_corner_coord[,c("x_rel","y_rel")]
    message("projected coordinates are not found in subplots items (no x_proj and y_proj colnames found), tree metric will be summarised in the relative coordinate system")
  }
  if(!is.null(value) && !value %in% names(subplots$tree_df)) {
    stop("value is not a column name of subplots$tree_df")
  }
  if(!is.function(fun)) {
    stop(paste("your function",function_name, "is not a function"))
  } else {
    fun <- match.fun(fun)
  }

  # Data processing ------------------------------------------------------------
  corner_dat <- data.table(subplots$sub_corner_coord)
  tree_summary <- data.table(subplots$tree_df)[!is.na(subplot_id), fun(get(value), ...) , by=c("subplot_id")]
  tree_summary[,plot_id:=sapply(strsplit(tree_summary$subplot_id,"_"),function(x)x[[1]])]#Add plot_id to be able to loop on it
  
  sf_polygons <- do.call(rbind,lapply(split(corner_dat, by = "subplot_id"), function(dat) {
    
    # creating polygon
    mat <- dat[, .(x_proj, y_proj)]
    mat <- as.matrix(rbind(mat, mat[1, ]))
    subplot_polygon <- sf::st_polygon(list(mat))
    
    # value summarised by the function
    value_fun <- tree_summary[subplot_id == unique(dat$subplot_id), V1]
    if(identical(value_fun,numeric(0))) value_fun <- 0
    
    # creating data-frame to associate with the polygon
    df_polygon <- data.frame(unique(dat$subplot_id),
                             value_fun,
                             value_fun / sf::st_area(subplot_polygon) * 10000)
    names(df_polygon) <- c("subplot_id",
                           paste(value, "summary", sep = "_"),
                           paste(value, "summary", "per_ha", sep = "_"))
    
    sf_polygon <- sf::st_sf(list(subplot_polygon),df_polygon)
    sf::st_geometry(sf_polygon) <- "sf_subplot_polygon"
    sf_polygon
  }))
  
  # Plot the plot(s) -----------------------------------------------------------
  
  if(draw_plot) {
    
    plot_list <- lapply(unique(tree_summary$plot_id), function(plot_id) {
      
      plot_design <- ggplot(sf_polygons[grep(plot_id,sf_polygons$subplot_id),]) +
        geom_sf(mapping = aes(fill=.data[[names(sf_polygons)[3]]])) +
        theme_minimal() + 
        scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
        theme(legend.position="bottom", #legend.title = element_blank(),
              legend.key.size = unit(0.8,"cm"), 
              axis.title = element_blank()
              )
      if(plot_id == "subplot") {
        plot_design <- plot_design + ggtitle(paste("Summary of",value,"per ha"))
      } else {
        plot_design <- plot_design + ggtitle(paste(plot_id,": summary of",value,"per ha"))
      }
        
      print(plot_design)
      plot_design
    })
    names(plot_list) <- unique(tree_summary$plot_id)
    
  }
  
  # Adjusting column names and adding summary per hectare
  setnames(tree_summary, "V1", paste(value,"summary",sep="_"))
  tree_summary <- data.frame(tree_summary[order(subplot_id)])
  
  tree_summary[[paste(value,"summary per_ha",sep="_")]] <- sf_polygons[[3]][match(x = tree_summary$subplot_id , table = sf_polygons[[1]])]
  
  if(all(tree_summary$plot_id=="subplot")) { # If just one plot :
    tree_summary$plot_id <- NULL # delete plot_id column
    plot_list <- plot_list[[1]] # unlist the output
  } else{
    tree_summary <- tree_summary[,c(3,1,2,4)]
  }
  
  output <- list(tree_summary = tree_summary, polygon = sf_polygons)
  
  if(draw_plot) {
    output$plot_design <- plot_list
  }
  
  return(output)
}

