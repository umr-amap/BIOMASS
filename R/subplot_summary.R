#' Summarise and display tree information by subplot
#'
#' @description
#' After applying the [divide_plot()] function, this function summarises with any defined function the desired tree metric by sub-plot and displays the plot representation.
#'
#' @param subplots output of the [divide_plot()] function
#' @param value a character indicating the column in subplots$tree_data (a tree metric) to be summarised 
#' @param draw_plot a logical indicating whether the plot design should be displayed
#' @param per_ha a logical indicating whether the metric summary should be per hectare
#' @param fun the function to be applied
#' @param ... optional arguments to fun
#'
#' @return a list containg the following elements :
#'  - `tree_summary` : a summary of the metric per subplot 
#'  - `polygon` : an sf object : simple feature collection of the subplot's polygon
#'  - `plot_design` : a ggplot object (or a list of ggplot objects) that can easily be modified
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
#'   subplots_201_sum$plot_design
#' }
#'
subplot_summary <- function(subplots, value = NULL, draw_plot = TRUE, per_ha = TRUE, fun = sum, ...) {

  # Checking parameters --------------------------------------------------------
  if(is.data.frame(subplots)) {
    stop("subplots argument does'nt contain any tree data frame. Use the divide_plot function with a non-null tree_data argument")
  }
  if (is.list(subplots) && (is.null(names(subplots)) || any(names(subplots)!=c("sub_corner_coord","tree_data")))) {
    stop("subplots argument must be the output of the divide_plot_function, with a non-null tree_data argument")
  }
  if(any(!c("x_proj","y_proj") %in% names(subplots$sub_corner_coord))) {
    subplots$sub_corner_coord[,c("x_proj","y_proj")] <- subplots$sub_corner_coord[,c("x_rel","y_rel")]
    message("Projected coordinates are not found in sub_corner_coord$subplots, tree metric will be summarised in the relative coordinate system")
  }
  if(is.null(value)) {
    stop("You must supply the tree variable to be summarised via the value argument.")
  }
  if(!value %in% names(subplots$tree_data)) {
    stop(paste(value,"is not a column name of subplots$tree_data"))
  }
  if(!is.function(fun)) {
    stop("the function supplied using `fun =` is not a function")
  } else {
    fun <- match.fun(fun)
  }

  # Data processing ------------------------------------------------------------
  corner_dat <- data.table(subplots$sub_corner_coord)
  tree_summary <- data.table(subplots$tree_data)[!is.na(subplot_ID), fun(get(value), ...) , by=c("subplot_ID")]
  if(any(duplicated(tree_summary$subplot_ID))) {
    stop("the function supplied using `fun` must return a single value")
  }
  tree_summary[, plot_id:= #Add plot_id to be able to loop on it
                 sapply(strsplit(tree_summary$subplot_ID,"_"),
                        function(x) paste(x[-((length(x)-1) : length(x))],collapse="_"))] # instead of function(x) x[1] in case plot_id contains any "_"
  
  sf_polygons <- do.call(rbind,lapply(split(corner_dat, by = "subplot_ID"), function(dat) {
    
    # creating polygon
    mat <- dat[, .(x_proj, y_proj)]
    mat <- as.matrix(rbind(mat, mat[1, ]))
    subplot_polygon <- sf::st_polygon(list(mat))
    
    # value summarised by the function
    value_fun <- tree_summary[subplot_ID == unique(dat$subplot_ID), V1]
    if(identical(value_fun,numeric(0))) value_fun <- 0
    
    # creating data-frame to associate with the polygon
    df_polygon <- data.frame(unique(dat$subplot_ID),
                             value_fun,
                             value_fun / sf::st_area(subplot_polygon) * 10000)
    names(df_polygon) <- c("subplot_ID",
                           paste(value, "summary", sep = "_"),
                           paste(value, "summary_per_ha", sep = "_"))
    
    sf_polygon <- sf::st_sf(list(subplot_polygon),df_polygon)
    sf::st_geometry(sf_polygon) <- "sf_subplot_polygon"
    sf_polygon
  }))
  sf_polygons <- sf_polygons[order(sf_polygons$subplot_ID),]
  
  # Plot the plot(s) -----------------------------------------------------------
  
  plot_list <- lapply(unique(tree_summary$plot_id), function(plot_id) {
    
    plot_design <- ggplot(sf_polygons[grep(plot_id,sf_polygons$subplot_ID),]) +
      geom_sf(mapping = aes(fill=.data[[names(sf_polygons)[ifelse(per_ha , 3, 2)]]])) +
      theme_minimal() + 
      scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
      theme(legend.position="bottom", #legend.title = element_blank(),
            legend.key.size = unit(0.8,"cm"), 
            axis.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust=1)
            )
    if(plot_id == "subplot") {
      plot_design <- plot_design + ggtitle(paste("Summary of",value,ifelse(per_ha,"per ha","")))
    } else {
      plot_design <- plot_design + ggtitle(paste(plot_id,": summary of",value,ifelse(per_ha,"per ha","")))
    }
    
    if(draw_plot) print(plot_design)
    
    return(invisible(plot_design))
  })
  names(plot_list) <- unique(tree_summary$plot_id)
  
  # Adjusting column names and adding summary per hectare
  setnames(tree_summary, "V1", paste(value,"summary",sep="_"))
  tree_summary <- data.frame(tree_summary[order(subplot_ID)])
  
  tree_summary[[paste(value,"summary_per_ha",sep="_")]] <- sf_polygons[[3]][match(x = tree_summary$subplot_ID , table = sf_polygons[[1]])]
  
  if(all(tree_summary$plot_id=="subplot")) { # If just one plot :
    tree_summary$plot_id <- NULL # delete plot_id column
    plot_list <- plot_list[[1]] # unlist the output
  } else{
    tree_summary <- tree_summary[,c(3,1,2,4)] # move plot_id column in first position
  }
  
  output <- list(tree_summary = tree_summary, polygon = sf_polygons, plot_design = plot_list)
  
  return(output)
}

