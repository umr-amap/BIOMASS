#' Summarize and display tree information by subplot
#'
#' @description
#'
#' @details
#' 
#'
#' @param subplots output of the function [divide_plot()]
#' @param value a character indicating the column in subplots$tree_ooord to display
#' @param fun the function to be applied
#' @param draw_plot a logical indicating wether the plot design should be displayed and returned
#' @param display_trees a logical indicating wether trees should be displayed
#'
#' @return a data frame where:
#'   - `plot`: the code of the plot
#'
#' If the `subplot` is set, the output is a list with the previous data frame and a simple features (sf) geometry object.
#'
#'
#' @export
#'
#' @importFrom data.table data.table := first setDT
#' @importFrom grDevices terrain.colors
#' @importFrom graphics segments
#' 
#' @examples
#'
#'
subplot_summary <- function(subplots, value=NULL, function_name="sum", draw_plot = TRUE, display_trees = FALSE) {

  # Checking parameters --------------------------------------------------------
  fun <- eval(parse(text = function_name))
  if(is.data.frame(subplots)) {
    stop("subplots argument does\'nt contain any tree data frame. Use the divide_plot() function with a non-null tree_coord argument")
  }
  if (!is.list(subplots) && any(names(subplots)!=c("sub_corner_coord","tree_coord"))) {
    stop("subplots argument must be the output of the divide_plot_function(), with a non-null tree_coord argument")
  }
  if(any(!c("x_proj","y_proj") %in% names(subplots$sub_corner_coord))) {
    subplots$sub_corner_coord[,c("x_proj","y_proj")] <- subplots$sub_corner_coord[,c("x_rel","y_rel")]
    #stop("projected coordinates are not found in subplots. Use the divide_plot() function with a non-null proj_coord argument (it can be proj_coord = rel_coord if you don\'t have any projected coordinates).")
  }
  if(!is.null(value) && !value %in% names(subplots$tree_coord)) {
    stop("value is not a column name of subplots$tree_coord")
  }
  if(!is.function(fun)) {
    stop(paste("your function",function_name, "is not a function"))
  }
  if(display_trees == T & draw_plot==F) {
    stop("display_trees cannot be TRUE if draw_plot is FALSE")
  }
  if(display_trees == T & any(!c("x_proj","y_proj") %in% names(subplots$tree_coord))) {
    stop("the tree_coord data-frame in subplots doesn\'t the columns x_proj and y_proj which are required to display the trees. Use the output of divide_plot() function with a non-null proj_coord argument (it can be proj_coord = rel_coord if you don\'t have any projected coordinates).")
  }

  # Data processing ------------------------------------------------------------
  corner_dat <- data.table(subplots$sub_corner_coord)
  tree_summary <- data.table(subplots$tree_coord)[, fun(get(value)) , by=c("subplot_id")]
  tree_summary[,plot_id:=sapply(strsplit(tree_summary$subplot_id,"_"),function(x)x[[1]])]#Add plot_id to be able to loop on it
  
  sf_polygons <- do.call(rbind,lapply(split(corner_dat, by = "subplot_id"), function(data) {
    
    # creating polygon
    mat <- data[, .(x_proj, y_proj)]
    mat <- as.matrix(rbind(mat, mat[1, ]))
    subplot_polygon <- sf::st_polygon(list(mat))
    
    # value summarized by the function
    value_fun <- tree_summary[subplot_id == unique(data$subplot_id), V1]
    if(identical(value_fun,numeric(0))) value_fun <- 0
    
    # creating data-frame to associate with the polygon
    df_polygon <- data.frame(unique(data$subplot_id),
                             value_fun,
                             value_fun / sf::st_area(subplot_polygon) * 10000)
    names(df_polygon) <- c("subplot_id",
                           paste(value, function_name, sep = "_"),
                           paste(value, function_name, "per_ha", sep = "_"))
    
    sf::st_sf(list(subplot_polygon),df_polygon)
  }))
  
  if(draw_plot) {
    
    plot_list <- lapply(unique(tree_summary$plot_id), function(plot_id) {
      
      plot_design <- ggplot() +
        geom_sf(data = sf_polygons[grep(plot_id,sf_polygons$subplot_id),],
                mapping = aes(fill=.data[[names(sf_polygons)[3]]])) +
        theme_minimal() + 
        scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
        theme(legend.position="bottom", #legend.title = element_blank(),
              legend.key.size = unit(0.8,"cm"), 
              axis.title = element_blank()
              )
      if(display_trees) {
        plot_design <- plot_design + 
          geom_point(data = subplots$tree_coord[grep(plot_id,subplots$tree_coord$subplot_id),],
                     mapping = aes(x = x_proj, y = y_proj),
                     shape = 3, size=0.5)
      }
      if(plot_id == "subplot") {
        plot_design <- plot_design + ggtitle(paste(function_name,"of",value,"per ha"))
      } else {
        plot_design <- plot_design + ggtitle(paste(plot_id,":",function_name,"of",value,"per ha"))
        
      }
        
      print(plot_design)
      plot_design
    })
    
  }
  
  setnames(tree_summary, "V1", paste(function_name,value,sep="_"))
  tree_summary <- data.frame(tree_summary)
  tree_summary[[paste(function_name,value,"per_ha",sep="_")]] <- sf_polygons[[3]]
  
  if(all(tree_summary$plot_id=="subplot")) {
    tree_summary$plot_id <- NULL
    plot_list <- plot_list[[1]]
  } else{
    tree_summary <- tree_summary[c(3,1,2,4)]
  }
  
  output <- list(tree_summary = tree_summary, polygon = sf_polygons)
  
  if(draw_plot) {
    output$plot_design <- plot_list
  }
  
  return(output)
}

