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
#' @importFrom stats reshape setNames
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
  if(any(!c("x_proj","y_proj") %in% names(subplots$sub_corner_coord))) {
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
    if(is.null(subplots$simu_coord)) {
      message("AGB uncertainties will be propagated without propagation of corner GPS measurement uncertainties.")
    } else if (ncol(AGB_simu) != length(subplots$simu_coord)) {
      if( ncol(AGB_simu) < length(subplots$simu_coord) ) {
        message(paste("As the 'AGB_simu' matrix only contains", ncol(AGB_simu),"simulations, the first", ncol(AGB_simu),"simulations contained in 'subplots$simu_coord' will be considered."))
        subplots$simu_coord <- subplots$simu_coord[1:ncol(AGB_simu)]
      } else {
        message(paste("As 'AGB_simu' contains", ncol(AGB_simu),"simulations, and 'subplots$simu_coord' contains", length(subplots$simu_coord),"simulations,", ncol(AGB_simu)-length(subplots$simu_coord),"simulations will be resampled in 'subplots'."))
        # keep in memory some parameters
        N_simu_coord <- length(subplots$simu_coord)
        N_row_coord <- nrow(subplots$simu_coord[[1]])
        simu_resampled <- sample(x = 1:N_simu_coord, size = ncol(AGB_simu)-length(subplots$simu_coord), replace = TRUE)
        # resample coordinate simulations to match the number of AGB simulations
        subplots$simu_coord <- c(subplots$simu_coord, subplots$simu_coord[simu_resampled])
      }
    }
  }
  if(is.null(AGB_simu) & is.null(ref_raster) & !is.null(subplots$simu_coord)) {
    warning("In BIOMASS, the propagation of uncertainties in the coordinates of the plot corners does not make sense if it is not accompanied by raster data or the propagation of biomass uncertainties. For this reason, the following results will not take into account the uncertainty of corner coordinates.")
  }
  
  ### Getting function's name ----
  # the issue was in BIOMASSapp when calling the function as follows: 
  # arg_fun <- available_functions[[input$sel_user_function]]
  # subplot_summary(..., fun = arg_fun)
  # it returned column names like "AGB_arg_fun_per_ha"
  # code generated with Claude AI
  
  # Helper function to find original function name ----
  find_function_name <- function(func) {
    search_packages <- c(".GlobalEnv", search())
    for (env_name in search_packages) {
      env <- tryCatch(as.environment(env_name), error = function(e) NULL)
      if (is.null(env)) next
      
      obj_names <- ls(envir = env, all.names = FALSE)
      
      for (obj_name in obj_names) {
        obj <- tryCatch(get(obj_name, envir = env, inherits = FALSE), error = function(e) NULL)
        if (!is.null(obj) && is.function(obj) && identical(obj, func)) {
          return(obj_name)
        }
      }
    }
    return(NULL)
  }
  
  ## retrieve the name(s) of the function(s) as character(s) ----
  fun_name_raw <- if (length(value) > 1) {
    as.character(substitute(fun))[-1]
  } else {
    deparse(substitute(fun))
  }
  
  # Evaluate the function in the parent frame
  fun_eval <- eval(substitute(fun), envir = parent.frame())
  # Get the name(s) of the function(s)
  if (is.list(fun_eval)) { # if multiple functions provided in a list, loop on fun_eval
    fun_name <- sapply(seq_along(fun_eval), function(i) {
      if (!is.function(fun_eval[[i]])) { # if not a correct function
        stop(paste("the function", fun_name_raw[i], "provided in `fun =` is not a function"))
      } else {
        found_name <- find_function_name(fun_eval[[i]])
        if(!is.null(found_name)) { # if the name has been found, return it
          return(found_name)
        } else { # else, return the fun_name_raw
          return(fun_name_raw[i])
        }
      }
    })
  } else if (is.function(fun_eval)) { # if only one function
    found_name <- find_function_name(fun_eval)
    if(!is.null(found_name)) { # if the name has been found, return it
      fun_name <- found_name
    } else { # else, return the fun_name_raw
      fun_name <- fun_name_raw
    }
  } else {
    stop("the function provided using `fun =` is not a function")
  }
  
  ### The same for raster_fun ----
  # Check is ref_raster is a SpatRaster
  if(!is.null(ref_raster) && !is(ref_raster, "SpatRaster") ) {
    stop("ref_raster is not recognised as a SpatRaster of terra package")
  }
  # retrieve the name(s) of the function(s) as character(s)
  raster_fun_name_raw <- if (length(raster_fun) > 1) {
    as.character(substitute(raster_fun))[-1]
  } else {
    deparse(substitute(raster_fun))
  }
  # Evaluate the function in the parent frame
  raster_fun_eval <- eval(substitute(raster_fun), envir = parent.frame())
  # Get the name(s) of the function(s)
  if (is.list(raster_fun_eval)) { # if multiple functions provided in a list, loop on fun_eval
    raster_fun_name <- sapply(seq_along(raster_fun_eval), function(i) {
      if (!is.function(raster_fun_eval[[i]])) { # if not a correct function
        stop(paste("the function", raster_fun_name_raw[i], "provided in `raster_fun =` is not a function"))
      } else {
        found_name <- find_function_name(raster_fun_eval[[i]])
        if(!is.null(found_name)) { # if the name has been found, return it
          return(found_name)
        } else { # else, return the fun_name_raw
          return(raster_fun_name_raw[i])
        } 
      }
    })
  } else if (is.function(raster_fun_eval)) { # if only one function
    found_name <- find_function_name(raster_fun_eval)
    if(!is.null(found_name)) { # if the name has been found, return it
      raster_fun_name <- found_name
    } else { # else, return the fun_name_raw
      raster_fun_name <- fun_name_raw
    }
  } else {
    stop("the function provided using `raster_fun =` is not a function")
  }
  
  
  # Check parameter length
  if(!is.null(value)) {
    # Replicate per_ha if needed
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
  
  
  # Data processing ------------------------------------------------------------
  if(!is.null(value)) {
    value_fun_name <- paste(value, fun_name, sep = "_")
  } else {
    value_fun_name <- NULL
  }
  # Formatting intial corner data
  corner_dat <- data.table(subplots$sub_corner_coord)
  corner_dat[ , c("x_center","y_center") := list(mean(x_proj),mean(y_proj)) , by = subplot_ID]
  
  # Formatting simulated corner data
  if(!is.null(subplots$simu_coord)) {
    simu_corner_dat <- do.call(rbind,lapply(1:length(subplots$simu_coord), function(n_simu) { # n_simu = 1
      cor_dat <- data.table(subplots$simu_coord[[n_simu]])
      # Adding simulation number and subplot centre coordinates
      cor_dat[, N_simu := n_simu]
      cor_dat[ , c("x_center","y_center") := list(mean(x_proj),mean(y_proj)) , by = subplot_ID]
    }))
  }
  
  # Creating tree_summary that will be the main output
  tree_summary <- data.table(subplots$tree_data)
  # stop if there is any column named "Vn" where n is an integer
  if( length(grep("^V[0-9]+",names(tree_summary))) != 0) stop("Sorry but you can't have any columns named 'Vn', where n is an integer, in subplots$tree_data")
  
  # Calculating metric(s) summary ----
  # and adding it(s) to tree_summary at subplot level
  if(!is.null(value)) {
    if(length(value) == 1) {
      #tree_summary <- tree_summary[!is.na(subplot_ID), fun(get(value)) , by=c("subplot_ID"), ]
      tree_summary <- tree_summary[!is.na(subplot_ID), fun(get(value), ...) , by=c("subplot_ID")]
      setnames(tree_summary, "V1", value_fun_name)
    } else { # Apply functions to values by subplot
      #tree_summary <- tree_summary[!is.na(subplot_ID), lapply( 1:length(value), function(i) fun[[i]](get(value[i])) ) , by=c("subplot_ID")]
      tree_summary <- tree_summary[!is.na(subplot_ID), lapply( 1:length(value), function(i) fun[[i]](get(value[i]), ...) ) , by=c("subplot_ID")]
      setnames(tree_summary, 2:(length(value)+1), value_fun_name)
    }
    # Check function(s) that would returned more than one value
    if(any(duplicated(tree_summary$subplot_ID))) {
      stop("the function provided using `fun` must return a single value")
    }
    # Check if there was a subplot without any tree
    if(nrow(tree_summary) != nrow(corner_dat)) {
      tree_summary <- tree_summary[data.table(subplot_ID = unique(corner_dat$subplot_ID)),
                                   on = "subplot_ID"]
      # replace NA by 0
      #for (i in names(tree_summary)[-1]) data.table::set(tree_summary,which(is.na(tree_summary[[i]])),i,0) 
    }
  } else {
    # if value is not provided, we still need to format tree_summary
    tree_summary <- data.table(subplot_ID = unique(corner_dat$subplot_ID))
  }
  
  # Calculating summary of AGB simulated values at subplot level
  if(!is.null(AGB_simu)) {
    AGB_simu <- data.table(AGB_simu)
    names(AGB_simu) <- paste("AGB",1:ncol(AGB_simu), sep="_")
    AGB_simu[, subplot_ID :=  subplots$tree_data$subplot_ID]
    #value_fun_name_AGB_simu <- paste0(value_AGB_simu,"_sum")
    AGB_simu_sum <- AGB_simu[!is.na(subplot_ID), lapply(.SD, sum), by = subplot_ID, .SDcols = !"subplot_ID"]
    
    # Check if there was a subplot without any tree
    if(nrow(AGB_simu_sum) != nrow(corner_dat)) {
      AGB_simu_sum <- AGB_simu_sum[data.table(subplot_ID = unique(corner_dat$subplot_ID)),
                                   on = "subplot_ID"]
      # replace NA by 0
      #for (i in names(AGB_simu_sum)[-1]) data.table::set(AGB_simu_sum,which(is.na(AGB_simu_sum[[i]])),i,0) 
    }
  }
  
  
  # Function to create sf_polygons and calculate the associated area ----
  create_polygon_fct <- function(dat) { # dat = corner_dat[ 1:4 , ] 
    
    # Create the associated polygon
    mat <- dat[, .(x_proj, y_proj)]
    mat <- as.matrix(rbind(mat, mat[1, ]))
    subplot_polygon <- sf::st_polygon(list(mat))
    
    # Get 1 summary row instead of 4 (because there was 4 corners) 
    coord_col_names <- c("x_rel","y_rel","x_proj","y_proj","long","lat")
    df_polygon <- unique(dat[,.SD,.SDcols = names(dat)[! names(dat) %in% coord_col_names] ])
    df_polygon$area <-  sf::st_area(subplot_polygon) / 10000
    
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
  
  # Creating sf_polygons ----
  
  ## for initial corners
  # that will be the $polygon output of the function (a simple feature collection of the subplot's polygon)
  sf_polygons <- corner_dat[ , create_polygon_fct(.SD) , by = "subplot_ID", .SDcols = colnames(corner_dat)][,-1]
  # sf_polygons <- sf_polygons[order(sf_polygons$subplot_ID),]
  
  # for simulated corners
  # used to extract the raster metric : we take unique polygons to not calculate multiple times the same subplot in the case of repeated corner simulations 
  # print("Calculating area for simulated corners...")
  if(!is.null(subplots$simu_coord)) {
    if(exists("N_simu_coord")) {
      inital_simu_coord <- copy(simu_corner_dat[1:(N_simu_coord*N_row_coord),])
    } else {
      inital_simu_coord <- copy(simu_corner_dat)
    }
    sf_simu_polygons <- inital_simu_coord[ , create_polygon_fct(.SD) , by = c("subplot_ID","N_simu"), .SDcols = colnames(inital_simu_coord)][,-c(1,2)]
  }
  
  # Divide tree metrics per number of hectare ----
  if(!is.null(value)) {
    for(i in 1:length(per_ha)) {
      if (per_ha[i]) {
        tree_summary[ , paste(value_fun_name[i], "per_ha", sep = "_") :=  get(value_fun_name[i])/ sf_polygons$area]
        tree_summary[ , eval(value_fun_name[i]) :=  NULL]
      }
    }
  }
  
  
  # Extract raster values ----
  # use of extract just once instead of multiple time if it was done during the sf_polygons creation
  if( !is.null(ref_raster)) {
    cat("Extracting raster metric...")
    if(is.null(subplots$simu_coord)) {
      extract_rast_val <- extract(x = ref_raster, y = vect(sf_polygons$sf_subplot_polygon), exact = TRUE)
    } else {
      extract_rast_val <- extract(x = ref_raster, y = vect(sf_simu_polygons$sf_subplot_polygon), exact = TRUE)
    }
    cat("Extracting raster metric done.")
    # keeping raster values whose fraction are > 0.5
    extract_rast_val = data.table(extract_rast_val)[ fraction >0.5,]
    rast_val_name <- names(extract_rast_val)[2]
    raster_value_fun_name <- paste(rast_val_name, raster_fun_name, sep = "_")
    
    if(is.null(subplots$simu_coord)) {
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
    } else {
      # Adding raster metric(s) summary to sf_simu_polygons at subplot level
      if(length(raster_fun) == 1) {
        sf_simu_polygons[, eval(raster_value_fun_name) := extract_rast_val[, raster_fun(get(rast_val_name), ...) , by=c("ID")][,"V1"]]
        #sf_simu_polygons[, eval(raster_value_fun_name) := extract_rast_val[, raster_fun(get(rast_val_name)) , by=c("ID")][,"V1"]]
      } else {
        for(i in 1:length(raster_fun)) {
          sf_simu_polygons[, raster_value_fun_name[i] := extract_rast_val[, raster_fun[[i]](get(rast_val_name), ...) , by=c("ID")][,"V1"]]
          #sf_simu_polygons[, raster_value_fun_name[i] := extract_rast_val[, raster_fun[[i]](get(rast_val_name)) , by=c("ID")][,"V1"]]
        }
      }
      # Check raster_fun(s) that would returned more than one value
      if("list" %in% sapply(sf_simu_polygons[,.SD,.SDcols = !"sf_subplot_polygon"],typeof)) {
        stop("the function provided using `raster_fun` must return a single value")
      }
    }
    # # Adding raster metric(s) summary to sf_polygons
    # sf_polygons <- cbind(sf_polygons, tree_summary[,..raster_value_fun_name])
  }
  
  # Recreate the simulated coord data-table with all simulations if needed
  if(exists("simu_resampled")) {
    sf_simu_polygons <- rbind(
      sf_simu_polygons,
      do.call(rbind, lapply(simu_resampled, function(i) sf_simu_polygons[N_simu==i,]))
    )
  }
  
  # AGBD calculation ----
  # Calculate AGBD_sum_median, cred_2.5, cred_97.5
  if(!is.null(AGB_simu)) { # Reshape AGB_simu_sum to a long format to include the simulation number in subplot_ID
    # long_AGB_simu_sum will be the long format output of the function
    long_AGB_simu_sum <- reshape(data = AGB_simu_sum[,.SD, .SDcols = colnames(AGB_simu_sum)],
                                 varying = grep("AGB",colnames(AGB_simu_sum),value = TRUE) , direction = "long", v.names = "AGB_sum", ids = NULL)
    setnames(long_AGB_simu_sum,"time","N_simu")
    
    if(exists("sf_simu_polygons")) {
      # Add the area and coord_center of simulated subplots
      sf_simu_polygons[, N_simu := long_AGB_simu_sum$N_simu]
      long_AGB_simu_sum <- long_AGB_simu_sum[sf_simu_polygons[,c("subplot_ID","N_simu","x_center","y_center","area")], on = c("subplot_ID","N_simu")]
    } else {
      # Add the area and coord_center of initial subplots
      rep_sf_polygons <- do.call(rbind,lapply(1:max(long_AGB_simu_sum$N_simu), function(x) sf_polygons))
      rep_sf_polygons[, N_simu := long_AGB_simu_sum$N_simu]
      long_AGB_simu_sum <- long_AGB_simu_sum[rep_sf_polygons[,c("subplot_ID","N_simu","x_center","y_center","area")], on = c("subplot_ID","N_simu")]
    }
    long_AGB_simu_sum[, AGBD := AGB_sum/area]
    
    res_AGBD_simu <- long_AGB_simu_sum[, .(AGBD_median = median(AGBD, na.rm=TRUE),
                                           AGBD_cred_2.5 = quantile(AGB_sum / area, probs = 0.025, na.rm=TRUE),
                                           AGBD_cred_97.5 = quantile(AGB_sum / area, probs = 0.975, na.rm=TRUE)),
                                       by = subplot_ID]
    long_AGB_simu_sum[, c("AGB_sum","area") := list(NULL, NULL)]
    
    tree_summary <- tree_summary[res_AGBD_simu, on = "subplot_ID"]
  }
  
  # Raster metric(s) calculation ----
  # Calculate raster_metric_median, cred_2.5, cred_97.5 if simulated corners
  if(!is.null(ref_raster) && !is.null(subplots$simu_coord)) {
    res_raster_sim <- sf_simu_polygons[,
                                       setNames(
                                         list(
                                           median(get(raster_value_fun_name)),
                                           quantile(get(raster_value_fun_name), probs = 0.025, na.rm=TRUE),
                                           quantile(get(raster_value_fun_name), probs = 0.975, na.rm=TRUE)
                                         ),
                                         paste0(raster_value_fun_name, c("_median", "_cred_2.5", "_cred_97.5"))
                                       ),
                                       by = subplot_ID]
    tree_summary <- tree_summary[res_raster_sim, on = "subplot_ID"]
    
    if(exists("long_AGB_simu_sum")) {
      long_AGB_simu_sum[, eval(raster_value_fun_name) := sf_simu_polygons[[raster_value_fun_name]] ]
    }
  }
  
  # Add metrics to sf_polygons
  sf_polygons <- sf_polygons[tree_summary, on = "subplot_ID"]
  # Delete coord_center and area columns
  sf_polygons[, c("x_center","y_center","area") := list(NULL, NULL, NULL)]
  
  # tree_summary <- cbind(tree_summary[, .SD, .SDcols = !value_fun_name_AGB_simu], res_AGB_sim[,-1])
  # AGB_sim[ , subplot_ID := paste0(subplot_ID , "_N_" , time)][ , time := NULL]
  # value <- "AGB"
  # value_fun_name <- "AGB_sum"
  # value_fun_name <- value_fun_name[-grep("^V[0-9]+",value_fun_name)]
  # value_fun_name <- c(value_fun_name,c("AGB_median","AGB_cred_2.5","AGB_cred_97.5"))
  
  
  # Plot the plot(s) -----------------------------------------------------------
  displayed_cols <- names(sf_polygons)[! names(sf_polygons) %in% c("plot_ID","subplot_ID","area","sf_subplot_polygon")]
  displayed_cols <- displayed_cols[!grepl("_cred_",displayed_cols)]
  
  plot_list <- lapply( split(sf_polygons , sf_polygons$plot_ID) , function(sf_pol) { # sf_pol = split(sf_polygons , sf_polygons$plot_ID)[[1]]
    
    plot_design <- lapply(displayed_cols , function(metric_name) {
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
  
  
  # Outputs --------------------------------------------------------------------
  
  # Organize the ggplots outputs :
  # if(is.null(AGB_simu)) {
  #   if( length(unique(sf_polygons$plot_ID)) == 1 ) { # If just one plot :
  #     sf_polygons$plot_ID <- NULL # delete plot_id column
  #     if(length(value)==1) { # If one metric :
  #       plot_list <- plot_list[[1]][[1]] # double unlist the output
  #     } else {
  #       plot_list <- plot_list[[1]] # unlist the output
  #     }
  #   } # If several plots, all is OK
  #   output <- list(tree_summary = as.data.frame(sf_polygons)[,-match("sf_subplot_polygon",names(sf_polygons))], polygon = sf_polygons, plot_design = plot_list)
  # } else { # if AGB_simu
  #   output <- list(tree_summary = tree_summary, polygon = sf_polygons, plot_design = plot_list, long_AGB_simu = long_AGB_simu_sum)
  # }
  
  if(is.null(AGB_simu)) {
    output <- list(tree_summary = as.data.frame(sf_polygons)[,-match("sf_subplot_polygon",names(sf_polygons))], polygon = sf_polygons, plot_design = plot_list)
  } else { # if AGB_simu
    output <- list(tree_summary = tree_summary, polygon = sf_polygons, plot_design = plot_list, long_AGB_simu = long_AGB_simu_sum)
  }
  
  # # Joined center coordinates
  # sf_polygons <- sf_polygons[unique(corner_dat[,c("subplot_ID","x_center","y_center")]),,on="subplot_ID"]
  
  # sf_polygons[,N_simu := as.numeric(tstrsplit(subplot_ID, split = "_N_", keep = 2)[[1]])]
  # sf_polygons[,subplot_ID := gsub("_N_\\d+","",subplot_ID)]
  # data.table::setorder(sf_polygons,subplot_ID,N_simu)
  # output <- sf_polygons
  
  return(output)
}

