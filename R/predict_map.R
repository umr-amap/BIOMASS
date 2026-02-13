#' Predict map of AGBD and associated uncertainty
#'
#' @description
#' This function enables to produce a map of
#' the AGBD and associated uncertainty, using a spatially varying coefficient
#' calibrated model created with the [calibrate_model()] function.
#'
#' @param fit_brms a brmsfit object, output of the [calibrate_model()] function.
#' @param pred_raster filename (character) or a SpatRaster object from terra package: the raster to predict using fit_brms (typically a CHM raster created from LiDAR data)
#' @param grid_size a numeric indicating the dimension of grid cells. Must be identical to 'grid_size' used in [divide_plot()]
#' @param raster_fun the function to apply to summarize the values of 'pred_raster'. Must be identical to 'raster_fun' used in [subplot_summary()]
#' @param n_cores number of cores to use for predictions
#' @param n_post_draws positive integer indicating how many posterior draws should be used 
#' @param alignment_raster filename (character) or a SpatRaster object from terra package: a raster whose coordinates will be used to align the coordinates of the predicted raster.
#' @param plot_maps A logical indicating whether the maps should be displayed (median, sd and CV of AGBD posterior distributions)
#'
#' @details
#' Speak about parallel computing ?
#' 
#' @return 
#' The data-table format of 'pred_raster', to which the following variables have been added: 
#'  - post_median_AGBD: the median of the posterior distributions of the predicted AGBDs
#'  - post_sd_AGBD: the sd of the posterior distributions of the predicted AGBDs
#'  - post_cred_2.5_AGBD and post_cred_97.5_AGBD: the 2.5 and 97.5 quantiles of the posterior distributions of the predicted AGBDs
#' 
#'
#' @export
#' 
#' @author Arthur BAILLY, Dominique LAMONICA
#'
#' @importFrom data.table as.data.table
#' @importFrom ggplot2 scale_fill_gradientn coord_fixed geom_raster labs
#'
#' @examples
#' \dontrun{
#' 
#' }

predict_map <- function(fit_brms,
                        pred_raster,
                        grid_size,
                        raster_fun = mean,
                        n_cores = getOption("mc.cores", 1), # NOT SURE ABOUT THAT
                        n_post_draws = 50, # NOT SURE ABOUT THAT
                        alignment_raster = NULL,
                        plot_maps = TRUE) {
  
  # Checking arguments ---------------------------------------------------------
  # Check that CRS of pred_raster and alignment_raster are the same
  # Check that CRS of pred_raster is in projected coordinates (we need to provide a grid_size in meters to divide it)
  
  # Check that n_post_draws is < total post-warmup draws of fit_brms
  
  # Check if package brms is available
  if (!requireNamespace("foreach", quietly = TRUE)) {
    warning(
      'To speed up map predictions, you must install the "foreach" library \n\n',
      '\t\tinstall.packages("foreach")'
    )
    return(invisible(NULL))
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    warning(
      'To speed up map predictions, you must install the "foreach" library \n\n',
      '\t\tinstall.packages("future")'
    )
    return(invisible(NULL))
  }
  if (!requireNamespace("doFuture", quietly = TRUE)) {
    warning(
      'To speed up map predictions, you must install the "foreach" library \n\n',
      '\t\tinstall.packages("doFuture")'
    )
    return(invisible(NULL))
  }
  if (!requireNamespace("progressr", quietly = TRUE)) {
    warning(
      'In order to check the progression of map predictions, you must install the "progressr" library \n\n',
      '\t\tinstall.packages("progressr")'
    )
    return(invisible(NULL))
  }
  
  
  # Formatting pred_raster -----------------------------------------------------
  if(!is.null(alignment_raster)) {
    # Set resolution of alignment_raster
    res_alignment_raster <- terra::res(alignment_raster)
    agg_fact <- res_alignment_raster / grid_size
    alignment_raster <- terra::disagg(alignment_raster, fact = agg_fact)
    # Resample pred_raster in alignment_raster with the desired function
    pred_raster <- terra::resample(x = pred_raster, y = alignment_raster, method = raster_fun)
  } else {
    # if missing alignment_raster, aggregate pred_raster to fit grid_size with the desired function
    res_pred_raster <- terra::res(pred_raster)
    agg_fact <- grid_size / res_pred_raster
    pred_raster <- terra::aggregate(x = pred_raster, fact = agg_fact, method = raster_fun, na.rm = TRUE)
  }
  
  # Formatting dt_pred -----------------------------------------------------
  dt_pred <- as.data.table(pred_raster, xy = T)
  dt_pred[ , log_CHM := log(dt_pred[[3]])] #change mean_logCHM to a more general name ????
  
  # If 0s in pred_raster -> -Inf when log(), for now replaced with 0
  dt_pred$log_CHM <- ifelse(is.infinite(dt_pred$log_CHM), 0, dt_pred$log_CHM)
  
  # Predictions ----------------------------------------------------------------
  draw_size <- n_post_draws/n_cores
  draws_id_list <- split(1:n_post_draws, ceiling(seq_along(1:n_post_draws) / draw_size))
  
  
  df_map_pred <- progressr::with_progress({
    
    p <- progressr::progressor(along = draws_id_list)
    
    doFuture::`%dofuture%`(
      
      foreach::foreach(
        i = seq_along(draws_id_list),
        .combine = 'rbind',
        .multicombine = TRUE,
        .options.future = list(seed = TRUE)
      ),
      {
        res <- predict(
          fit_brms,
          newdata = dt_pred,
          summary = FALSE,
          draw_ids = draws_id_list[[i]]
        )
        
        p()  # increment progression bar
        res
      }
    )
  })
  
  # Adding median and sd posterior distributions to dt_pred
  dt_pred[, post_median_AGBD := apply(X = exp(df_map_pred), MARGIN = 2, FUN = mean)]
  dt_pred[, post_sd_AGBD := apply(X = exp(df_map_pred), MARGIN = 2, FUN = sd)]
  dt_pred[, post_cred_2.5_AGBD := apply(X = exp(df_map_pred), MARGIN = 2, FUN = quantile, probs = 0.025, na.rm=TRUE)]
  dt_pred[, post_cred_97.5_AGBD := apply(X = exp(df_map_pred), MARGIN = 2, FUN = quantile, probs = 0.975, na.rm=TRUE)]
  dt_pred[, log_CHM := NULL]
  
  # Display maps ---------------------------------------------------------------
  if(plot_maps) {
    plot_median <- ggplot(dt_pred, aes(x, y, fill = post_median_AGBD)) +
      geom_raster() +
      scale_fill_gradientn(colours = rev(terrain.colors(20))) +
      coord_fixed()+
      labs(fill = "Median of posterior\nAGBD distributions")
    print(plot_median)
    
    plot_sd <- ggplot(dt_pred, aes(x, y, fill = post_sd_AGBD)) +
      geom_raster() +
      scale_fill_gradientn(colours = rev(terrain.colors(20))) +
      coord_fixed()+
      labs(fill = "Sd of posterior\nAGBD distributions")
    print(plot_sd)
    
    plot_cv <- ggplot(dt_pred, aes(x, y, fill = post_sd_AGBD/post_median_AGBD)) +
      geom_raster() +
      scale_fill_gradientn(colours = rev(terrain.colors(20))) +
      coord_fixed()+
      labs(fill = "CV of posterior\nAGBD distributions") 
    print(plot_cv)
  }
  
  # return dt_pred in a raster format
  
  return(terra::rast(dt_pred, crs = terra::crs(pred_raster) ))
  
}
