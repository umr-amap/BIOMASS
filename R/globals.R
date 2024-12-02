#' @importFrom utils globalVariables


if(getRversion() >= "2.15.1") {
  # modelHD function 
  utils::globalVariables(c("x","y"))
  
  # check_plot_coord function 
  utils::globalVariables(c("x_proj","y_proj","outlier","Xmean","Ymean","row_number","whatpoint","Xend","Yend"))
  
  # checkPlotCoord function 
  utils::globalVariables("nRow")
  
  # divide_plot function 
  utils::globalVariables(c("x_rel","y_rel","subplot_id","plot_id"))

  # subplot_summary function 
  utils::globalVariables(c("subplot_id","plot_id",".data"))
}