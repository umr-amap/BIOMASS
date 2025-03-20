#' @importFrom utils globalVariables


if(getRversion() >= "2.15.1") {
  
  # common global variables
  utils::globalVariables(c(".",".I",".data",".N",".EACHI"))
  utils::globalVariables(c("x","y","x_rel","y_rel","x_proj","y_proj","i"))
  
  
  # correctTaxo
  utils::globalVariables(c("query","from"))
  
  # getTaxonomy
  utils::globalVariables(c("id"))
  
  # getWoodDensity
  utils::globalVariables(c("regionId","i.family","family","wd","wd.x","wd.y","taxo","meanWDsp","nIndsp","meanWD","meanWDgn",
                           "nInd","nIndgn","sdWD","sdWDsp","sdWDgn","levelWD",
                           "meanWDfm","nIndfm","sdWDfm","meanWDst","nIndst","sdWDst"))
  
  # predictHeight
  utils::globalVariables(c("H"))
  
  # computeE
  utils::globalVariables(c("RASTval","i.RASTval","submittedName","slice","Name_submitted","Overall_score","Name_matched","Accepted_name",
                           "..score","acceptedName","matchedName","outName","nameModified","genusCorrected","speciesCorrected"))
  
  # check_plot_coord function 
  utils::globalVariables(c("outlier","x_proj_mean","y_proj_mean","row_number","whatpoint","x_end","y_end","nRow",
                           "..rel_coord","..proj_coord","..longlat","is_in_plot","tree_shape","tree_label"))
  
  # divide_plot and subplot_summary functions
  utils::globalVariables(c("subplot_id","plot_id","subplot_ID","plot_ID","V1","st_crs","..col_names"))
  
  # summaryByPlot
  utils::globalVariables(c("indice_line","indice_col","Cred_2.5","Cred_97.5"))
  
  
}
