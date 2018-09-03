getTaxonomy <- function(genus, findOrder = FALSE)
{    
  ### Find the family (and the order) of a vector of genus
  
  ################## 1. Retrieve the Family 
  
  # Load taxonomical data (sourced from Angiosperm Phylogeny Website, http://www.mobot.org/MOBOT/research/APweb/)
  genusFamily <- NULL
  data(genusFamily, envir = environment())
  
  # Create ids
  inputGenus <- data.frame(id = 1:length(genus), inputGenus = as.character(genus), 
                           stringsAsFactors = FALSE)
  
  # Merge the input genera with the genus family table
  genusFam <- unique(merge(inputGenus, genusFamily, by.x = "inputGenus", by.y = "genus", all.x = TRUE))
    
  # Sort data by id
  genusFam <- genusFam[order(genusFam$id),]

  ################## 2. Retrieve the Order 
  
  if(findOrder == TRUE)
  {
	apgFamilies <- NULL
    data(apgFamilies, envir = environment())
    
    tmp <- unique(genusFam[, c("inputGenus", "family")])
    tmpOrder <- unique(merge(tmp, apgFamilies, by.x = "family", by.y = "famAPG", all.x = TRUE))
    
    for(f in unique(tmpOrder$family))
      genusFam$order[genusFam$family %in% f] <- unique(tmpOrder$order[tmpOrder$family %in% f])
  }
  
  genusFam <- genusFam[order(genusFam$id), ]
  genusFam$id <- NULL
  return(genusFam)  
}
