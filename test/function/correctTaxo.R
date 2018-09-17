rm(list = ls())
library(BIOMASS)


# data used for the function
data("KarnatakaForest")

genus = KarnatakaForest$genus[1:100]
species = KarnatakaForest$species[1:100]

score = 0.5










correctTaxo1 = function( genus, species = NULL, score = 0.5 ){
  
  ########### Data preparation
  
  options(stringsAsFactors = F)
  
  genus = as.character(genus)
  
  require(data.table, quietly = T)
  
  if(!is.null(species))
  {
    species = as.character(species)
    
    # Check the length of the inputs
    if(length(genus) != length(species))
      stop("You should provide two vectors of genus and species of the same length")
    
    # Create a dataframe with the original values
    oriData <- data.table(genus = genus, species = species, 
                          query = paste(genus, species), id = 1:length(genus))
    
    
    
  }else{
    
    # Create a dataframe with the original values
    oriData <- data.table(genus = sapply(strsplit(genus," "),"[",1), 
                          species = sapply(strsplit(genus," "),"[",2),
                          query = genus, id = 1:length(genus))
    
  }
  
  # Regroup unique query and filter the column species and genus if they are NA in the same time
  query = oriData[!(is.na(genus)&is.na(species)), query, by = query][,2]
  
  setkey(oriData, query)
  
  if ( nrow(query) == 0 )
    stop("Please supply at least one name", call. = FALSE)
  
  getpost <- "get"
  if(nrow(query) > 50)
    getpost <- "post"
  
  
  
  # If there is too much data, better submit it in separated queries
  splitby <- 30
  slicedQu <- rep(1, nrow(query))
  if ( nrow(query) > splitby ){
    query[, slicedQu := rep(1:ceiling(length(query) / splitby), each = splitby)[1:length(query)] ]
  } else {
    query[, slicedQu := 1]
  }
  
  
  
  ########### sending and retrive the data from taxosaurus
  
  tc <- function(l) Filter(Negate(is.null), l)
  con_utf8 <- function(x) httr::content(x, "text", encoding = "UTF-8")
  
  url <- "http://taxosaurus.org/submit"
  
  setkey(query, query)
  query[, c("matchedName", "score1") := list(as.character(NA), as.double(0))]
  for(s in query[, unique(slicedQu)])
  {
    x <- query[slicedQu == s, query]
    
    if(getpost == "get") 
    {
      query2 <- paste(gsub(" ", "+", x, fixed = T), collapse = "%0A")
      args <- tc(list(query = query2))
      out <- httr::GET(url, query = args)
      retrieve <- out$url
      
    } else {
      
      loc <- tempfile(fileext = ".txt")
      write.table(data.frame(x), file = loc, col.names = FALSE, row.names = FALSE)
      args <- tc(list(file = httr::upload_file(loc), source = "iPlant_TNRS"))
      out <- httr::POST(url, body = args, httr::config(followlocation = 0))
      tt <- con_utf8(out)
      message <- jsonlite::fromJSON(tt, FALSE)[["message"]]
      retrieve <- jsonlite::fromJSON(tt, FALSE)[["uri"]]
    }
    
    print(paste("Calling", retrieve))
    
    timeout <- "wait"
    while (timeout == "wait") 
    {
      ss <- httr::GET(retrieve)
      output <- jsonlite::fromJSON(con_utf8(ss), FALSE)
      if(!grepl("is still being processed", output["message"]) == TRUE) 
        timeout <- "done"
    }
    out <- tc(output$names)
    
    if(length(out)>0){
      submittedName = sapply(out, function(x) x$submittedName)
      receiveData = t( sapply(out, function(x) c( x[[2]][[1]]$matchedName, x[[2]][[1]]$score) ) )
      
      # Remove some parasite characters
      submittedName <- gsub("\"", "", submittedName)
      submittedName <- gsub("\r", "", submittedName)
      receiveData[,1] <- gsub("\"", "", receiveData[,1])
      receiveData[,1] <- gsub("\r", "", receiveData[,1])
      
      
      query[submittedName, ':='(matchedName = receiveData[,1], score1 = as.double(receiveData[, 2]))]
    }
  }
  
  ########### data analysis
  
  query[ , c( "nameModified", "outname" ) := list(as.character(TRUE), as.character(NA) )]
  query[, slicedQu := NULL]

  # If score ok
  query[score1 >= score, outname := matchedName]
  
  # If score non ok
  query[score1 < score, c("outname", "nameModified") := list(query, "NoMatch(low_score)")]

  # If no modified name value of nameModified as False
  query[!is.na(outname) & outname == query & nameModified != "NoMatch(low_score)", nameModified := as.character(FALSE)]
  
  
  out = merge(oriData, query, all.x = T)[ order(id), .(id, genus, species, outname, nameModified)]

  out[, c("genusCorrected", "speciesCorrected") := tstrsplit(outname, " ", keep = 1:2)]
  
  # # If genera or species not found by TNRS
  # Genera
  filt <- out$nameModified==TRUE & is.na(out$genusCorrected) & !is.na(out$genus)
  out[filt, c("genusCorrected", "nameModified") := list(genus, "TaxaNotFound")]

  # Species
  filt <- (out$nameModified==TRUE | out$nameModified=="TaxaNotFound") & is.na(out$speciesCorrected) & !is.na(species)
  out[filt, speciesCorrected := species]
  out[filt & nameModified != "TaxaNotFound", nameModified := "SpNotFound"]

  
  return(out[, c("genusCorrected", "speciesCorrected", "nameModified")])
  
}


