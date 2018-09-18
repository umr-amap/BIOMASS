rm(list = ls())
library(BIOMASS)


# data used for the function
data("KarnatakaForest")

genus = KarnatakaForest$genus
species = KarnatakaForest$species

score = 0.5






correctTaxo1 = function( genus, species = NULL, score = 0.5 ){
  
  ########### preparation of log file
  
  sep = ifelse(length(grep( "win", Sys.info()["sysname"], ignore.case = T )) != 0, "\\", "/")
  path = paste(rappdirs::user_data_dir("BIOMASS"), "correctTaxo.log", sep = sep)
  file_exist = T
  
  if( !dir.exists( rappdirs::user_data_dir("BIOMASS")) ){
    file_exist = F
    dir.create(rappdirs::user_data_dir("BIOMASS"))
  }
  
  if( !file.exists(path) ){
    file_exist = F
    file.create(path)
    write(paste("query", "outname", "nameModified", "genusCorrected", "speciesCorrected", sep = "\t"), file = path)
  } else {
    taxo_already_have = fread(file = path)
    setkey(taxo_already_have, query)
  }
  
  
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
  query = oriData[!(is.na(genus)&is.na(species)), query, keyby = query][, 2]
  setkey(oriData, query)
  
  if ( nrow(query) == 0 )
    stop("Please supply at least one name", call. = FALSE)
  
  # Comparison between the taxo we already have and the taxo we want. We would have the unique taxo between the the two
  if (file_exist){
    if (nrow(taxo_already_have) != 0)
      query = query[!taxo_already_have, on = "query"]
  }
  
  # End the function if we already have all the data needed
  if ( nrow(query) == 0 ){
    out = merge(oriData, taxo_already_have, all.x = T)
    return(out[order(id), c("genusCorrected", "speciesCorrected", "nameModified")])
  }
  
  
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
  
  
  
  
  query[, c("genusCorrected", "speciesCorrected") := tstrsplit(outname, " ", keep = 1:2)]
  query[, c("genus", "species") := tstrsplit(query, " ", keep = 1:2)]
  
  # # If genera or species not found by TNRS
  # Genera
  filt <- query$nameModified==TRUE & is.na(query$genusCorrected) & !is.na(query$genus)
  query[filt, c("genusCorrected", "nameModified") := list(genus, "TaxaNotFound")]
  
  # Species
  filt <- (query$nameModified==TRUE | query$nameModified=="TaxaNotFound") & is.na(query$speciesCorrected) & !is.na(query$species)
  query[filt, speciesCorrected := species]
  query[filt & nameModified != "TaxaNotFound", nameModified := "SpNotFound"]
  
  
  
  ########### write all the new data on the log file created
  fwrite(query[, .(outname, nameModified, genusCorrected, speciesCorrected), by=query], 
         file = path, col.names = F, sep = "\t", append = T)
  
  
  
  ########## merge the data for the return
  out = merge(oriData, query, all.x = T)[ order(id), .(id, nameModified, query, genusCorrected, speciesCorrected)]
  
  if(exists("taxo_already_have")){
    setkey(out, query)
    out[taxo_already_have, 
        ':='(nameModified = i.nameModified, genusCorrected = i.genusCorrected, speciesCorrected = i.speciesCorrected)]
  }
  
  return(out[order(id), .(genusCorrected, speciesCorrected, nameModified)])
  
}


