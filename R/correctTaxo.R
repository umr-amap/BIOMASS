if(getRversion()>="2.15.1") {
  utils::globalVariables(c(
    "query","from","submittedName","slice",".I",
    "..score","matchedName","outName","nameModified",
    "genusCorrected","speciesCorrected","acceptedName",
    ".N","."))
}

#' Checking typos in names
#'
#' This function corrects typos for a given taxonomic name using the Taxonomic
#' Name Resolution Service (TNRS) via the Taxosaurus interface.
#' This function has been adapted from the \code{tnrs} function from the taxize package (\code{\link[taxize]{tnrs}}).
#'
#'
#' @details
#' This function create a file named correctTaxo.log (see Localisation), this file have the memory of all the previous requests, as
#' to avoid the replication of time-consuming servor requests.
#'
#'
#' @inheritSection folderControl Localisation
#'
#'
#' @param genus Vector of genera to be checked. Alternatively, the whole species name (genus + species)
#'  or (genus + species + author) may be given (see example).
#' @param species (optional) Vector of species to be checked (same size as the genus vector).
#' @param score Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match) below which corrections are discarded.
#' @param useCache logical. Whether or not use a cache to reduce online search of taxa names (NULL means use cache but clear it first)
#' @param verbose logical. If TRUE various messages are displayed during process
#'
#' @return The function returns a dataframe with the corrected (or not) genera and species.
#'
#' @references Boyle, B. et al. (2013). \emph{The taxonomic name resolution service: An online tool for automated
#' standardization of plant names}. BMC bioinformatics, 14, 1.
#' @references Chamberlain, S. A. and Szocs, E. (2013). \emph{taxize: taxonomic search and retrieval in R}. F1000Research, 2.
#'
#' @author Ariane TANGUY, Arthur PERE, Maxime REJOU-MECHAIN, Guillaume CORNU
#'
#' @examples
#' \dontrun{correctTaxo(genus = "Astrocarium", species="standleanum")}
#' \dontrun{correctTaxo(genus = "Astrocarium standleanum")}
#'
#' @export
#' @importFrom data.table tstrsplit := data.table setkey chmatch fread fwrite setDF rbindlist
#' @importFrom rappdirs user_data_dir
#' @importFrom jsonlite fromJSON
#' @importFrom utils head
#'
correctTaxo <- function(genus, species = NULL, score = 0.5, useCache=FALSE, verbose=FALSE) {
  WAIT_DELAY <- 0.5 # delay between requests to taxosaurus (to reduce load on server)
  SLICE_SIZE <- 30 # number of taxa sought per request to taxosaurus

  # check parameters -------------------------------------------------

  if (all(is.na(genus))) {
    stop("Please supply at least one name for genus")
  }

  if (!is.null(species)) {
    if (all(is.na(species))) {
      stop("Please supply at least one name for species")
    }
    if (length(genus) != length(species)) {
      stop("You should provide two vectors of genera and species of the same length")
    }
    if(any(is.na(genus) & !is.na(species))) {
      stop("Species must have a genus")
    }
  }

  # Check if package httr is available
  if (!requireNamespace("httr", quietly = T)) {
    stop(
      'To use this function, you must install the "httr" library \n\n',
      '\t\tinstall.packages("httr")'
    )
  }

  # sub-function definition -------------------------------------------------

  # split x always returning count columns (padding with NA)
  tstrsplit_NA <- function(x, pattern = " ", count=2) {
    # NOTE extraneous columns ignored maybe better paste them together
    split <- utils::head(tstrsplit(x, pattern), count)
    
    # pad with NA
    if (length(split) < count) {
      split <- c(split, rep(NA_character_, count-length(split)))
    }
    split
  }

  # Data preparation --------------------------------------------------------

  genus <- as.character(genus)

  if (is.null(species)) {
    
    # Create a dataframe with the original values
    userTaxo <- data.table(
      genus = NA_character_,
      species = NA_character_,
      query = genus
    )
    # split genus (query)
    userTaxo[, c("genus", "species") := tstrsplit_NA(query)]
  } else {
    
    species <- as.character(species)
    
    # Create a dataframe with the original values
    userTaxo <- data.table(
      genus = genus, 
      species = species,
      query = genus
    )
    # species can be NA so handle it with care when pasting
    userTaxo[!is.na(genus) & !is.na(species), query:=paste(query, species)]
  }

  # get unique values
  qryTaxo <- unique(userTaxo[!is.na(query)])
  
  # get cached taxonomic corrections if needed -------------------------------------------------
  
  cachedTaxo <- NULL
  
  if(is.null(useCache) || useCache) {
    cachePath <- folderControl(correctTaxo = T)
    
    if(file.exists(cachePath)) {
      
      # should we remove cache ?
      if(is.null(useCache)) {
        file.remove(cachePath)
        useCache <- TRUE
      } else {
        
        # TODO display file date
        cachedTaxo <- fread(file = cachePath)
        cachedTaxo[, from:="cache"]
        
        # if not the right format then ignore it!
        if(!("submittedName" %in% names(cachedTaxo)))
          cachedTaxo <- NULL
      }
    }
  }
  
  # init cachedTaxo with empty data
  if(is.null(cachedTaxo))
    cachedTaxo <-  data.table(submittedName=character(0),score=numeric(0),matchedName=character(0),from=character(0))  
  
  # identify taxo not present in cache
  missingTaxo <- qryTaxo[!cachedTaxo[,.(submittedName)], on=c(query="submittedName")]

  # query taxosaurus for missing taxo if any
  queriedTaxo <- NULL
  if(nrow(missingTaxo)) {
    
    # split missing taxo in chunks of 30
    slices <- split(missingTaxo[, slice:=ceiling(.I/SLICE_SIZE)], by="slice", keep.by=TRUE)
  
    # for each slice of queries
    if(verbose)
      message("Progress 0%", appendLF = FALSE)
    queriedTaxo <- rbindlist(lapply(slices, function(slice) {
      
      # build query
      baseURL <- "http://taxosaurus.org/submit"
      qry <- paste(gsub(" ", "+", slice$query, fixed = T), collapse = "%0A")
      args <- list(query = qry)
      
      # send query
      qryResult <- httr::GET(baseURL, query = args)
      
      # check for errors
      if(httr::http_error(qryResult))
        httr::stop_for_status(qryResult, "connect to taxosaurus service. Retry maybe later")
      
      # wait for response
      retrieveURL <- qryResult$url
      repeat {
        
        # be polite with server
        Sys.sleep(WAIT_DELAY)
        
        # fetch answer
        qryResult <- httr::GET(retrieveURL)
        
        # normal waiting bahaviour is redirecting to self with 302 status
        # if not then break from waiting loop
        if(httr::status_code(qryResult) != 302) 
          break
      }
      
      # check for errors
      if(httr::http_error(qryResult))
        httr::stop_for_status(qryResult,"get answer from taxosaurus service. Retry maybe later")
      
      # parse answer from taxosaurus
      answer <- jsonlite::fromJSON(httr::content(qryResult, "text", encoding = "UTF-8"), FALSE)

      # WARNING when no match is found, taxosaurus does not return an answer
      
      # do we have answers ?
      if(length(answer$names)) {
        
        # function to preprocess match      
        flatten <- function(match) {
          match$annotations <- paste(sprintf("%s(%s)", names(match$annotations), match$annotations), collapse=", ")
          match$score <- as.numeric(match$score)
          match
        }
        
        # get first match (highest score) for each submitted name
        result <- rbindlist(lapply(answer$names, function(name) c(
          submittedName = name$submittedName,
          flatten(name$matches[[1]]),
          from = "taxosaurus"
        )))
        
      } else {
        
        # nothing found ! Pathological case -> return empty answer
        result <- data.table(submittedName=character(0), score=numeric(0), matchedName=character(0), from=character(0))
      }
      
      # handle not founds (as taxosaurus does not give answers for them)
      result <- result[slice[,.(query)], on=c(submittedName="query"), nomatch=NA]
      result[is.na(from), `:=`(
        score = 1, # I'm positive. It does not exist !
        matchedName = NA_character_,
        from = "taxosaurus(not_found)"
      )]
      
      if(verbose)
        message("\rProgress ",ceiling(100*slice$slice[1]/length(slices)),"%", appendLF = FALSE)
      
      result
    }))
    if(verbose)
      message("\nDone")
  }
  
  # build reference taxonomy from cached and queried ones
  fullTaxo <- rbindlist(list(queriedTaxo, cachedTaxo), fill=TRUE)
  
  # inject taxo names in original (user provided) taxonomy
  userTaxo[fullTaxo, on=c(query="submittedName"), `:=`(
    outName      = ifelse(score>=..score, matchedName, query), 
    nameModified = ifelse(score>=..score, "TRUE", "NoMatch(low_score)"),
    from = from
  )]
  
  # if nothing changed tell it
  userTaxo[!is.na(outName) & (outName==query) & (nameModified!="NoMatch(low_score)"),
    nameModified := "FALSE"
  ]
  
  # split name
  userTaxo[, c("genusCorrected", "speciesCorrected") := tstrsplit_NA(outName)]
  
  # If genera or species not found by TNRS
  # Genera
  userTaxo[(nameModified=="TRUE") & is.na(genusCorrected) & !is.na(genus), 
    c("genusCorrected", "nameModified") := list(genus, "TaxaNotFound")
  ]
  
  # Species
  userTaxo[(nameModified %in% c("TRUE","TaxaNotFound")) & is.na(speciesCorrected) & !is.na(species),
    `:=`(
      speciesCorrected = species,
      nameModified = ifelse(nameModified=="TRUE", "SpNotFound", nameModified)
  )]
  
  # cache full taxonomy for further use
  if(useCache && nrow(queriedTaxo)) {
    
    # complete taxo with matched names and accepted names
    matchedTaxo <- unique(fullTaxo[submittedName!=matchedName], by="matchedName")[
      ,`:=`(submittedName=matchedName, score=1)]
    
    acceptedTaxo <- unique(fullTaxo[(submittedName!=acceptedName) & (acceptedName!=matchedName)], by="acceptedName")[
      ,`:=`(submittedName=acceptedName, matchedName=acceptedName, score=1)]
    
    fullTaxo <- rbindlist(list(fullTaxo,matchedTaxo,acceptedTaxo))[submittedName!=""]
    
    # write cache
    fwrite(fullTaxo[order(submittedName),-"from"], file=cachePath)
    if(verbose)
      message("Cache updated")
  }
  
  # stats
  if(verbose) {
    stats <- userTaxo[,by=from,.N]
    message("Source ",paste(sprintf("%s:%d",stats$from,stats$N),collapse=", "))
    
    stats <- userTaxo[,by=nameModified,.N]
    message("Corrections ",paste(sprintf("%s:%d",stats$nameModified,stats$N),collapse=", "))
  }
  
  # return corrected taxo
  data.frame(userTaxo[,.(genusCorrected, speciesCorrected, nameModified)])
}
