if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "query", "from", "submittedName", "nameSubmitted","slice", ".I",
    "..score", "matchedName", "outName", "nameModified", "scientificScore",
    "genusCorrected", "speciesCorrected", "acceptedName", "nameScientific",
    "Name_submitted", "Overall_score", "Name_matched", "Accepted_name",
    ".N", "."
  ))
}

##%######################################################%##
#                                                          #
####             ' Checking typos in names              ####
#                                                          #
##%######################################################%##

#'
#' This function corrects typos for a given taxonomic name using the Taxonomic Name Resolution Service (TNRS).
#'
#'
#' @details
#' This function create a file named correctTaxo.log (see Localisation), this file have the memory of all the previous requests, as
#' to avoid the replication of time-consuming server requests.
#'
#' By default, names are queried in batches of 500, with a 0.5s delay between each query. These values can be modified using options:
#' `options(BIOMASS.batch_size=500)` for batch size (max 1000), `options(BIOMASS.wait_delay=0.5)` for delay (in seconds).
#'
#'
#' @inheritSection cacheManager Localisation
#'
#'
#' @param genus Vector of genera to be checked. Alternatively, the whole species name (genus + species)
#'  or (genus + species + author) may be given (see example).
#' @param species (optional) Vector of species to be checked (same size as the genus vector).
#' @param score Score of the matching ( see https://tnrs.biendata.org/instructions ) below which corrections are discarded.
#' @param useCache logical. Whether or not use a cache to reduce online search of taxa names (NULL means use cache but clear it first)
#' @param verbose logical. If TRUE various messages are displayed during process
#' @param accepted logical. If TRUE accepted names will be returned instead of matched names. Cache will not be used as synonymy changes over time.
#'
#' @return The function returns a dataframe with the corrected (or not) genera and species.
#'
#' @references Boyle, B. et al. (2013).
#' _The taxonomic name resolution service: An online tool for automated standardization of plant names_. BMC bioinformatics, 14, 1. doi:10.1186/1471-2105-14-16
#'
#' @author Ariane TANGUY, Arthur PERE, Maxime REJOU-MECHAIN, Guillaume CORNU
#'
#' @examples
#' \donttest{
#' correctTaxo(genus = "Astrocarium", species = "standleanum")
#' correctTaxo(genus = "Astrocarium standleanum")
#' }
#'
#' @export
#' @importFrom data.table tstrsplit := data.table setkey chmatch fread fwrite setDF setDT rbindlist
#' @importFrom rappdirs user_data_dir
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils head
#'
correctTaxo <- function(genus, species = NULL, score = 0.5, useCache = FALSE, verbose = TRUE, accepted=FALSE) {

  # Check if package httr2 is available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    warning(
      'To use this function, you must install the "httr2" library \n\n',
      '\t\tinstall.packages("httr2")'
    )
    return(invisible(NULL))
  }

  # check parameters -------------------------------------------------

  WAIT_DELAY <- getOption("BIOMASS.wait_delay", 0.5) # delay between requests to tnrs (to reduce load on server)
  BATCH_SIZE <- min(getOption("BIOMASS.batch_size", 500), 1000) # number of taxa sought per request to tnrs (max 1000)

  if (is.logical(useCache) && !useCache) {
    message("Using useCache=TRUE is recommended to reduce online search time for the next query")
  }

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
    species[is.na(genus)] <- NA
  }

  if(accepted && !is.null(useCache) && useCache) {
    warning("Cache cannot be used if accepted names are required! I will ignore it")
    useCache <- FALSE
  }

  checkURL <- function(url) {
    tryCatch(
      {
        req <- httr2::request(url)
        req <- httr2::req_method(req, "HEAD")
        httr2::req_perform(req)
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
  }

  if(!checkURL("https://tnrsapi.xyz")) {
    warning("Sorry there is no internet connexion or the tnrs site is unreachable!", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  # sub-function definition -------------------------------------------------

  # split x always returning count columns (padding with NA)
  tstrsplit_NA <- function(x, pattern = " ", count = 2) {
    # NOTE extraneous columns ignored maybe better paste them together
    split <- utils::head(tstrsplit(x, pattern), count)

    # pad with NA
    if (length(split) < count) {
      split <- c(split, rep(NA_character_, count - length(split)))
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
    userTaxo[!is.na(genus) & !is.na(species), query := paste(query, species)]
  }

  # If there is an empty genus
  userTaxo[genus == "", ":="(genus = NA_character_, species = NA_character_, query = NA_character_)]

  # If there is empty species
  userTaxo[species == "", ":="(species = NA_character_, query = gsub(" ", "", query))]

  # get unique values
  qryTaxo <- unique(userTaxo[!is.na(query)])

  # get cached taxonomic corrections if needed -------------------------------------------------

  cachedTaxo <- NULL

  if (is.null(useCache) || useCache) {
    cachePath <- cacheManager("correctTaxo.log")

    if (file.exists(cachePath)) {

      # should we remove cache ?
      if (is.null(useCache)) {
        file.remove(cachePath)
        useCache <- TRUE
      } else {
        if (verbose) {
          message("Cache last modification time : ", as.character.POSIXt(file.info(cachePath)["mtime"]))
        }
        cachedTaxo <- fread(file = cachePath)
        cachedTaxo[, from := "cache"]

        # if not the right format then ignore it!
        if (!("submittedName" %in% names(cachedTaxo))) {
          cachedTaxo <- NULL
        }
      }
    } else if (is.null(useCache)) {
      useCache <- TRUE
    }
  }

  # init cachedTaxo with empty data
  if (is.null(cachedTaxo)) {
    cachedTaxo <- data.table(
      submittedName = character(0), score = numeric(0), matchedName = character(0), from = character(0),
      acceptedName = character(0)
    )
  }

  # identify taxo not present in cache
  missingTaxo <- qryTaxo[!cachedTaxo[, .(submittedName)], on = c(query = "submittedName")]

  # query tnrs for missing taxo if any
  queriedTaxo <- NULL
  if (nrow(missingTaxo)) {

    # split missing taxo in chunks of 30
    slices <- split(missingTaxo[, slice := ceiling(.I / BATCH_SIZE)], by = "slice", keep.by = TRUE)

    # for each slice of queries
    if (verbose) {
      pb <- utils::txtProgressBar(style = 3)
    }
    queriedTaxo <- rbindlist(lapply(slices, function(slice) {

      req <- httr2::request("https://tnrsapi.xyz/tnrs_api.php")
      req <- httr2::req_headers(req,
        'Accept' = 'application/json',
        'Content-Type' = "application/json",
        'charset' = "UTF-8"
      )
      req <- httr2::req_body_json(req, list(
        opts = list(
          class = jsonlite::unbox("wfo"),
          mode = jsonlite::unbox("resolve"),
          matches = jsonlite::unbox("best")
        ),
        data = unname(data.frame(seq_along(slice$query),slice$query))
      ))

      req <- httr2::req_error(req, function(response) FALSE)
      qryResult <- httr2::req_perform(req)

      if (httr2::resp_is_error(qryResult)) {
        message("There appears to be a problem reaching the tnrs API.")
        return(invisible(NULL))
      }

      # parse answer from tnrs
      answer <- setDT(httr2::resp_body_json(qryResult, simplifyVector = TRUE))

      # recode empty strings as NA
      answer[, names(answer) := lapply(.SD, function(x) {
        x[x==""]<-NA
        x
      })]

      # format result
      answer <- answer[, .(
        submittedName = Name_submitted,
        score = as.numeric(Overall_score),
        matchedName = Name_matched,
        from = "iplant_tnrs",
        acceptedName = Accepted_name
      )]

      if (verbose) {
        utils::setTxtProgressBar(pb, slice$slice[1] / length(slices))
      }

      Sys.sleep(WAIT_DELAY)

      answer
    }))
    if (verbose) {
      close(pb)
    }
  }

  # build reference taxonomy from cached and queried ones
  fullTaxo <- rbindlist(list(queriedTaxo, cachedTaxo), fill = TRUE)

  # inject taxo names in original (user provided) taxonomy
  if(accepted) {
    userTaxo[fullTaxo, on = c(query = "submittedName"), `:=`(
      outName = ifelse(score >= ..score, acceptedName, query),
      nameModified = ifelse(score >= ..score, "TRUE", "NoMatch(low_score)"),
      from = from
    )]

  } else {
    userTaxo[fullTaxo, on = c(query = "submittedName"), `:=`(
      outName = ifelse(score >= ..score, matchedName, query),
      nameModified = ifelse(score >= ..score, "TRUE", "NoMatch(low_score)"),
      from = from
    )]
  }

  # if nothing changed tell it
  userTaxo[
    !is.na(outName) & (outName == query) & (nameModified != "NoMatch(low_score)"),
    nameModified := "FALSE"
  ]

  # split name
  userTaxo[, c("genusCorrected", "speciesCorrected") := tstrsplit_NA(outName)]

  # If genera or species not found by TNRS
  # Genera
  userTaxo[
    ((nameModified == "TRUE") | is.na(nameModified)) & is.na(genusCorrected) & !is.na(genus),
    c("genusCorrected", "nameModified") := list(genus, "TaxaNotFound")
  ]

  # Species
  userTaxo[
    (nameModified %in% c("TRUE", "TaxaNotFound")| is.na(nameModified)) & is.na(speciesCorrected) & !is.na(species),
    `:=`(
      speciesCorrected = species,
      nameModified = ifelse(nameModified == "TRUE", "SpNotFound", nameModified)
    )
  ]

  # cache full taxonomy for further use
  if (useCache && !is.null(queriedTaxo)) {

    # complete taxo with matched names and accepted names
    matchedTaxo <- unique(fullTaxo[submittedName != matchedName],
      by = "matchedName"
    )[, `:=`(
      submittedName = matchedName,
      score = 1
    )]

    acceptedTaxo <- unique(fullTaxo[(submittedName != acceptedName) & (acceptedName != matchedName)],
      by = "acceptedName"
    )[, `:=`(
      submittedName = acceptedName,
      matchedName = acceptedName,
      score = 1
    )]

    fullTaxo <- unique(rbindlist(list(fullTaxo, matchedTaxo, acceptedTaxo))[submittedName != ""])

    # write cache
    fwrite(fullTaxo[order(submittedName), -"from"], file = cachePath)
    if (verbose) {
      message("Cache updated")
    }
  }

  # stats
  if (verbose) {
    stats <- userTaxo[, by = from, .N]
    message("Source ", paste(sprintf("%s:%d", stats$from, stats$N), collapse = ", "))

    stats <- userTaxo[, by = nameModified, .N]
    message("Corrections ", paste(sprintf("%s:%d", stats$nameModified, stats$N), collapse = ", "))
  }

  # return corrected taxo
  data.frame(userTaxo[, .(genusCorrected, speciesCorrected, nameModified)])
}
