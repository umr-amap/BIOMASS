#' Correct and match taxonomic names to the World Flora Online database
#'
#' @param x vector of taxonomic names
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 0 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param useCache logical, if TRUE use cached values in
#'     `options("wfo.api_uri")` preferentially, to reduce the number of API
#'     calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param raw logical, if TRUE raw a nested list is returned, otherwise a
#'     dataframe
#'
#' @return data.frame containing taxonomic name information with rows matching
#'     names in `x`, or a list containing unique values in `x` if raw = TRUE
#' 
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom httr2 request req_method req_perform
#'
#' @examples
#' x <- c("Burkea africana", "Julbernardia paniculata", "Fabaceae", 
#'   "Indet indet", "Brachystegia")
#' correctTaxo(x)
#' correctTaxo(x, raw = TRUE)
#' correctTaxo(x, fallbackToGenus = TRUE)
#' correctTaxo(x, interactive = FALSE)
#'
correctTaxo <- function(x, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = FALSE, fuzzyNameParts = 0, interactive = TRUE, 
  useCache = FALSE, useAPI = TRUE, raw = FALSE) {

  if (!useCache & !useAPI) {
    stop("Either useCache or useAPI must be TRUE")
  }

  # Define function to check URL
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

  # Check if WFO API is reachable 
  if (!checkURL(options("wfo.api_uri")$wfo.api_uri)) {
    w <- paste("WFO API unreachable:", options("wfo.api_uri")$wfo.api_uri)
    if (useCache) {
      warning(w, "\nOnly cached names will be filled")
      useAPI <- FALSE
    } else {
      stop("\n", w, " and useCache = FALSE, Exiting ...")
    }
  }

  # Extract unique names 
  xun <- sort(unique(x))
  xlen <- length(xun)

  # For each unique taxonomic name 
  resp_list <- lapply(seq_along(xun), function(i) {
    taxon <- xun[i]

    cat(sprintf("%i of %i:\t%s\n", i, xlen, taxon))

    # Submit API query
    c(list(submitted_name = taxon), 
      matchWFOName(taxon, 
        fallbackToGenus = fallbackToGenus, 
        checkRank = checkRank,
        checkHomonyms = checkHomonyms,
        fuzzyNameParts = fuzzyNameParts,
        useCache = useCache,
        useAPI = useAPI,
        interactive = interactive))
  })

  # Define helper function to convert NULL values to NA
  null2na <- function(x) {
    if (is.null(x)) {
      NA_character_ 
    } else { 
      x
    }
  }

  if (raw) {
    # Return raw list output
    out <- resp_list
  } else {
    # Create formatted dataframe
    out <- data.table::rbindlist(lapply(resp_list, function(i) { 
      if ("id" %in% names(i)) {
        data.frame(
          taxon_name_subm = null2na(i$submitted_name),
          method = null2na(i$method),
          fallbackToGenus = null2na(i$fallbackToGenus),
          checkRank = null2na(i$checkRank),
          checkHomonyms = null2na(i$checkHomonyms),
          fuzzyNameParts = null2na(i$fuzzyNameParts),
          taxon_wfo_syn = null2na(i$id),
          taxon_name_syn = null2na(i$fullNameStringNoAuthorsPlain),
          taxon_auth_syn = null2na(i$authorsString),
          taxon_stat_syn = null2na(i$nomenclaturalStatus),
          taxon_role_syn = null2na(i$role),
          taxon_rank_syn = null2na(i$rank),
          taxon_path_syn = null2na(i$wfoPath),
          taxon_wfo_acc = null2na(i$currentPreferredUsage$hasName$id),
          taxon_name_acc = null2na(i$currentPreferredUsage$hasName$fullNameStringNoAuthorsPlain),
          taxon_auth_acc = null2na(i$currentPreferredUsage$hasName$authorsString),
          taxon_stat_acc = null2na(i$currentPreferredUsage$hasName$nomenclaturalStatus),
          taxon_role_acc = null2na(i$currentPreferredUsage$hasName$role),
          taxon_rank_acc = null2na(i$currentPreferredUsage$hasName$rank),
          taxon_path_acc = null2na(i$currentPreferredUsage$hasName$wfoPath))
      } else { 
        data.frame(
          taxon_name_subm = i$submitted_name,
          method = i$method,
          fallbackToGenus = i$fallbackToGenus,
          checkRank = i$checkRank,
          checkHomonyms = i$checkHomonyms,
          fuzzyNameParts = i$fuzzyNameParts,
          taxon_wfo_syn = NA_character_,
          taxon_name_syn = NA_character_,
          taxon_auth_syn = NA_character_,
          taxon_stat_syn = NA_character_,
          taxon_role_syn = NA_character_,
          taxon_rank_syn = NA_character_,
          taxon_path_syn = NA_character_,
          taxon_wfo_acc = NA_character_,
          taxon_name_acc = NA_character_,
          taxon_auth_acc = NA_character_,
          taxon_stat_acc = NA_character_,
          taxon_role_acc = NA_character_,
          taxon_rank_acc = NA_character_,
          taxon_path_acc = NA_character_)
      }
    }))

    # Match row order of dataframe to x
    out <- out[match(x, out$taxon_name_subm),]
  }

  # Return
  return(out)
}

#' Manually pick a taxonomic name from a list returned by the WFO GraphQL API
#'
#' @param x original taxonomic name searched by `callWFOAPI()`
#' @param cand list of candidate taxa returned by `callWFOAPI()`
#' @param offset initial index value used internally by pager, controls index
#'     of page start
#' @param offset index value used internally by pager, controls page length
#'
#' @return list containing information of matched taxonomic name
#' 
#' @examples
#' x <- "Burkea af"
#' resp <- callWFOAPI(x, query_taxonNameMatch())
#' pickWFOName(x, resp$data$taxonNameMatch$candidates)
#' 
#' @export
#' 
pickWFOName <- function(x, cand, offset = 0, page_size = 10) {

  # If no candidates, SKIP
  if (length(cand) == 0) {
    cat(sprintf("No candidates, skipping: %s\n", x))
    match <- list(method = "EMPTY")
    return(match)
  }

  # Set up number of candidates per page
  start_page <- offset + 1
  end_page <- start_page + page_size -1
  if (end_page > length(cand)) {
    end_page <- length(cand)
  }

  # Create header
  cat("\n\n--- Pick a name ---")
  cat(sprintf("\nMatching string:\t%s\n", x))

  # List candidates
  for (i in start_page:end_page) {
    cat(
      sprintf(
        "%-4s%s\t%s\t%s\t%s\n",
        i,
        cand[[i]]$id,
        cand[[i]]$fullNameStringPlain,
        cand[[i]]$role,
        cand[[i]]$wfoPath
      )
    )
  }

  valid <- FALSE
  while (!valid) {

    # Create footer
    prompt <- paste(
      "Enter a number to pick a row from the list,",
      "a valid WFO ID,", 
      "'N' for the next page,", 
      "'P' for the previous page,",
      "'S' to skip this name: ")

    # Prompt the user for input
    input <- trimws(readline(prompt))
    input_num <- suppressWarnings(as.numeric(input))

    # Valid numeric selection
    if (!is.na(input_num) && input_num > 0 && input_num <= length(cand)) {
      match <- cand[[input_num]]
      match$method <- "MANUAL"
      valid <- TRUE
    } else if (grepl("^wfo-[0-9]{10}$", tolower(trimws(input)))) {
      input <- tolower(trimws(input))
      match <- callWFOAPI(input, query_taxonNameById())$data$taxonNameById
      match$method <- "MANUAL"
      valid <- TRUE
    } else if (tolower(input) == "n") {
      if (end_page < length(cand)) {
        return(pickWFOName(x, cand, offset + page_size, page_size))
      } else {
        cat("Already on last page.\n")
      }
    } else if (tolower(input) == "p") {
      if (start_page > 1) {
        return(pickWFOName(x, cand, offset - page_size, page_size))
      } else {
        cat("Already on first page.\n")
      }
    } else if (tolower(input) %in% c("s", "")) {
      match <- list(method = "SKIP")
      valid <- TRUE
    } else {
      cat("Invalid input. Enter an integer, a valid WFO ID, 'N', 'P', 'S', or press Enter to skip.\n" )
    }
  }

  # Return
  return(match)
}

#' Match a taxonomic name against the WFO GraphQL API
#'
#' @param x taxonomic name to be searched 
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 0 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param useCache logical, if TRUE use cached values in
#'     `options("wfo.api_uri")` preferentially, to reduce the number of API
#'     calls
#' @param useAPI logical, if TRUE (default) allow API calls
#'
#' @return list representation of JSON returned by API call 
#' 
#' @noRd
#' @examples
#' matchWFOName("Burkea africana")
#' 
matchWFOName <- function(x, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = FALSE, fuzzyNameParts = 0, useCache = FALSE, useAPI = TRUE,
  interactive = TRUE) {

  # Search cache for name 
  if (useCache && x %in% names(wfo_cache$names) ) {
    # If name found, use cached version
    match <- wfo_cache$names[[x]]
    cat(sprintf("Using cached data for: %s\n", x))
  } else if (useAPI == TRUE) {
    # If name not found in cache, send API call
    response <- callWFOAPI(x, query_taxonNameMatch(), 
      fallbackToGenus = fallbackToGenus,
      checkRank = checkRank,
      checkHomonyms = checkHomonyms,
      fuzzyNameParts = fuzzyNameParts)

    # If interactive and name not found
    if (is.null(response$data$taxonNameMatch$match) & interactive) {
      match <- pickWFOName(x, response$data$taxonNameMatch$candidates)
    } else {
      match <- response$data$taxonNameMatch$match
      match$method <- "AUTO"
    }
  } else {
    cat(sprintf("No cached name for: %s\n", x))
    match <- list()
      match$method <- "EMPTY"
  }

  # Add query parameters
  match$fallbackToGenus <- fallbackToGenus
  match$checkRank <- checkRank
  match$checkHomonyms <- checkHomonyms
  match$fuzzyNameParts <- fuzzyNameParts 

  # Store result in cache
  if (match$method %in% c("AUTO", "MANUAL") & 
      !x %in% names(wfo_cache$names) &
      !is.null(match$id) && !is.na(match$id)) {
    wfo_cache$names[[x]] <- match
  }

  # Return
  return(match)
}


#' Call the WorldFlora API to match a taxonomic name
#'
#' @param x taxonomic name to be searched 
#' @param query GraphQL API query string, e.g. as returned `query_taxonNameMatch()`
#'     or `query_taxonNameById()`
#' @param fallbackToGenus logical, if TRUE genus-level matches will be returned
#'     if no species-level match is available
#' @param checkRank logical, if TRUE consider matches to be ambiguous if it is
#'     possible to estimate taxonomic rank from the search string and the rank
#'     does not match that in the name record
#' @param checkHomonyms logical, if TRUE consider matches to be ambiguous if
#'     there are other names with the same words but different author strings
#' @param fuzzyNameParts integer value of 0 (default) or greater. The maximum
#'     Levenshtein distance used for fuzzy matching words in `x`
#'
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#' @return list representation of JSON returned by API call 
#' 
#' @noRd
#' @examples
#' callWFOAPI("Burkea africana", query_taxonNameMatch())
#' callWFOAPI("wfo-0000214110", query_taxonNameById())
#'
callWFOAPI <- function(x, query, fallbackToGenus = FALSE, checkRank = FALSE, 
  checkHomonyms = FALSE, fuzzyNameParts = 0) {

  # Create request 
  req <- httr2::request(paste(unlist(options("wfo.api_uri"))))

  # prepare the body
  variables <- list(
    searchString = x, 
    fallbackToGenus = fallbackToGenus,
    checkRank = checkRank,
    checkHomonyms = checkHomonyms,
    fuzzyNameParts = fuzzyNameParts
  )
  payload <- list(query = query, variables = variables)

  # Set body
  req <- httr2::req_body_json(req, payload, auto_unbox = TRUE)

  # Run request
  resp <- httr2::req_perform(req)

  # return the whole thing as a list of lists
  return(httr2::resp_body_json(resp))
}


#' Define WFO GraphQL API query for name matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonNameMatch <- function() {
  "query NameMatch(
    $searchString: String, 
    $checkHomonyms: Boolean,
    $checkRank: Boolean,
    $fallbackToGenus: Boolean
    $fuzzyNameParts: Int
  )
    {
      taxonNameMatch(
        inputString: $searchString
        checkHomonyms: $checkHomonyms
        checkRank: $checkRank
        fallbackToGenus: $fallbackToGenus
        fuzzyNameParts: $fuzzyNameParts
      ) {
        inputString
        searchString
        match {
          id
          fullNameStringPlain
          fullNameStringNoAuthorsPlain
          genusString
          nameString
          authorsString
          nomenclaturalStatus
          role
          rank
          wfoPath
          currentPreferredUsage {
            hasName {
              id
              fullNameStringPlain
              fullNameStringNoAuthorsPlain
              genusString
              nameString
              authorsString
              nomenclaturalStatus
              role
              rank
              wfoPath
            }
          }
        }
        candidates {
          id
          fullNameStringPlain
          fullNameStringNoAuthorsPlain
          genusString
          nameString
          authorsString
          nomenclaturalStatus
          role
          rank
          wfoPath
          currentPreferredUsage {
            hasName {
              id
              fullNameStringPlain
              fullNameStringNoAuthorsPlain
              genusString
              nameString
              authorsString
              nomenclaturalStatus
              role
              rank
              wfoPath
            }
          }
        }
        error
        errorMessage
        method
        narrative
      }
    }"
}

#' Define WFO GraphQL API query for WFO ID matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonNameById <- function() {
  "query NameByID(
    $searchString: String)
    {
      taxonNameById(
        nameId: $searchString
      ) {
        id
        fullNameStringPlain
        fullNameStringNoAuthorsPlain
        genusString
        nameString
        authorsString
        nomenclaturalStatus
        role
        rank
        wfoPath
        currentPreferredUsage {
          hasName {
            id
            fullNameStringPlain
            fullNameStringNoAuthorsPlain
            genusString
            nameString
            authorsString
            nomenclaturalStatus
            role
            rank
            wfoPath
          }
        }
      }
    }"
}
