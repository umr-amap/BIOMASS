#' Check that a URL is responsive
#'
#' @param x URL to check
#'
#' @return logical
#' 
#' @noRd
#' 
checkURL <- function(x) {
  tryCatch(
    {
      req <- httr2::request(x)
      req <- httr2::req_method(req, "HEAD")
      httr2::req_perform(req)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Convert NULL and empty vectors to NA
#'
#' @param x vector that is potentially `NULL` or of `length(0)`
#'
#' @noRd
#' 
null2na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    NA_character_ 
  } else { 
    x
  }
}

#' Common taxonomic name substitutions
#' 
#' @return character vector with regular expressions for use with `gsub()`
#'
#' @details
#' Used in argument `sub_pattern` in `correctTaxo()`
#' 
#' @export
#' 
subPattern <- function() {
  c(" sp[.]", " spp[.]", " pl[.]", " indet[.]", " ind[.]", " gen[.]", " g[.]",
    " fam[.]", " nov[.]", " prox[.]", " cf[.]", " aff[.]", " s[.]s[.]", 
    " s[.]l[.]", " p[.]p[.]", " p[.] p[.]", "[?]", " inc[.]", " stet[.]",
    "Ca[.]", "nom[.] cons[.]", "nom[.] dub[.]", " nom[.] err[.]", " nom[.] illeg[.]", 
    " nom[.] inval[.]", " nom[.] nov[.]", " nom[.] nud[.]", " nom[.] obl[.]", 
    " nom[.] prot[.]", " nom[.] rej[.]", " nom[.] supp[.]", " sensu auct[.]", 
    "\\bsp\\b", "\\bspp\\b", "\\bpl\\b", "\\bindet\\b", "\\bind\\b", "\\bgen\\b", "\\bg\\b",
    "\\bfam\\b", "\\bnov\\b", "\\bprox\\b", "\\bcf\\b", "\\baff\\b")
}

#' Construct WorldFlora GraphQL API calls
#'
#' @param vars list of variables used in query
#' @param query GraphQL API query string, e.g. as returned
#'     `query_taxonNameMatch()`, `query_taxonNameById()`
#'     `query_taxonConceptById()`, `query_classifications()`
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return API call object
#' 
#' @noRd
#' 
#' @examples
#' callAPI("Burkea africana", query_taxonNameMatch())
#' callAPI("wfo-0000214110", query_taxonNameById())
#'
callAPI <- function(vars, query, capacity = 60, fill_time_s = 60, timeout = 10) {

  # Create request 
  req <- httr2::request(getOption("wfo.api_uri"))

  payload <- list(query = query, variables = vars)

  # Set body
  req <- httr2::req_body_json(req, payload, auto_unbox = TRUE)
  
  # Set timeout
  req <- httr2::req_options(req, timeout = timeout)

  # Set throttle to avoid rate-limiting 
  req <- httr2::req_throttle(req, 
    capacity = capacity, 
    fill_time_s = fill_time_s)

  # Return
  return(req)
}

#' Manually pick a taxonomic name from a list returned by the WFO GraphQL API
#'
#' @param x original taxonomic name 
#' @param cand list of candidate taxa 
#' @param offset initial index value used internally by pager, controls index
#'     of page start
#' @param page_size index value used internally by pager, controls page length
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return list containing information of matched taxonomic name
#' 
#' @noRd
#' 
pickName <- function(x, cand, offset = 0, page_size = 10, timeout = 10) {

  # If no candidates, SKIP
  if (length(cand) == 0) {
    message(sprintf("No candidates, skipping: %s", x))
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
        "%-4s%s\t%s\t%s\t%s\t%s\n",
        i,
        cand[[i]]$id,
        cand[[i]]$fullNameStringNoAuthorsPlain,
        cand[[i]]$authorsString,
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
      # Prepare body of call
      input <- tolower(trimws(input))
      api_vars <- list(searchString = input)
      api_call <- callAPI(api_vars, query_taxonNameById(), timeout = timeout)
      api_resp <- httr2::req_perform(api_call)
      api_json <- httr2::resp_body_json(api_resp)
      match <- api_json$data$taxonNameById
      match$method <- "MANUAL"
      valid <- TRUE
    } else if (tolower(input) == "n") {
      if (end_page < length(cand)) {
        return(pickName(x, cand, offset + page_size, page_size))
      } else {
        cat("Already on last page.\n")
      }
    } else if (tolower(input) == "p") {
      if (start_page > 1) {
        return(pickName(x, cand, offset - page_size, page_size))
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

#' Correct and match taxonomic names to the World Flora Online database
#'
#' @description
#' Match taxonomic names using the World Flora Online database, via their 
#'     GraphQL API
#'
#' @param genus vector of genera. Alternatively, the whole taxonomic name
#'     (genus + species) 
#' @param species optional, vector of species epithets to be checked (same
#'     length as `genus`)
#' @param interactive logical, if TRUE (default) user will be prompted to pick
#'     names from a list where multiple ambiguous matches are found, otherwise
#'     names with multiple ambiguous matches will be skipped
#' @param preferAccepted logical, if TRUE, if multiple ambiguous matches are
#'     found, and if only one candidate is an "accepted" name,
#'     automatically choose that name
#' @param preferFuzzy logical, if TRUE, if multiple ambiguous matches are 
#'     found, the accepted matched name with the lowest Levenshtein distance to
#'     the submitted name will be returned
#' @param sub_pattern character vector of regex patterns which will be removed
#'     from `paste(genus, species)` using `gsub()`. The order of this vector
#'     matters, substitutions are applied sequentially. Sensible defaults are
#'     provided by `subPattern()`
#' @param useCache logical, if TRUE use cached values in `the$wfo_cache` 
#'     preferentially, to reduce the number of API calls
#' @param useAPI logical, if TRUE (default) allow API calls
#' @param capacity maximum number of API calls which can accumulate over the 
#'     duration of `fill_time_s`. See documentation for `httr2::req_throttle()`
#' @param fill_time_s time in seconds to refill the capacity for repeated API 
#'     calls. See documentation for `httr2::req_throttle()`
#' @param timeout time in seconds to wait before disconnecting from an
#'     unresponsive request
#'
#' @return data.frame of taxonomic names with rows matching `genus` + `species`.
#' \describe{
#'   \item{nameOriginal}{Original name as in `genus` + `species`}
#'   \item{nameSubmitted}{Name after optional sanitisation according to
#'       `sub_pattern`}
#'   \item{nameMatched}{Matched taxonomic name}
#'   \item{nameAccepted}{Accepted taxonomic name}
#'   \item{familyAccepted}{Family of accepted name}
#'   \item{genusAccepted}{Genus of accepted name}
#'   \item{speciesAccepted}{Species epithet of accepted name}
#'   \item{nameModified}{Flag indicating if `matchedName` is different from
#'       `nameOriginal`, not including the removal of excess whitespace}
#' }
#'
#' @references 
#' Borsch, T. et al. (2020). _World Flora Online: Placing taxonomists at the
#' heart of a definitive and comprehensive global resource on the world's
#' plants_. TAXON, 69, 6. doi10.1002/tax.12373:
#'
#' @author John L. Godlee
#'
#' @examples
#' \donttest{
#' correctTaxo(genus = "Astrocarium", species = "standleanum")
#' correctTaxo(genus = "Astrocarium", species = "standleanum", interactive = F, preferFuzzy = T)
#' correctTaxo(genus = "Astrocarium standleanum", interactive = F, preferFuzzy = T)
#' }
#'
#' @export
#' @importFrom stringdist stringsim 

correctTaxo <- function(genus, species = NULL, interactive = TRUE,
  preferAccepted = FALSE, preferFuzzy = FALSE, sub_pattern = subPattern(),
  useCache = FALSE, useAPI = TRUE, capacity = 60, fill_time_s = 60, timeout = 10) {

  # Combine genus and species fragments
  if (!is.null(species)) { 
    if (length(genus) != length(species)) {
      stop("'genus' and 'species' must be of equal length")
    }
    species[is.na(species)] <- ""
    x <- paste(genus, species, sep = " ")
  } else { 
    x <- genus
  }

  # If API throttling arguments are empty, set generous limits 
  if (is.na(capacity)) { 
    capacity <- length(x)
  }

  if (is.na(fill_time_s)) { 
    fill_time_s <- length(x)
  }

  # Check use of API and cache
  if (!useCache && !useAPI) {
    stop("Either useCache or useAPI must be TRUE")
  }

  if (preferFuzzy && interactive) { 
    warning(
      "'preferFuzzy' and 'interactive' are both TRUE, defaulting to interactive matching", 
      immediate. = TRUE)
    preferFuzzy <- FALSE
  }

  # Check if WFO API is reachable 
  if (useAPI && !checkURL(getOption("wfo.api_uri"))) {
    w <- paste("WFO API unreachable:", getOption("wfo.api_uri"))
    if (useCache) {
      warning(w, "\nOnly cached names will be filled", immediate. = TRUE)
      useAPI <- FALSE
    } else {
      stop("\n", w, " and useCache = FALSE, Exiting ...")
    }
  }

  # Duplicate vector of names to prepare for sanitising
  xsub <- x

  # Convert characters to lowercase
  xsub <- tolower(xsub)

  # Optionally remove substrings matched by sub_pattern
  sub_pattern <- na.omit(sub_pattern)
  if (length(sub_pattern) > 0) {
    for (i in seq_along(sub_pattern)) {
      xsub <- gsub(sub_pattern[i], "", xsub)
    }
  } 

  # Remove leading and trailing whitespace and multiple spaces
  xsub <- trimws(gsub("\\s+", " ", xsub))

  # Extract unique names 
  xun <- sort(unique(xsub))

  # Optionally search cache for names
  match_cache_list <- list()
  if (useCache) {
    # Extract cached names
    match_cache_list <- the$wfo_cache[xun]
    match_cache_list[sapply(match_cache_list, is.null)] <- NULL

    # Remove names matched in cache from vector of names
    xun <- xun[!xun %in% names(match_cache_list)]

    # Message
    if (length(match_cache_list) > 0) {
      message(sprintf("Using cached data for %s names", length(match_cache_list)))
    }
  }

  # Send message if names not matched in cache and API is off 
  if (!useAPI && length(xun) > 0) {
  message(sprintf(
    "Some names not found in cache and useAPI = FALSE. These names will be NA:\n  %s",
      paste(xun, collapse = "\n  ")
    ))
  }

  # For each taxonomic name (optionally excluding names matched in cache)
  # Construct API calls
  match_api_list <- list()
  if (useAPI && length(xun) > 0) {
    # Get current WFO backbone version
    req <- httr2::request(getOption("wfo.api_uri"))
    bb_payload <- list(query = query_classifications())
    bb_req <- httr2::req_body_json(req, bb_payload, auto_unbox = TRUE)
    bb_resp <- httr2::req_perform(bb_req)
    bb_json <- httr2::resp_body_json(bb_resp)
    bb <- unlist(bb_json)

    # Construct API calls for name matching
    api_call_list <- list()
    for (i in seq_along(xun)) {

      api_vars <- list(
        searchString = xun[i], 
        fallbackToGenus = FALSE,
        checkRank = FALSE,
        checkHomonyms = FALSE,
        fuzzyNameParts = 3
      )

      api_call_list[[i]] <- callAPI(api_vars, 
          query = query_taxonNameMatch(), 
          capacity = capacity,
          fill_time_s = fill_time_s,
          timeout = timeout)
    }

    # Submit API calls
    api_resp_list <- httr2::req_perform_parallel(api_call_list)

    # Convert API responses to JSON
    api_json_list <- lapply(api_resp_list, httr2::resp_body_json)

    # Collect matched names 
    for (i in seq_along(api_json_list)) {

      # If unambiguous match found
      if (!is.null(api_json_list[[i]]$data$taxonNameMatch$match)) {
        # Singular accepted name
        match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$match
        match_api_list[[i]]$method <- "AUTO"
      } else if (length(api_json_list[[i]]$data$taxonNameMatch$candidates) != 0) {
        # Find Levenshtein distance between submitted name and candidate names
        cand_dist <- 1 - stringdist::stringsim(xun[i],
          unlist(lapply(
            api_json_list[[i]]$data$taxonNameMatch$candidates, 
            "[[", "fullNameStringNoAuthorsPlain")) )

        # List role of candidate matches (accepted, synonym, unplaced, etc.) 
        role_lev <- c("accepted", "synonym", "unplaced", "deprecated")
        cand_roles <- factor(unlist(lapply(
            api_json_list[[i]]$data$taxonNameMatch$candidates, 
            "[[", "role")), levels = role_lev)

        # Reorder candidate matches according to Levenshtein distance and role
        cand_names <- unlist(lapply(
            api_json_list[[i]]$data$taxonNameMatch$candidates, 
            "[[", "fullNameStringNoAuthorsPlain")) 

        # Sort candidate matches first by Levenshtein distance, then by role
        cand_sort <- order(cand_dist, cand_roles, cand_names)

        api_json_list[[i]]$data$taxonNameMatch$candidates <- 
          api_json_list[[i]]$data$taxonNameMatch$candidates[cand_sort]
        cand_roles <- cand_roles[cand_sort]

        if (preferAccepted && sum(cand_roles == "accepted") == 1) {
          # Auto-accept singular accepted name
          match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$candidates[[
            which(cand_roles == "accepted")]]
          match_api_list[[i]]$method <- "AUTO ACC"
        } else if (preferFuzzy) {
          # Auto-accept singular accepted name
          match_api_list[[i]] <- api_json_list[[i]]$data$taxonNameMatch$candidates[[1]]
          match_api_list[[i]]$method <- "AUTO FUZZY"
        } else if (interactive) {
          # Interactive name picking
          match_api_list[[i]] <- pickName(xun[i], api_json_list[[i]]$data$taxonNameMatch$candidates)
        } else {
          # No successful match
          message(sprintf("No unique match for: %s", xun[i]))
          match_api_list[[i]] <- list()
          match_api_list[[i]]$method <- "EMPTY"
        }
      } else {
        # No candidates 
        message(sprintf("No candidates for: %s\n", xun[i]))
        match_api_list[[i]] <- list()
        match_api_list[[i]]$method <- "EMPTY"
      }

      # Add submitted name
      match_api_list[[i]]$submitted_name <- xun[i]
    }

    # Construct API calls to extract family, genus, and species epithet
    wfo_id_bb_api_call_list <- list()
    for (i in seq_along(match_api_list)) {
      wfo_id_bb <- paste0(
        match_api_list[[i]]$currentPreferredUsage$hasName$id, "-", bb)

      # Extract accepted name WFO IDs and add current backbone version 
      api_vars <- list(searchString = wfo_id_bb)
      wfo_id_bb_api_call_list[[i]] <- callAPI(api_vars, 
        query = query_taxonConceptById(),
        capacity = capacity,
        fill_time_s = fill_time_s,
        timeout = timeout)
    }

    # Submit API calls
    api_req <- httr2::req_perform_parallel(wfo_id_bb_api_call_list)

    # Convert API responses to JSON
    api_resp <- lapply(api_req, httr2::resp_body_json)

    # Extract family, genus and species epithet
    for (i in seq_along(api_resp)) {

      path <- api_resp[[i]]$data$taxonConceptById$path

      # Extract all ranks
      ranks <- vapply(
        path,
        function(j) {
          r <- j$hasName$rank
          if (length(r) == 0) NA_character_ else r
        },
        character(1)
      )

      # Isolate desired ranks
      rank_sel <- c("family", "genus", "species")
      idx <- match(rank_sel, ranks)

      # Build named output 
      rank_list <- setNames(vector("list", length(rank_sel)),
                      paste0(rank_sel, "Acc"))

      for (j in seq_along(idx)) {
        if (is.na(idx[j])) {
          # Set missing ranks to NA
          rank_list[[j]] <- NA_character_
        } else {
          # Extract rank values
          rank_list[[j]] <- path[[idx[j]]]$hasName$fullNameStringNoAuthorsPlain
        }
      }

      # Add ranks to list 
      match_api_list[[i]] <- c(match_api_list[[i]], rank_list)

      # Get species epithet only
      match_api_list[[i]]$speciesAcc <- gsub(".*\\s", "", trimws(match_api_list[[i]]$speciesAcc))

    }
    names(match_api_list) <- xun
  }

  # Combine match lists
  match_list <- c(match_cache_list, match_api_list)

  # Create formatted dataframe
  if (length(match_list) == 0) {
    # Handle the case where nothing was matched (API down + empty cache)
    match_df <- data.frame(
      nameSubmitted = xun,
      nameMatched = NA_character_,
      nameAccepted = NA_character_,
      familyAccepted = NA_character_,
      genusAccepted = NA_character_,
      speciesAccepted = NA_character_
    )
  } else {
    match_df <- do.call(rbind, lapply(match_list, function(i) { 
      if ("id" %in% names(i)) {
        data.frame(
          nameSubmitted = null2na(i$submitted_name),
          nameMatched = null2na(i$fullNameStringNoAuthorsPlain),
          nameAccepted = null2na(i$currentPreferredUsage$hasName$fullNameStringNoAuthorsPlain),
          familyAccepted = null2na(i$familyAcc), 
          genusAccepted = null2na(i$genusAcc), 
          speciesAccepted = null2na(i$speciesAcc))
      } else { 
        data.frame(
          nameSubmitted = null2na(i$submitted_name),
          nameMatched = NA_character_,
          nameAccepted = NA_character_,
          familyAccepted = NA_character_,
          genusAccepted = NA_character_,
          speciesAccepted = NA_character_)
      }
    }))
  }
  
  # Match row order of dataframe to x
  out <- cbind(nameOriginal = x, match_df[match(xsub, match_df$nameSubmitted),])
  rownames(out) <- NULL

  # Add whether name has been modified
  out$nameModified <- ifelse(
    trimws(gsub("\\s+", " ", out$nameOriginal)) != out$nameMatched, 
    TRUE, FALSE)

  # Store good API results in cache
  # Non-ambiguous automatic matches and manual assertions only
  match_list_new <- match_list[
    !names(match_list) %in% names(the$wfo_cache)]
  the$wfo_cache <- c(the$wfo_cache, match_list_new)

  # Return
  return(out)
}

#' Reusable GraphQL fields for taxon objects
#' 
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
wfo_query_fields <- function() {
  "id
   fullNameStringNoAuthorsPlain
   authorsString
   nomenclaturalStatus
   role
   rank
   wfoPath
   currentPreferredUsage {
     hasName {
       id
       fullNameStringNoAuthorsPlain
       authorsString
       nomenclaturalStatus
       role
       rank
       wfoPath
     }
   }"
}

#' Define WFO GraphQL API query for name matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonNameMatch <- function() {
  paste0("query NameMatch(
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
        match { ", wfo_query_fields(), " }
        candidates { ", wfo_query_fields(), "}
        error
        errorMessage
        method
        narrative
      }
    }")
}

#' Define WFO GraphQL API query for WFO ID name matching
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonNameById <- function() {
  paste0("query NameByID(
    $searchString: String)
    {
      taxonNameById(
        nameId: $searchString
      ) { ", wfo_query_fields(), " }
    }")
}

#' Define WFO GraphQL API query for WFO ID concept matching
#' 
#' Used to return higher order taxonomic rank information
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_taxonConceptById <- function() {
  "query ConceptByID(
    $searchString: String)
  {
    taxonConceptById(
      taxonId: $searchString
    ) {
      path {
        hasName {
          id
          fullNameStringNoAuthorsPlain
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

#' Define WFO GraphQL API query for current backbone version
#' 
#' Used to return higher order taxonomic rank information
#'
#' @return character string with WFO GraphQL API query
#' 
#' @noRd
#' 
query_classifications <- function() { 
  "query {
      classifications(classificationId: \"DEFAULT\") {
        id
      }
    }
  "
}

