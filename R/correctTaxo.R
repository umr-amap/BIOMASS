#' Replace taxonomic names using lookup tables
#'
#' @param x vector of species names
#' @param lookup a single dataframe or a list of dataframes containing lookup
#'     tables. The first column should contain names in `x` to be changed. The
#'     second column should contain the new names.
#'
#' @return Vector of corrected species names
#' 
#' @details Lookup tables are run in order through the list of lookup tables, meaning 
#' names may change incrementally multiple times.
#' 
#' @importFrom data.table rbindlist
#' 
#' @keywords internal 
#' @noRd 
#'
synonymyFix <- function(x, lookup) {

  # Make list if not already
  if (!inherits(lookup, "list")) {
    lookup <- list(lookup)
  }

  # Combine lookup tables into a single dataframe
  lookup_combi <- as.data.frame(rbindlist(lookup))

  # Check no NAs
  if (any(is.na(lookup_combi))) {
    stop("Lookup table cannot contain NA entries")
  }

  # Do substitution
  out <- lookup_combi[,2][match(x, lookup_combi[,1])]
  out[is.na(out)] <- x[is.na(out)]

  return(out)
}

#' Return default pattern substitution for `taxonCheck()`
#'
#' @return vector of regex patterns for use with `taxonCheck()` in argument
#'     `sub.pattern`
#' 
#' @keywords internal 
#' @noRd 
#' 
WFO.prepare_default <- function() { 
  c(
    " indet$",
    " sp[.]",
    " spp[.]",
    " ssp[.]",
    " pl[.]",
    " indet[.]",
    " ind[.]",
    " gen[.]",
    " g[.]",
    " fam[.]",
    " nov[.]",
    " prox[.]",
    " cf[.]",
    " aff[.]",
    " s[.]s[.]",
    " s[.]l[.]",
    " p[.]p[.]",
    " p[.] p[.]",
    "[?]",
    " inc[.]",
    " stet[.]",
    "nom[.] cons[.]",
    "nom[.] dub[.]",
    " nom[.] err[.]",
    " nom[.] illeg[.]",
    " nom[.] inval[.]",
    " nom[.] nov[.]",
    " nom[.] nud[.]",
    " nom[.] obl[.]",
    " nom[.] prot[.]",
    " nom[.] rej[.]",
    " nom[.] supp[.]",
    " sensu auct[.]"
  )
}

#' Return URL linking to WorldFlora Online data from Zenodo
#' 
#' @param zenodo_url Zenodo API URL linking to the World Flora Online archive 
#'
#' @return 
#' 
#' @details 
#' Returns the URL for the most recent WorldFlora taxonomic backbone from
#' Zenodo. The download source from Zenodo is generally kept more up to date
#' than the source provided by `WorldFlora::WFO.download()`, and downloads
#' quicker.
#' 
#' @importFrom httr2 resp_body_json req_perform request
#' 
#' @export
#' 
WFO_url <- function(
  zenodo_record = "https://zenodo.org/api/records/7460141") {

  # Check input
  if (is.null(zenodo_record)) { 
    stop("zenodo_record must be provided")
  }

  # Extract JSON from Zenodo record
  resp <- httr2::resp_body_json(httr2::req_perform(httr2::request(zenodo_record)))

  # Isolate URL
  urls <- sapply(resp$files, function(f) f$links$self)
  WFO.url <- urls[grepl("_DwC_backbone_R", urls)]

  # Return
  return(WFO.url)
}

#' Correct and match taxonomic names to the World Flora Taxonomic Backbone
#'
#' @param x vector of taxonomic names
#' @param WFO.file optional file name of static copy of World Flora Online
#'     Taxonomic Backbone. If not NULL, data will be reloaded from this file
#' @param WFO.data optional object containinf static copy of World Flora Online
#'     Taxonomic backbone. Ignored if `WFO.file` is not NULL
#' @param lookup optional a single dataframe or a list of dataframes containing
#'     lookup tables. The first column should contain names in `x` to be
#'     changed. The second column should contain the new names.
#' @param ret_wfo logical, if TRUE the function stops after
#'     `WorldFlora::WFO.match()` and returns the raw output from this function.
#' @param ret_unk logical, if TRUE taxa not matched in the World
#'     Flora Online are returned to the user as a vector containing
#'     the unmatched values. If FALSE these taxa are returned as NA.
#' @param ret_multi logical, if TRUE taxa matching multiple records in the
#'     World Flora Online are returned to the user as a list with one element
#'     for each original name containing the unmatched values. If FALSE the
#'     "best" name is selected by `WorldFlora::WFO.one()`
#' @param sub.pattern vector with regular expressions defining sections of `x`
#'     to be removed during correction of common orthographic errors by
#'     `WorldFlora::WFO.prepare()`
#' @param fuzzy If larger than 0, then attempt fuzzy matching. See `WorldFlora::WFO.match()`
#' @param ... Additional arguments passed to `WorldFlora::WFO.match()`
#'
#' @return Dataframe with cleaned taxonomic names and metadata
#'
#' @details
#' Taxonomic names are matched against the World Flora Online database using
#' `WorldFlora::WFO.match()`.
#' 
#' The search algorithm is as follows:
#'     \enumerate{
#'       \item{Optionally replace names with `lookup`}
#'       \item{Correct common orthographic errors with `WorldFlora::WFO.prepare()`}
#'       \item{Query `WorldFlora::WFO.match()` for accepted
#'             name and taxonomic rank information}
#'       \item{Optionally return multiple matches or unsuccessful matches}
#'       \item{Consolidate multiple matches with `WorldFlora::WFO.one()`}
#'       \item{Return formatted dataframe}
#'     }
#' 
#' Names that cannot be matched should be replaced with "Indet indet" in
#' `lookup`. These are replaced with NA_character_ before `WorldFlora::WFO.match()`
#'
#' If both WFO.file and WFO.data are not provided, the most recent WFO
#' taxonomic backbone will be downloaded from Zenodo. See `WFO.url()` and
#' `cacheManager()`.
#'
#' @importFrom data.table fread data.table
#' @importFrom WorldFlora WFO.prepare WFO.match WFO.one
#'
#' @export
#' @author John L. GODLEE
#'
#' @examples
#' 
#' # Define vector of taxonomic names
#' s <- c("Senegalia caffra", "Brachystegia spiciformis", "Julbernardia indet")
#' 
#' # Basic usage
#' correctTaxo(s)
#' 
#' # Return non-perfect matches
#' correctTaxo(s, ret_unk = TRUE)
#'
#' # Create a lookup table to pre-emptively correct some names
#' lookup <- tibble::tribble(
#'   ~"original", ~"corrected",
#'   "Senegalia caffra", "Senegalia afra")
#' 
#' # Use lookup table
#' correctTaxo(s, lookup = lookup, ret_unk = TRUE)
#' 
#' # Download and use specific version of WFO backbone 
#' download.file("https://zenodo.org/records/10425161/files/_DwC_backbone_R.zip", 
#'   destfile = "./wfo.zip")
#' unzip("./wfo.zip")
#' wfo_data <- data.table::fread("./classification.csv")
#' 
#' correctTaxo(s, WFO.data = wfo_data, ret_unk = TRUE)
#' 
#' correctTaxo(s, WFO.file = "./classification.csv", ret_unk = TRUE)
#' 
#' # Return unmodified output from `WorldFlora::WFO.match()`
#' correctTaxo(s, ret_wfo = TRUE)
#'
correctTaxo <- function(x, WFO.file = NULL, WFO.data = NULL, 
   lookup = NULL, ret_wfo = FALSE, ret_unk = FALSE, ret_multi = FALSE,
   sub.pattern = WFO.prepare_default(), fuzzy = 0.1, ...) {

  # Check WFO data is available
  if (is.null(WFO.data) & is.null(WFO.file)) {
    message("Both WFO.file and WFO.data not provided, searching for cached WFO backbone data")
    WFO.data <- data.table::fread(cacheManager("classification.csv"))
  } else if (!is.null(WFO.data)) {
    message(paste("Reading WFO.data"))
    WFO.data <- data.table::data.table(WFO.data)
  } else if (!is.null(WFO.file)) {
    WFO.data <- data.table::fread(WFO.file, encoding = "UTF-8")
  }

  WFO.data$scientificName <- gsub("\\s+", " ", WFO.data$scientificName)

  # Get unique taxonomic names
  xu <- unique(x)

  # Substitute names with lookup table
  if (!is.null(lookup)) {
    message("Substituting names with `lookup`")
    xf <- synonymyFix(xu, lookup = lookup)
  } else {
    xf <- xu
  }

  # Prepare taxonomic names for WFO query
  xs <- WorldFlora::WFO.prepare(xf, sub.pattern = sub.pattern)$spec.name
  
  # Replace Indet genera with ""
  xi <- xs
  xi[xi == "Indet"] <- ""

  # Run WFO matching 
  message("Querying World Flora Online")
  wfo <- WorldFlora::WFO.match(xi, 
    WFO.data = WFO.data, Fuzzy = fuzzy, ...)

  # Add original names
  wfo <- cbind("taxon_name_orig" = xu[match(wfo$spec.name.ORIG, xi)], wfo)

  # Check all original names are matched back 
  stopifnot(all(!is.na(wfo$taxon_name_orig)))

  # Optionally return raw WFO output  
  if (ret_wfo) { return(wfo) }

  # Consolidate to single best name per taxon
  wfo_one <- WorldFlora::WFO.one(wfo, verbose = FALSE)
  wfo_sel <- wfo_one[,c(
    "taxon_name_orig",
    "spec.name.ORIG",  # taxon_name_sanit
    "Old.name",  # taxon_name_syn
    "Old.ID",  # taxon_wfo_syn
    "scientificName",  # taxon_name_acc
    "taxonID",  # taxon_wfo_acc
    "scientificNameAuthorship",  # taxon_auth_acc
    "taxonRank",  # taxon_rank_acc
    "parentNameUsageID",  # taxon_wfo_parent
    "specificEpithet",  # taxon_epithet_acc
    "genus",  # taxon_genus_acc
    "family"  # taxon_family_acc
  )]
  wfo_sel <- unique(wfo_sel)

  # All submitted names should be included in WFO output
  stopifnot(all(sort(unique(wfo_sel$spec.name.ORIG)) == sort(unique(xs))))

  # Consolidate genus and species
  wfo_sel$species <- trimws(paste(wfo_sel$genus, wfo_sel$specificEpithet))

  wfo_sel$species <- ifelse(!wfo_sel$taxonRank %in% 
      c("species", "subspecies", "variety", "subvariety", 
          "form", "subform", "prole", "unranked"), 
    NA_character_, wfo_sel$species)

  # Extract subsp. and var. epithets from accepted names
  wfo_sel$taxon_subspecies_acc <- gsub(".*subsp\\.\\s", "", wfo_sel$scientificName)
  wfo_sel$taxon_subspecies_acc[!grepl("\\ssubsp\\.\\s", wfo_sel$scientificName)] <- NA_character_

  wfo_sel$taxon_variety_acc <- gsub(".*var\\.\\s", "", wfo_sel$scientificName)
  wfo_sel$taxon_variety_acc[!grepl("\\svar\\.\\s", wfo_sel$scientificName)] <- NA_character_

  # Fill wfo ID of synonyms
  wfo_sel$Old.ID <- ifelse(wfo_sel$Old.ID == "", 
    wfo_sel$taxonID, wfo_sel$Old.ID)

  wfo_sel$Old.name <- ifelse(wfo_sel$Old.name == "", 
    wfo_sel$scientificName, wfo_sel$Old.name)

  # Add date of processing
  wfo_sel$taxon_wfo_date <- Sys.Date()

  # Create output dataframe
  out <- wfo_sel[,c(
    "taxon_name_orig",  
    "spec.name.ORIG",  # taxon_name_sanit
    "Old.name",  # taxon_name_syn
    "Old.ID",  # taxon_wfo_syn
    "scientificName",  # taxon_name_acc
    "taxonID",  # taxon_wfo_acc
    "scientificNameAuthorship",  # taxon_auth_acc
    "taxonRank",  # taxon_rank_acc
    "parentNameUsageID",  # taxon_wfo_parent
    "taxon_variety_acc",
    "taxon_subspecies_acc",
    "specificEpithet",  # taxon_epithet_acc
    "species",  # taxon_species_acc
    "genus",  # taxon_genus_acc
    "family",  # taxon_family_acc
    "taxon_wfo_date")]

  names(out) <- c(
    "taxon_name_orig",
    "taxon_name_sanit",
    "taxon_name_syn",
    "taxon_wfo_syn",
    "taxon_name_acc",
    "taxon_wfo_acc",
    "taxon_auth_acc",
    "taxon_rank_acc",
    "taxon_wfo_parent",
    "taxon_variety_acc",
    "taxon_subspecies_acc",
    "taxon_epithet_acc",
    "taxon_species_acc",
    "taxon_genus_acc",
    "taxon_family_acc",
    "taxon_wfo_date")

  # Optionally return unmatched names
  if (ret_unk & any(out$taxon_name_sanit != out$taxon_name_syn, na.rm = TRUE)) {
    unmatched <- out$taxon_name_orig[
      (out$taxon_name_sanit != out$taxon_name_syn) | 
        is.na(out$taxon_name_syn) | is.na(out$taxon_name_sanit)]
    warning("Some taxonomic names not matched by WFO.match, returning original names")
    return(unmatched)
  }

  # Optionally return names with multiple matches
  if (ret_multi & any(duplicated(wfo$taxon_name_orig))) {
    multis <- wfo$taxon_name_orig[duplicated(wfo$taxon_name_orig)]
    multis_df <- wfo[wfo$taxon_name_orig %in% multis,]
    multis_list <- split(multis_df, multis_df$taxon_name_orig)
    warning("Some taxonomic names matched to multiple names by WFO.match, returning options")
    return(multis_list)
  }

  # Change "" to NA in all columns
  out[] <- lapply(out, function(x) {
    if (is.character(x)) {
      x[x == ""] <- NA_character_
    }
    x
  })

  # All original names should be filled
  stopifnot(all(!is.na(out$species[out$species_sanit != "Indet indet"])))

  # Return
  return(out)
}

