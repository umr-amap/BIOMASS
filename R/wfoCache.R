#' Get, set, load, and save the World Flora Online name matching cache
#'
#' @description
#' [correctTaxo()] stores the results of taxonomic name matching in a cache
#' when `useCache = TRUE`, including choices made during interactive matching.
#' These functions allow the cache to be inspected, replaced, saved to a
#' file, and loaded from a file, so that name matching does not need to be
#' repeated.
#'
#' @param x for `setWFOCache()`, a named list, as returned by
#'     `getWFOCache()`. For `loadWFOCache()` and `saveWFOCache()`, a file path.
#' @param rm_empty logical, if TRUE names for which no match was found
#'     (method "EMPTY") are not saved, so that they can be matched later, e.g.
#'     interactively
#'
#' @return `getWFOCache()` returns the cache, a named list with one element
#'     per matched name. `setWFOCache()`, `loadWFOCache()` and `saveWFOCache()`
#'     return the cache invisibly.
#'
#' @details
#' `loadWFOCache()` replaces the current cache with the contents of the file.
#' If the file does not exist the cache is emptied.
#'
#' @author John L. Godlee
#'
#' @examples
#' \dontrun{
#' correctTaxo(genus = "Astrocarium standleanum", useCache = TRUE)
#' f <- tempfile(fileext = ".rds")
#' saveWFOCache(f)
#' setWFOCache(list())
#' loadWFOCache(f)
#' names(getWFOCache())
#' }
#'
#' @name wfoCache
NULL

#' @rdname wfoCache
#' @export
getWFOCache <- function() {
  the$wfo_cache
}

#' @rdname wfoCache
#' @export
setWFOCache <- function(x) {
  if (!is.list(x)) {
    stop("'x' must be a list")
  }
  the$wfo_cache <- x
  invisible(x)
}

#' @rdname wfoCache
#' @export
loadWFOCache <- function(x) {
  wfo_cache <- if (file.exists(x)) readRDS(x) else list()
  setWFOCache(wfo_cache)
}

#' @rdname wfoCache
#' @export
saveWFOCache <- function(x, rm_empty = FALSE) {
  wfo_cache <- getWFOCache()

  if (rm_empty) {
    is_empty <- vapply(wfo_cache, function(y) {
      identical(y$method, "EMPTY")
    }, logical(1))
    wfo_cache <- wfo_cache[!is_empty]
  }

  saveRDS(wfo_cache, x)
  invisible(wfo_cache)
}
