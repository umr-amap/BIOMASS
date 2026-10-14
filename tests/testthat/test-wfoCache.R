test_that("WFO cache can be set, saved, and loaded", {
  old <- getWFOCache()
  on.exit(setWFOCache(old))

  cache <- list(
    "burkea africana" = list(method = "AUTO", submitted_name = "burkea africana"),
    "unknown sp" = list(method = "EMPTY", submitted_name = "unknown sp"),
    "julbernardia" = list(method = "MANUAL", submitted_name = "julbernardia"))

  setWFOCache(cache)
  expect_identical(getWFOCache(), cache)
  expect_error(setWFOCache("a"), "must be a list")

  # Save all names, or without names with no match
  f_all <- tempfile(fileext = ".rds")
  f_rm <- tempfile(fileext = ".rds")
  saveWFOCache(f_all)
  saveWFOCache(f_rm, rm_empty = TRUE)
  expect_identical(readRDS(f_all), cache)
  expect_named(readRDS(f_rm), c("burkea africana", "julbernardia"))

  # Load replaces the cache
  setWFOCache(list())
  loadWFOCache(f_all)
  expect_identical(getWFOCache(), cache)

  # Loading a file which does not exist empties the cache
  loadWFOCache(tempfile(fileext = ".rds"))
  expect_identical(getWFOCache(), list())
})
