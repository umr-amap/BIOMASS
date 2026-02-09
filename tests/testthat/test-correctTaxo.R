# Create dummy cache environment for tests
if (!exists("the")) the <- new.env()

# Helper function to reset cache between tests
reset_cache <- function() {
  the$wfo_cache <- list()
}

# Reset cache
reset_cache()

# null2na 
test_that("null2na works correctly", {
  # Test various strings and combinations
  expect_equal(null2na(NULL), NA_character_)
  expect_equal(null2na(numeric(0)), NA_character_)
  expect_equal(null2na("Quercus"), "Quercus")
  expect_equal(null2na(c("A", "B")), c("A", "B"))
  expect_equal(null2na(c(NA, "B")), c(NA_character_, "B"))
  expect_type(null2na(NA_real_), "double")
})

# subPattern 
test_that("subPattern returns a character vector of regexes", {
  patterns <- subPattern()
  expect_type(patterns, "character")
  expect_true(length(patterns) > 0)
  expect_true(any(grepl("sp[.]", patterns, fixed = TRUE)))
})


# correctTaxo - input validation
test_that("correctTaxo validates genus and species length match", {
  expect_error(
    correctTaxo(genus = c("Quercus", "Fagus"), species = c("alba")),
    "'genus' and 'species' must be of equal length"
  )
})

# correctTaxo - Data source toggles
test_that("correctTaxo requires at least one data source", {
  expect_error(
    correctTaxo("Quercus", "alba", useCache = FALSE, useAPI = FALSE),
    "Either useCache or useAPI must be TRUE"
  )
})

# correctTaxo - API availability handling
test_that("correctTaxo handles unreachable API gracefully", {
  reset_cache()

  # checkURL returns FALSE always, to simulate unreachable API
  mockery::stub(correctTaxo, "checkURL", FALSE)
  
  # Error when cache is disabled and API not reachable
  expect_error(
    correctTaxo("Quercus", "alba", useCache = FALSE, useAPI = TRUE), 
    "WFO API unreachable"
  )
  
  # Warn when cache is enabled and API not reachable
  expect_warning(
    correctTaxo("Quercus", "alba", useCache = TRUE, useAPI = TRUE),
    "WFO API unreachable.*Only cached names will be filled"
  )
})

# correctTaxo - fuzzy matching and interactivity
test_that("correctTaxo warns when both interactive and preferFuzzy are TRUE", {
  reset_cache()

  # checkURL returns TRUE always, 
  # to simulate reachable API without pinging the API
  mockery::stub(correctTaxo, "checkURL", TRUE)
  
  # Warn when both interactive and preferFuzzy are TRUE
  expect_warning(
    correctTaxo("Quercus", "alba", interactive = TRUE, preferFuzzy = TRUE, 
      useAPI = FALSE, useCache = TRUE),
    "'preferFuzzy' and 'interactive' are both TRUE"
  )
})

# correctTaxo - cache functionality
test_that("correctTaxo uses cache and avoids API calls when possible", {
  reset_cache()
  
  # Create fake cache record
  the$wfo_cache <- list(
    "burkea africana" = list(
      submitted_name = "burkea africana",
      id = "wfo-1",
      fullNameStringNoAuthorsPlain = "TEST"
    )
  )
  
  # checkURL returns TRUE always, 
  # to simulate reachable API without pinging API
  mockery::stub(correctTaxo, "checkURL", TRUE)

  # Simulate backbone version without pinging API 
  mock_bb <- list(data = list(classifications = list(id = "v2023.12")))
  mockery::stub(correctTaxo, "httr2::req_perform", mockery::mock(mock_bb))
  mockery::stub(correctTaxo, "httr2::resp_body_json", mockery::mock(mock_bb))
  
  # Choose cached value 
  expect_equal(
    correctTaxo("Burkea", "africana", useCache = TRUE, useAPI = TRUE, 
      interactive = FALSE)$nameMatched, 
    "TEST")
})

# correctTaxo - matching logic
test_that("correctTaxo selects accepted name when preferAccepted is TRUE", {
  reset_cache()

  # checkURL returns TRUE always, 
  # to simulate reachable API without pinging API
  mockery::stub(correctTaxo, "checkURL", TRUE)
  
  # Create mock backbone classification response
  mock_bb_resp <- structure(list(), class = "httr2_response")
  mock_bb <- list(data = list(classifications = list(list(id = "v1"))))
  
  # Mock name matching response with synonym and accepted name
  mock_match <- list(data = list(taxonNameMatch = list(
    match = NULL,
    candidates = list(
      list(
        id = "wfo-syn", 
        role = "synonym", 
        fullNameStringNoAuthorsPlain = "Quercus oldname",
        authorsString = "L.",
        nomenclaturalStatus = "",
        rank = "species",
        wfoPath = "Fagaceae/Quercus/oldname",
        currentPreferredUsage = list(hasName = list(
          id = "wfo-acc", 
          fullNameStringNoAuthorsPlain = "Quercus alba",
          authorsString = "L.",
          nomenclaturalStatus = "",
          role = "accepted",
          rank = "species",
          wfoPath = "Fagaceae/Quercus/alba"
        ))
      ),
      list(
        id = "wfo-acc", 
        role = "accepted", 
        fullNameStringNoAuthorsPlain = "Quercus alba",
        authorsString = "L.",
        nomenclaturalStatus = "",
        rank = "species",
        wfoPath = "Fagaceae/Quercus/alba",
        currentPreferredUsage = list(hasName = list(
          id = "wfo-acc", 
          fullNameStringNoAuthorsPlain = "Quercus alba",
          authorsString = "L.",
          nomenclaturalStatus = "",
          role = "accepted",
          rank = "species",
          wfoPath = "Fagaceae/Quercus/alba"
        ))
      )
    )
  )))
  
  # Create mock taxon concept response 
  mock_ranks <- list(data = list(taxonConceptById = list(
    path = list(
      list(hasName = list(
        rank = "family", 
        fullNameStringNoAuthorsPlain = "Fagaceae",
        id = "wfo-fam",
        authorsString = "",
        nomenclaturalStatus = "",
        role = "accepted",
        wfoPath = "Fagaceae"
      )),
      list(hasName = list(
        rank = "genus", 
        fullNameStringNoAuthorsPlain = "Quercus",
        id = "wfo-gen",
        authorsString = "L.",
        nomenclaturalStatus = "",
        role = "accepted",
        wfoPath = "Fagaceae/Quercus"
      )),
      list(hasName = list(
        rank = "species", 
        fullNameStringNoAuthorsPlain = "Quercus alba",
        id = "wfo-acc",
        authorsString = "L.",
        nomenclaturalStatus = "",
        role = "accepted",
        wfoPath = "Fagaceae/Quercus/alba"
      ))
    )
  )))

  # Create mock response objects
  mock_match_resp <- structure(list(), class = "httr2_response")
  mock_ranks_resp <- structure(list(), class = "httr2_response")
  
  # Stub JSON parsing to return mock data in sequence
  m_json <- mockery::mock(mock_bb, mock_match, mock_ranks)
  mockery::stub(correctTaxo, "httr2::resp_body_json", m_json)
  
  # Stub req_perform for the backbone call
  m_perform <- mockery::mock(mock_bb_resp)
  mockery::stub(correctTaxo, "httr2::req_perform", m_perform)
  
  # Stub req_perform_parallel for batch calls (returns list of responses)
  m_parallel <- mockery::mock(list(mock_match_resp), list(mock_ranks_resp))
  mockery::stub(correctTaxo, "httr2::req_perform_parallel", m_parallel)

  # Choose the accepted name
  expect_equal(
    correctTaxo("Quercus", "alba", interactive = FALSE, 
      preferAccepted = TRUE, preferFuzzy = FALSE)$nameMatched, 
    "Quercus alba")
})

# correctTaxo - fuzzy matching 
test_that("correctTaxo selects best fuzzy match when preferFuzzy is TRUE", {
  reset_cache()

  # checkURL returns TRUE always, 
  # to simulate reachable API without pinging API
  mockery::stub(correctTaxo, "checkURL", TRUE)
  
  # Simulate backbone version without pinging API 
  mock_bb <- list(data = list(classifications = list(id = "v2023.12")))
  
  # Mock name matching response with synonym and accepted name
  mock_match <- list(data = list(taxonNameMatch = list(
    match = NULL,
    candidates = list(
      list(
        id = "wfo-1", 
        role = "accepted", 
        fullNameStringNoAuthorsPlain = "Quercus alba",
        authorsString = "L.",
        currentPreferredUsage = list(hasName = list(
          id = "wfo-1", 
          fullNameStringNoAuthorsPlain = "Quercus alba"
        ))
      ),
      list(
        id = "wfo-2", 
        role = "accepted", 
        fullNameStringNoAuthorsPlain = "Quercus albus",
        authorsString = "Mill.",
        currentPreferredUsage = list(hasName = list(
          id = "wfo-2", 
          fullNameStringNoAuthorsPlain = "Quercus albus"
        ))
      )
    )
  )))
  
  # Create mock taxon concept response 
  mock_ranks <- list(data = list(taxonConceptById = list(
    path = list(
      list(hasName = list(rank = "family", fullNameStringNoAuthorsPlain = "Fagaceae")),
      list(hasName = list(rank = "genus", fullNameStringNoAuthorsPlain = "Quercus")),
      list(hasName = list(rank = "species", fullNameStringNoAuthorsPlain = "Quercus alba"))
    )
  )))

  # Stub JSON parsing to return mock data in sequence
  m_json <- mockery::mock(mock_bb, mock_match, mock_ranks)
  mockery::stub(correctTaxo, "httr2::req_perform", list())
  mockery::stub(correctTaxo, "httr2::req_perform_parallel", list(list()))
  mockery::stub(correctTaxo, "httr2::resp_body_json", m_json)

  # Choose closest fuzzy match
  expect_equal(
    correctTaxo("Quercus alba", NULL, interactive = FALSE, 
      preferFuzzy = TRUE)$nameMatched, 
    "Quercus alba")
})

# correctTaxo - empty matched
test_that("correctTaxo returns empty dataframe when nothing matches", {
  reset_cache()

  # checkURL returns FALSE always, to simulate unreachable API
  mockery::stub(correctTaxo, "checkURL", FALSE)
  
  # Suppress expected warning
  suppressWarnings({
    res <- correctTaxo("Quercus", "alba", useCache = TRUE, useAPI = TRUE)
  })
  
  # Return a dataframe with required columns but empty match
  expect_s3_class(res, "data.frame")
  expect_true("nameOriginal" %in% names(res))
  expect_true("nameMatched" %in% names(res))
  expect_true(is.na(res$nameMatched))
})

# correctTaxo - test against CSV
# Does not run during CRAN testing
test_that("correctTaxo handles real API calls", {
  skip_on_cran()
  reset_cache()
  options(wfo.api_uri = "https://list.worldfloraonline.org/gql.php")

  # Import data
  test <- read.csv("../testdata/test_correctTaxo.csv")

  # Run correctTaxo
  # Not interactive, prefer accepted names and best fuzzy matches
  resp <- correctTaxo(
    genus = test$genus,
    species = test$species,
    interactive = FALSE,
    preferAccepted = TRUE,
    preferFuzzy = TRUE)

  # Match expected values in CSV
  expect_equal(resp$nameAccepted, test$nameAccepted)
})

# callAPI - valid API call
test_that("callAPI constructs correct httr2 request", {
  options(wfo.api_uri = "http://example.com/graphql")
  
  vars <- list(searchString = "Burkea africana")
  query <- "query { test }"
  
  req <- callAPI(vars, query, timeout = 15)
  
  expect_s3_class(req, "httr2_request")
  expect_equal(req$url, "http://example.com/graphql")
})

# callAPI - throttling
test_that("callAPI applies throttle settings correctly", {
  req <- callAPI(list(searchString = "test"), "query { }", 
                 capacity = 100, fill_time_s = 120)
  
  expect_s3_class(req, "httr2_request")
})

# pickName - "skip" input
test_that("pickName handles 'S' for skip", {
  # Simulate "S" keystroke
  m <- mockery::mock("S") 
  mockery::stub(pickName, "readline", m)
  
  # Create mock input for pickName
  cand <- list(list(
    id = "wfo-1", 
    fullNameStringNoAuthorsPlain = "Test",
    authorsString = "L.",
    role = "accepted",
    wfoPath = "path"
  ))
  
  # Capture output
  capture.output(res <- pickName("Test", cand))
  
  # S keystroke triggers "SKIP"
  expect_equal(res$method, "SKIP")
  mockery::expect_called(m, 1)
})

# pickName - empty inputs (incl. <Enter>) are interpreted as "skip"
test_that("pickName handles empty string as skip", {
  # Simulate empty keystroke
  m <- mockery::mock("") 
  mockery::stub(pickName, "readline", m)
  
  # Create mock input for pickName
  cand <- list(list(
    id = "wfo-1", 
    fullNameStringNoAuthorsPlain = "Test",
    authorsString = "L.",
    role = "accepted",
    wfoPath = "path"
  ))
  
  # Capture output
  capture.output(res <- pickName("Test", cand))
  
  # Empty keystroke triggers "SKIP"
  expect_equal(res$method, "SKIP")
})

# pickName - empty input
test_that("pickName exits early if no candidates provided", {
  # Simulate empty candidate list
  m <- mockery::mock()
  mockery::stub(pickName, "readline", m)
  
  # Empty candidates list triggers "EMPTY" and exit 
  expect_message(res <- pickName("Fake Name", list()), "No candidates, skipping")
  expect_equal(res$method, "EMPTY")
  mockery::expect_called(m, 0)
})

# pickName - handle selecting candidate by number
test_that("pickName handles valid numeric selection", {
  # Simulate "2" keystroke to select candidate 2
  m <- mockery::mock("2")
  mockery::stub(pickName, "readline", m)
  
  # Create mock input for pickName
  cand <- list(
    list(
      id = "wfo-1", 
      fullNameStringNoAuthorsPlain = "Species A", 
      authorsString = "L.", 
      role = "accepted", 
      wfoPath = "A"
    ),
    list(
      id = "wfo-2", 
      fullNameStringNoAuthorsPlain = "Species B", 
      authorsString = "L.", 
      role = "synonym", 
      wfoPath = "B"
    )
  )
  
  # Capcture output
  capture.output(res <- pickName("Test", cand))
  
  # Selected entry and flag "MANUAL"
  expect_equal(res$id, "wfo-2")
  expect_equal(res$method, "MANUAL")
})

# pickName - paging to next page
test_that("pickName handles paging to next page", {
  # Simulate "11" keystroke to move to next page
  m <- mockery::mock("n", "11")
  mockery::stub(pickName, "readline", m)
  mockery::stub(pickName, "pickName", pickName)
  
  # Create mock input for pickName
  cand <- lapply(1:15, function(i) {
    list(
      id = paste0("wfo-", i), 
      fullNameStringNoAuthorsPlain = "Test", 
      authorsString = "L.", 
      role = "accepted", 
      wfoPath = "path"
    )
  })
  
  # Capture output
  capture.output(res <- pickName("Test", cand))
  
  # Selected entry 11
  expect_equal(res$id, "wfo-11")
  mockery::expect_called(m, 2)
})

# pickName - cycling through pages to the beginning
test_that("pickName prevents paging beyond last page", {
  # Simulate "n" keystroke to select next page, even when <10 candidates
  m <- mockery::mock("n", "s")
  mockery::stub(pickName, "readline", m)
  
  # Create mock input for pickName
  cand <- lapply(1:5, function(i) {
    list(
      id = paste0("wfo-", i), 
      fullNameStringNoAuthorsPlain = "Test", 
      authorsString = "L.", 
      role = "accepted", 
      wfoPath = "path"
    )
  })
  
  # Capture output
  out <- capture.output(res <- pickName("Test", cand))
  
  # Detect output message, then "SKIP"
  expect_true(any(grepl("Already on last page", out)))
  expect_equal(res$method, "SKIP")
})

# pickName - prevent negative paging
test_that("pickName prevents paging before first page", {
  # Simulate "p" keystroke to select previous page, even when on first page
  m <- mockery::mock("p", "s")
  mockery::stub(pickName, "readline", m)
  
  # Create mock input for pickName
  cand <- list(list(
    id = "wfo-1", 
    fullNameStringNoAuthorsPlain = "Test", 
    authorsString = "L.", 
    role = "accepted", 
    wfoPath = "path"
  ))
  
  # Capture output
  out <- capture.output(res <- pickName("Test", cand))
  
  # Detect output message, then "SKIP"
  expect_true(any(grepl("Already on first page", out)))
  expect_equal(res$method, "SKIP")
})

# pickName - select WFO ID directly
test_that("pickName handles manual WFO ID entry", {
  # Simulate manual WFO ID entry keystrokes
  m_input <- mockery::mock("wfo-0000214110")
  mockery::stub(pickName, "readline", m_input)
  
  # Create mock API response
  mock_json <- list(data = list(taxonNameById = list(
    id = "wfo-0000214110",
    fullNameStringNoAuthorsPlain = "Target Species"
  )))
  
  # Stub API calls
  mockery::stub(pickName, "callAPI", function(...) list())
  mockery::stub(pickName, "httr2::req_perform", function(...) list())
  mockery::stub(pickName, "httr2::resp_body_json", mock_json)
  
  # Create mock input for pickName
  cand <- list(list(
    id = "wfo-1", 
    fullNameStringNoAuthorsPlain = "Wrong", 
    authorsString = "L.", 
    role = "accepted", 
    wfoPath = "P"
  ))
  
  # Capture output
  capture.output(res <- pickName("Test", cand))
  
  # Correctly selected WFO ID and assert "MANUAL" selection
  expect_equal(res$id, "wfo-0000214110")
  expect_equal(res$method, "MANUAL")
})

# pickName - invalid keystrokes
test_that("pickName re-prompts on invalid input", {
  # Simulate "99" invalid keystroke
  m <- mockery::mock("hello", "99", "S")
  mockery::stub(pickName, "readline", m)
  
  # Create mock input for pickName
  cand <- list(list(
    id = "wfo-1", 
    fullNameStringNoAuthorsPlain = "Test", 
    authorsString = "L.", 
    role = "accepted", 
    wfoPath = "path"
  ))
  
  # Capture output
  out <- capture.output(res <- pickName("Test", cand))
  
  # Detect output message and "SKIP"
  expect_true(any(grepl("Invalid input", out)))
  expect_equal(res$method, "SKIP")
  mockery::expect_called(m, 3)
})

# Query builder functions
test_that("query functions return valid GraphQL strings", {
  # Check output class
  fields <- wfo_query_fields()
  expect_type(fields, "character")

  # Simply check titles of queries inside each function
  expect_type(query_taxonNameMatch(), "character")
  expect_true(grepl("query NameMatch", query_taxonNameMatch()))
  
  expect_type(query_taxonNameById(), "character")
  expect_true(grepl("query NameByID", query_taxonNameById()))
  
  expect_type(query_taxonConceptById(), "character")
  expect_true(grepl("query ConceptByID", query_taxonConceptById()))
  
  expect_type(query_classifications(), "character")
  expect_true(grepl("classifications", query_classifications()))
})

