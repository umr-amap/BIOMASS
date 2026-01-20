# Create dataframe with genus and species combinations and expected matched name
out <- as.data.frame(
  matrix(
    c(
      # Correct genus
      "Terminalia", "superba", "Terminalia superba",
      "Thomandersia", "hensii", "Thomandersia hensii",
      "Guarea", "glabra", "Guarea glabra",

      # Families instead of genus
      "Fabaceae", NA_character_, "Fabaceae",
      "Fabaceae", "", "Fabaceae",
      "Fabaceae", " ", "Fabaceae",
      "Fabaceae sp", NA_character_, "Fabaceae",
      "Fabacee sp", "", "Fabaceae",
      "Leguminosae", NA_character_, "Fabaceae",

      # White space issues (preserved)
      "Terminalia ", "superba", "Terminalia superba",
      " Terminalia", "superba ", "Terminalia superba",
      "Terminalia  ", " superba", "Terminalia superba",

      # Misspelling on genus
      "Burke", "africana", "Burkea africana",
      "Brke", "africn", NA_character_,
      "Thommandersia", "hensii", "Thomandersia hensii",
      "Thommandersio", "hensi", NA_character_,

      # Genus with sp or spp
      "Terminalia sp", NA, "Terminalia",
      "Guarea sp", "sp", "Guarea",
      "Guarea spp", "spp", "Guarea",
      "Guarea sp.", "sp.", "Guarea",

      # Case-sensitive
      "terminalia", "Superba", "Terminalia superba",
      "TERMINALIA", "SUPERBA", "Terminalia superba",

      # With subspecies
      "Guarea glabra  subsp. excelsa", "glabra  subsp. excelsa", "Guarea glabra subsp. excelsa",
      "Guarea glabra subsp. excelsa", "Guarea glabra subsp. excelsa", "Guarea glabra subsp. excelsa",
      "Guarea", "glabra subsp. excelsa", "Guarea glabra subsp. excelsa",
      "Guarea", "glabra sub excelsa", "Guarea glabra",
      "Gymnacranthera", "farquhariana  var. paniculata", "Gymnacranthera farquhariana var. paniculata",
      "Gymnacranthera", "farquhariana  var paniculata", "Gymnacranthera farquhariana var. paniculata"
    ),
    ncol = 3,
    byrow = TRUE
  ),
  stringsAsFactors = FALSE
)
names(out) <- c("genus", "species", "nameAccepted")

resp <- correctTaxo(
  genus = out$genus,
  species = out$species,
  interactive = FALSE,
  preferAccepted = TRUE,
  preferFuzzy = TRUE)

stopifnot(out$nameAccepted == resp$nameAccepted)

write.csv(out, file = "./test_correctTaxo.csv", row.names = FALSE)
