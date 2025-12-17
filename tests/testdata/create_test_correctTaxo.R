# Datasets creation containing different problem cases.

## Scientific names dataset
inv_test_sci_names <- data.frame(
  sci_names = c(
    # Correct Scientific names
    "Terminalia superba", "Thomandersia hensii", "Guarea glabra",
    # Mispelling on scientific names
    "Termialia  superba", "Thommandersia hensii", "Guarea galbra",
    # Scientific names with authors
    "Terminalia superba Engl. & Diels", "Terminalia superba aut Engl. & Diels"
  )
)
correctTaxo_sci_names <- correctTaxo(genus = inv_test_sci_names$sci_names, accepted = TRUE)

res_sci_names <- cbind(inv_test_sci_names, correctTaxo_sci_names)

## Genus species dataset
inv_test_gen_sp <- data.frame(
  genus = c(
    # Correct genus
    "Terminalia", "Thomandersia", "Guarea",
    # Families instead of genus
    "Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae sp", "Fabacee sp", "Leguminosae",
    # Genus with qualifiers
    "Terminalia sp. superba", "Terminalia cf. superba", "Terminalia aff. superba", 
    # White space issues
    "Terminalia ", " Terminalia", "Terminalia  ",
    # Misspeling on genus
    "Teminalia", "Terminlia", "Termnalia",
    "Thommandersia", "Thommandersio", "Thomadersia",
    # Genus with sp or spp
    "Terminalia sp", "Guarea sp", "Guarea spp", "Guarea sp.",
    # Case-sensitive
    "terminalia", "TERMINALIA",
    # With supspecies
    "Guarea glabra  subsp. excelsa",  "Guarea glabra subsp. excelsa", "Guarea", "Guarea", "Gymnacranthera", "Gymnacranthera"
  ),
  
  species = c(
    # Correct species
    "superba", "hensii", "glabra",
    # Families instead of genus: empty- or NA-species 
    NA, "", " ", NA, "", NA,
    # species with qualifiers
    "superba", "cf. superba", "aff. superba",
    # White space issues
    "superba", "superba ", " superba",
    # Misspeling on species
    "superba", "superb", "suprba",
    "hensii", "hensi", "heensii",
    # species with sp or spp
    NA, "sp", "spp", "sp.",
    # Case-sensitive
    "Superba", "SUPERBA",
    # With supspecies
    "glabra  subsp. excelsa",  "Guarea glabra subsp. excelsa", "glabra subsp. excelsa", "glabra sub excelsa", "farquhariana  var. paniculata", "farquhariana  var paniculata"
  )
)

correctTaxo_gen_sp <- correctTaxo(genus = inv_test_gen_sp$genus, species = inv_test_gen_sp$species, accepted = TRUE)

res_gen_sp <- cbind(inv_test_gen_sp, correctTaxo_gen_sp)

res_expected <- data.frame(
  taxon_family_acc = c(
    # Correct genus
    "Combretaceae", "Thomandersiaceae", "Meliaceae",
    # Families instead of genus
    NA, NA, NA, NA, NA, NA,
    # Genus with qualifiers
    "Combretaceae", "Combretaceae", "Combretaceae", 
    # White space issues
    "Combretaceae", "Combretaceae", "Combretaceae",
    # Misspeling on genus
    "Combretaceae", "Combretaceae", "Combretaceae",
    "Thomandersiaceae", "Thomandersiaceae", "Thomandersiaceae",
    # Genus with sp or spp
    "Combretaceae", "Combretaceae", "Combretaceae", "Combretaceae",
    # Case-sensitive
    "Combretaceae", "Combretaceae",
    # With supspecies
    "Meliaceae", "Meliaceae", "Meliaceae", "Meliaceae", "Myristicaceae", "Myristicaceae"
  ),
  
  taxon_genus_acc = c(
    # Correct genus
    "Terminalia", "Thomandersia", "Guarea",
    # Families instead of genus
    "Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae sp", "Fabacee sp", "Leguminosae",
    # Genus with qualifiers
    "Terminalia", "Terminalia", "Terminalia", 
    # White space issues
    "Terminalia", "Terminalia", "Terminalia",
    # Misspeling on genus
    "Terminalia", "Terminalia", "Terminalia",
    "Thomandersia", "Thomandersia", "Thomandersia",
    # Genus with sp or spp
    "Terminalia", "Guarea", "Guarea", "Guarea",
    # Case-sensitive
    "Terminalia", "Terminalia",
    # With supspecies
    "Guarea", "Guarea", "Guarea", "Guarea", "Gymnacranthera", "Gymnacranthera"
  ),
  
  taxon_species_acc = c(
    # Correct species
    "superba", "hensii", "glabra",
    # Families instead of genus: empty- or NA-species 
    NA, NA, NA, NA, NA, NA,
    # species with qualifiers
    "superba", "superba", "superba",
    # White space issues
    "superba", "superba", "superba",
    # Misspeling on species
    "superba", "superba", "superba",
    "hensii", "hensii", "hensii",
    # species with sp or spp
    NA, NA, NA, NA,
    # Case-sensitive
    "superba", "superba",
    # With supspecies
    "glabra  subsp. excelsa",  "glabra  subsp. excelsa", "glabra  subsp. excelsa", "glabra  subsp. excelsa", "farquhariana  var. paniculata", "farquhariana  var. paniculata"
  )
)

res_gen_sp <- cbind(res_gen_sp, res_expected)

res_gen_sp |> write.csv(file = "data/test_correctTaxo.csv", row.names = FALSE)
