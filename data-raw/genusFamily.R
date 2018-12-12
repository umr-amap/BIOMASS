require(rvest)
require(data.table)


# create from internet the file -------------------------------------------



if (!file.exists("data-raw/genList_tpl.csv")) {
  # Take all the genus from the genus list of the plant list
  fam <- read_html("http://www.theplantlist.org/1.1/browse/-/-/")

  # extract the list number 7 where we have all the interesting data
  textFam <- fam %>% html_nodes("ul") %>% html_nodes("li") %>% html_text()

  # work on the data, separate the class and family and genus
  textFam <- gsub("\\n|\\t", "", textFam)
  textFam <- textFam[grepl("[A-Z].+ \\([A-Z][a-z]+\\)", textFam)]

  tplList <- data.table(
    family = gsub(".+(?:\\()|\\)", "", textFam),
    genus = gsub(" \\(.+\\)", "", textFam)
  )
  rm(fam)

  # remove all the space at the begining of the genus
  tplList[, genus := gsub("^.[^A-z]", "x ", genus)]

  tplList[grepl("^Leguminosae-", family), family := "Fabaceae"]

  # write the file
  fwrite(tplList, file = "data-raw/genList_tpl.csv")
}

tplList <- fread("data-raw/genList_tpl.csv")


# correct the family ------------------------------------------------------



# take apg families
if (!file.exists("data-raw/apgFamilies.csv")) source("data-raw/apgfamily.R")
apgFamilies <- fread("data-raw/apgFamilies.csv")



# Correct the families (take the apg ones) thanks to apgFamilies (from before)
tplList[apgFamilies[, .(famAPG, family = famSyn)], on = "family", family := i.famAPG]














# TO UPDATE KEW GENERA --------------------------------

if (!file.exists("data-raw/vascularKew.csv")) {
  listFam <- read_html("http://data.kew.org/vpfg1992/genlist.html")
  listFam <- listFam %>% html_nodes("ul") %>% html_nodes("li") %>% html_text()
  listFam <- listFam[grepl("[A-Z]{2,}", listFam)]
  listFam <- listFam[!grepl("^FAQ$", listFam)]
  listFam <- gsub(" |\\n|\\t", "", listFam)
  listFam <- sort(listFam)

  # And retrieve all the .txt who contains all the genera / family

  file <- rbindlist(lapply(listFam, function(x) {
    file <- NULL
    while (!is.data.frame(file)) {
      file <- try(read.csv(paste("http://data.kew.org/vpfg1992/", x, ".TXT", sep = ""), header = F, stringsAsFactors = F), silent = T)
      Sys.sleep(0.5)
    }
    print(x)
    colnames(file) <- c("num", "smthg", "genus", "Author", "numAgain")
    file$family <- paste(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))), sep = "")

    return(file)
  }))
  write.csv(file, "data-raw/vascularKew.csv", row.names = F)
}





# kew ---------------------------------------------------------------------

kew <- fread("data-raw/vascularKew.csv")

# Correct all family with Leguminosae-
kew[grepl("^Leguminosae-", family), family := "Fabaceae"]


# Correct the families (take the apg ones)
kew[apgFamilies[, .(famAPG, family = famSyn)], on = "family", family := i.famAPG]
kew[, ":="(num = NULL, smthg = NULL, Author = NULL, numAgain = NULL)]

# take all the genus we don't have in "the plant list"
kew <- kew[!tplList, on = "genus"]









# Add evrything -----------------------------------------------------------

# Add the remaining Kew genera
genusFamily <- rbind(tplList, kew)
genusFamily <- unique(genusFamily[base::order(genus)])

# Do something for the duplicated genus
dup <- unique(genusFamily[duplicated(genus), genus])
write.csv(dup, "data-raw/dupGenusToSub.csv", quote = F)

if (!file.exists("data-raw/dupGenus_TNRS.txt")) {
  stop(
    "Please submite the file 'dupGenusToSub.csv' to this site:\n\t\t",
    "http://tnrs.iplantcollaborative.org/TNRSapp.html\n",
    "\t take the detailed report, in UTF-8\n",
    "\t and deplace/rename it in the folder 'data-raw/dupGenus_TNRS.txt'"
  )
}


dupGen <- fread("data-raw/dupGenus_TNRS.txt")
dupGen <- dupGen[Overall_score != 0]


dupGen <- unique(dupGen[(Selected), .(Name_matched, Name_matched_accepted_family, Selected)])
genusFamily[dupGen[, .(genus = Name_matched, family = Name_matched_accepted_family)],
  on = "genus", family := i.family
]






# correct few families ------------------------------------------------------

genusFamily[family == "Isoëtaceae", family := "Isoetaceae"]
genusFamily[genusFamily$genus == "Acanthocladium", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Acanthodium", "family"] <- "Acanthaceae"
genusFamily[genusFamily$genus == "Acosta", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Adelia", "family"] <- "Euphorbiaceae"
genusFamily[genusFamily$genus == "Alliaria", "family"] <- "Meliaceae"
genusFamily[genusFamily$genus == "Amblytropis", "family"] <- "Leguminosae"
genusFamily[genusFamily$genus == "Anacampseros", "family"] <- "Anacampserotaceae"
genusFamily[genusFamily$genus == "Arthraxon", "family"] <- "Loranthaceae"
genusFamily[genusFamily$genus == "Asteriscium", "family"] <- "Apiaceae"
genusFamily[genusFamily$genus == "Balbisia", "family"] <- "Vivianiaceae"
genusFamily[genusFamily$genus == "Banalia", "family"] <- "Salicaceae"
genusFamily[genusFamily$genus == "Bartonia", "family"] <- "Gentianaceae"
genusFamily[genusFamily$genus == "Bassia", "family"] <- "Amaranthaceae"
genusFamily[genusFamily$genus == "Benthamia", "family"] <- "Boraginaceae"
genusFamily[genusFamily$genus == "Blechum", "family"] <- "Acanthaceae"
genusFamily[genusFamily$genus == "Blyttia", "family"] <- "Apocynaceae"
genusFamily[genusFamily$genus == "Bonapartea", "family"] <- "Asparagaceae"
genusFamily[genusFamily$genus == "Brachyloma", "family"] <- "Ericaceae"
genusFamily[genusFamily$genus == "Brachypodium", "family"] <- "Poaceae"
genusFamily[genusFamily$genus == "Bridgesia", "family"] <- "Sapindaceae"
genusFamily[genusFamily$genus == "Brocchia", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Brodiaea", "family"] <- "Amaryllidaceae"
genusFamily[genusFamily$genus == "Brugmansia", "family"] <- "Solanaceae"
genusFamily[genusFamily$genus == "Bruguiera", "family"] <- "Rhizophoraceae"
genusFamily[genusFamily$genus == "Brya", "family"] <- "Leguminosae"
genusFamily[genusFamily$genus == "Bulbostylis", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Burnettia", "family"] <- "Orchidaceae"
genusFamily[genusFamily$genus == "Calophyllum", "family"] <- "Calophyllaceae"
genusFamily[genusFamily$genus == "Candollea", "family"] <- "Dilleniaceae"
genusFamily[genusFamily$genus == "Cedrus", "family"] <- "Pinaceae"
genusFamily[genusFamily$genus == "Cladocolea", "family"] <- "Loranthaceae"
genusFamily[genusFamily$genus == "Coelidium", "family"] <- "Leguminosae"
genusFamily[genusFamily$genus == "Condalia", "family"] <- "Rhamnaceae"
genusFamily[genusFamily$genus == "Corynotheca", "family"] <- "Xanthorrhoeaceae"
genusFamily[genusFamily$genus == "Ctenium", "family"] <- "Poaceae"
genusFamily[genusFamily$genus == "Cuspidaria", "family"] <- "Bignoniaceae"
genusFamily[genusFamily$genus == "Cycnia", "family"] <- "Rosaceae"
genusFamily[genusFamily$genus == "Cynoctonum", "family"] <- "Apocynaceae"
genusFamily[genusFamily$genus == "Cynodon", "family"] <- "Poaceae"
genusFamily[genusFamily$genus == "Drummondia", "family"] <- "Orthotrichaceae"
genusFamily[genusFamily$genus == "Dunalia", "family"] <- "Solanaceae"
genusFamily[genusFamily$genus == "Dusenia", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Duvalia", "family"] <- "Apocynaceae"
genusFamily[genusFamily$genus == "Esenbeckia", "family"] <- "Rutaceae"
genusFamily[genusFamily$genus == "Gilibertia", "family"] <- "Meliaceae"
genusFamily[genusFamily$genus == "Goeppertia", "family"] <- "Lauraceae"
genusFamily[genusFamily$genus == "Griffithia", "family"] <- "Rubiaceae"
genusFamily[genusFamily$genus == "Guidonia", "family"] <- "Salicaceae"
genusFamily[genusFamily$genus == "Haemanthus", "family"] <- "Amaryllidaceae"
genusFamily[genusFamily$genus == "Harrisonia", "family"] <- "Rutaceae"
genusFamily[genusFamily$genus == "Harveya", "family"] <- "Orobanchaceae"
genusFamily[genusFamily$genus == "Hedwigia", "family"] <- "Burseraceae"
genusFamily[genusFamily$genus == "Heeria", "family"] <- "Anacardiaceae"
genusFamily[genusFamily$genus == "Holboellia", "family"] <- "Lardizabalaceae"
genusFamily[genusFamily$genus == "Humboldtia", "family"] <- "Leguminosae"
genusFamily[genusFamily$genus == "Hygrophila", "family"] <- "Acanthaceae"
genusFamily[genusFamily$genus == "Isoloma", "family"] <- "Gesneriaceae"
genusFamily[genusFamily$genus == "Kaulfussia", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Lasia", "family"] <- "Araceae"
genusFamily[genusFamily$genus == "Laxmannia", "family"] <- "Asparagaceae"
genusFamily[genusFamily$genus == "Leersia", "family"] <- "Poaceae"
genusFamily[genusFamily$genus == "Lepidostemon", "family"] <- "Convolvulaceae"
genusFamily[genusFamily$genus == "Limnobium", "family"] <- "Hydrocharitaceae"
genusFamily[genusFamily$genus == "Malacocarpus", "family"] <- "Zygophyllaceae"
genusFamily[genusFamily$genus == "Massularia", "family"] <- "Rubiaceae"
genusFamily[genusFamily$genus == "Microstegium", "family"] <- "Poaceae"
genusFamily[genusFamily$genus == "Mollia", "family"] <- "Malvaceae"
genusFamily[genusFamily$genus == "Moniera", "family"] <- "Rutaceae"
genusFamily[genusFamily$genus == "Myconia", "family"] <- "Compositae"
genusFamily[genusFamily$genus == "Myrmecophila", "family"] <- "Orchidaceae"
genusFamily[genusFamily$genus == "Oreocallis", "family"] <- "Ericaceae"
genusFamily[genusFamily$genus == "Oxymitra", "family"] <- "Annonaceae"
genusFamily[genusFamily$genus == "Pachyneurum", "family"] <- "Brassicaceae"
genusFamily[genusFamily$genus == "Pancovia", "family"] <- "Sapindaceae"
genusFamily[genusFamily$genus == "Parsonsia", "family"] <- "Apocynaceae"
genusFamily[genusFamily$genus == "Phalangium", "family"] <- "Asparagaceae"
genusFamily[genusFamily$genus == "Pseudogaltonia", "family"] <- "Asparagaceae"
genusFamily[genusFamily$genus == "Rauia", "family"] <- "Rutaceae"
genusFamily[genusFamily$genus == "Rhynchocarpa", "family"] <- "Leguminosae"
genusFamily[genusFamily$genus == "Richardia", "family"] <- "Araceae"
genusFamily[genusFamily$genus == "Rostellaria", "family"] <- "Sapotaceae"
genusFamily[genusFamily$genus == "Solmsia", "family"] <- "Thymelaeaceae"
genusFamily[genusFamily$genus == "Swartzia", "family"] <- "Leguminosae"
genusFamily[genusFamily$genus == "Thomandersia", "family"] <- "Thomandersiaceae"
genusFamily[genusFamily$genus == "Tulasnea", "family"] <- "Melastomataceae"
genusFamily[genusFamily$genus == "Washingtonia", "family"] <- "Arecaceae"
genusFamily[genusFamily$genus == "Zappania", "family"] <- "Lamiaceae"
genusFamily[genusFamily$genus == "Zieria", "family"] <- "Rutaceae"









# Build the data ----------------------------------------------------------


genusFamily <- unique(genusFamily)
fwrite(genusFamily, file = "data-raw/genusFamily.csv")
setDF(genusFamily)
usethis::use_data(genusFamily, compress = "xz")




genusFamily <- genusFamily[base::order(family)]
genusFamilyOrg <- setDT(copy(BIOMASS::genusFamily))
genusFamilyOrg <- genusFamilyOrg[base::order(family)]

result <- merge(genusFamilyOrg, genusFamily, all = T, by = "genus")
result <- result[is.na(family.x) | is.na(family.y) | family.x != family.y]

result <- merge(result, apgFamilies, by.x = "family.y", by.y = "famAPG", all.x = T)
result <- result[family.x != famSyn | is.na(family.x) | is.na(family.y)]

setnames(result, c("family.x", "family.y"), c("family_old", "family_new"))

fwrite(result, "../result_family.csv")
