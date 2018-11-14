require(data.table)

# Construct the APGII families dataset
apg = fread("data-raw/APGIII.csv", header = F, sep = "\t", quote = "")
apg = apg[!grepl("^$", V1), ]
apg[, c("fam1", "fam2", "order") := tstrsplit(V1, "=")]
apg[, ':='(fam1 = gsub('(^ )|( $)', "", fam1), fam2 = gsub('(^ )|( $)', '', fam2), order = gsub('(^ )|( $)', '', order))]

apg[, V1 := NULL]

# When the order is NA but fam2 isn't : means that fam2 contains the order
apg[is.na(order), ':='(order = fam2, fam2 = NA_character_)]


# Some families have bad character inside, change them
apg[fam1 == "Montsechiaceae (extinct)", fam1 := "Montsechiaceae"]
apg[fam1 == "[Acanthochlamydaceae]", fam1 := "Acanthochlamydaceae"]
apg[fam1 == "Isoëtaceae", fam1 := "Isoetaceae"]
apg[fam2 == "?Gelsemiaceae", fam2 := "Gelsemiaceae"]

# Create a new column with the apg families (means, the closest ones from the order on the website)
apg[!is.na(fam2), famAPG := fam2]
apg[is.na(fam2), famAPG := fam1]

# Delete family2
apg[, fam2 := NULL]

# Change the column names
apgFamilies = unique( apg[base::order(order), .(order, famAPG, famSyn = fam1)] )

# Check duplicated families
a = unique(apgFamilies[, .(order, famAPG)])
a = a[famAPG %in% famAPG[duplicated(famAPG)], ]

# And correct the duplicates
apgFamilies[famAPG == "Equisetaceae", order := "Equisetales"]
apgFamilies[famAPG == "Orchidaceae", order := "Asparagales"]
apgFamilies[famAPG == "Sabiaceae", order := "Proteales"]

# Save the file with the old families (to be used to correct the families with the last updated ones)
apgFamilies = unique(apgFamilies)
a = apgFamilies
fwrite(apgFamilies, file = "apgFamilies.csv", row.names = F)

apgFamilies[, famSyn := NULL]
apgFamilies = unique(apgFamilies)

usethis::use_data(apgFamilies, compress = 'xz')