require(data.table)
require(rvest)
require(taxize)


a = setDT( taxize::apgFamilies() )
a[family == "Iso&euml;taceae", family := "Isoetaceae"]

setnames(a, c("family", "synonym"), c("famAPG", "famSyn"))

# Save the file with the old families (to be used to correct the families with the last updated ones)
apgFamilies <- unique(a[, .(order, famAPG, famSyn)])
apgFamilies[is.na(famSyn), famSyn := famAPG]

fwrite(apgFamilies, "data-raw/apgFamilies.csv")

apgFamilies[, famSyn := NULL]
apgFamilies <- unique(apgFamilies)

setDF(apgFamilies)
usethis::use_data(apgFamilies, compress = "xz", overwrite = F)
