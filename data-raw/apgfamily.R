require(data.table)
require(rvest)
require(taxize)


a <- setDT(taxize::apgFamilies())
a[family == "Iso&euml;taceae", family := "Isoetaceae"]
a[, family := gsub("\"", "", family)]

setnames(a, c("family", "synonym"), c("famAPG", "famSyn"))

# Save the file with the old families (to be used to correct the families with the last updated ones)
apgFamilies <- unique(a[, .(order, famAPG, famSyn)])
apgFamilies[is.na(famSyn), famSyn := famAPG]

fwrite(apgFamilies, "data-raw/apgFamilies.csv")

apgFamilies[, famSyn := NULL]
apgFamilies <- unique(apgFamilies)

setDF(apgFamilies)
usethis::use_data(apgFamilies, compress = "xz", overwrite = FALSE)


apgFamiliesOrg <- setDT(copy(BIOMASS::apgFamilies))
apgFamiliesOrg <- apgFamiliesOrg[base::order(order)]
apgFamilies <- apgFamilies[base::order(order)]

result <- merge(apgFamiliesOrg, apgFamilies, all = T, by = "famAPG")
result <- result[is.na(order.x) | is.na(order.y) | order.x != order.y]
setnames(result, c("order.x", "order.y"), c("order_old", "order_new"))

fwrite(result, "../result_order.csv")
