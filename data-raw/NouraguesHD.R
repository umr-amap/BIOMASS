require(data.table)

NouraguesHD <- fread(file = "data-raw/NouraguesHD.csv")
setnames(NouraguesHD, colnames(NouraguesHD), c("plotId", "D", "H"))

NouraguesHD[plotId == "Plot1", ":="(lat = 4.068246, long = -52.68831)]
NouraguesHD[plotId == "Plot2", ":="(lat = 4.036816, long = -52.67368)]
NouraguesHD <- na.omit(NouraguesHD)

setDF(NouraguesHD)
usethis::use_data(NouraguesHD, overwrite = F)
