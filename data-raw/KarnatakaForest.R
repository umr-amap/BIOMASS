require(data.table)

# read the data
dataflo <- fread("data-raw/Macroplot_data_Rev.txt")
datasp <- fread("data-raw/Species_list.txt")
datalat <- fread("data-raw/Site_variables.txt")

# merge the data
datatot <- merge(dataflo, datasp, by = "SpCode")
datatot[datalat, on = "PlotID", ":="(lat = i.LatDec, long = i.LongDec)]

# split the Name into genus and species
datatot[, c("genus", "species") := tstrsplit(Name, " ")]

# remove the column that are useless
datatot[, ":="(SpCode = NULL, Name = NULL, Author = NULL, Synonym = NULL, Habit = NULL, Pheno = NULL, WGEnd = NULL)]

# Give the Id of the trees: the PlotID and a unique number restored to one for each plot id
datatot[, treeId := paste(PlotID, seq(.N), sep = "_"), by = PlotID]

# melt the table, regroup all the tree gitrh in two column, the column stem (the past 5 columns TreeGirth)
# and TreeGrith the values of those columns
data_Kar <- melt(datatot,
  measure.vars = grep("TreeGirth", colnames(datatot)),
  variable.name = "stem",
  value.name = "TreeGirth"
)


# remove the line where the Tree Girth equal to 0
data_Kar <- data_Kar[TreeGirth != 0]


# update the tree id: add a letter for each stems
data_Kar[, treeId := paste(treeId, LETTERS[.GRP], sep = "_"), by = stem]



# calculate the D (diameter) form the Tree Girth
data_Kar[, D := TreeGirth / pi]


# build the Karnataka forest data base
KarnatakaForest <- data_Kar[, .(plotId = PlotID, treeId, family = Family, genus, species, D, lat, long)]
setorder(KarnatakaForest, plotId)


usethis::use_data(KarnatakaForest)

















KarnatakaForestB <- setDT(copy(BIOMASS::KarnatakaForest))

KarnatakaForest$genus


HDmodel <- modelHD(NouraguesHD$D, NouraguesHD$H, useWeight = T, method = "log2")
KarnatakaForest[, c("H", "RSE") := retrieveH(D = D, model = HDmodel) ]
KarnatakaForestB[, c("H", "RSE") := retrieveH(D = D, model = HDmodel) ]


KarnatakaForest[, c("genusCorr", "speciesCorr") := correctTaxo(genus, species)[, 1:2]]
KarnatakaForestB[, c("genusCorr", "speciesCorr") := correctTaxo(genus, species)[, 1:2]]

KarnatakaForest[, c("WD", "sdWD") := getWoodDensity(genus = genusCorr, species = speciesCorr, family = family, stand = plotId)[, c("meanWD", "sdWD")]]
KarnatakaForestB[, c("WD", "sdWD") := getWoodDensity(genus = genusCorr, species = speciesCorr, family = family, stand = plotId)[, c("meanWD", "sdWD")]]

resultMC <- AGBmonteCarlo(
  D = KarnatakaForest$D,
  WD = KarnatakaForest$WD,
  errWD = KarnatakaForest$sdWD,
  HDmodel = HDmodel,
  Dpropag = "chave2004"
)

resultMCB <- AGBmonteCarlo(
  D = KarnatakaForestB$D,
  WD = KarnatakaForestB$WD,
  errWD = KarnatakaForestB$sdWD,
  HDmodel = HDmodel,
  Dpropag = "chave2004"
)

result <- summaryByPlot(plot = KarnatakaForest$plotId, AGB_simu = resultMC$AGB_simu)
resultB <- summaryByPlot(plot = KarnatakaForestB$plotId, AGB_simu = resultMCB$AGB_simu)


ord <- order(result$AGB)
result <- result[ord, ]
resultB <- resultB[ord, ]

plot(seq_along(ord), result$AGB, cex = 0.2, ylim = c(0, max(result$Cred_97.5, resultB$Cred_97.5)))
points(seq_along(ord) + 0.5, resultB$AGB, cex = 0.2, col = "red")

segments(seq_along(ord), result$Cred_2.5, seq_along(ord), result$Cred_97.5)
segments(seq_along(ord) + 0.5, resultB$Cred_2.5, seq_along(ord) + 0.5, resultB$Cred_97.5, col = "red")
legend("topleft", legend = c("orignal", "modified"), pch = 1, col = c("red", "black"))

plot(resultB$AGB, result$AGB, asp = 1, ylab = "AGB from modified", xlab = "AGB from original")
