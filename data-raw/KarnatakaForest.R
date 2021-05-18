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


# update the tree id: add a letter for each stems if there is different stem
data_Kar[, N := .N, by = treeId]
data_Kar[ N > 1, treeId := paste(treeId, LETTERS[.GRP], sep = "_"), by = stem]



# calculate the D (diameter) form the Tree Girth
data_Kar[, D := TreeGirth / pi]


# build the Karnataka forest data base
KarnatakaForest <- data_Kar[, .(plotId = PlotID, treeId, family = Family, genus, species, D, lat, long)]
setorder(KarnatakaForest, plotId)

setDF(KarnatakaForest)
usethis::use_data(KarnatakaForest, overwrite = FALSE)
