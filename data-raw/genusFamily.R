require(rvest)
require(data.table)


# Take all the genus from the genus list of the plant list
fam = read_html("http://www.theplantlist.org/1.1/browse/-/-/")

# extract the list number 7 where we have all the interesting data
subnode = fam %>% html_nodes("ul") %>% .[[7]] %>% html_nodes("li") %>% html_nodes("i")
tplList = data.table( class = subnode %>% html_attr("class"),
                      fam = subnode %>% html_text)
rm(fam, subnode)


# work on the data, separate the class and family and genus
tplList = data.table( class = tplList[class != "family", class], 
                      family = tplList[class == "family", fam], 
                      genus = tplList[class != "family", fam] )

# remove all the space at the begining of the genus
tplList[ , genus := gsub('^.+[^A-z]', '', genus)]



source("data-raw/apgfamily.R")
apgFamilies = fread("data-raw/apgFamilies.csv")













# Correct the families (take the apg ones) thanks to apgFamilies (from before)
tplList = merge(tplList, apgFamilies, by.x = "family", by.y = "famSyn", all.x = T)
tplList$family[!is.na(tplList$famAPG)] = tplList$famAPG[!is.na(tplList$famAPG)]

tplList$order = tplList$famAPG = NULL

#### KEW

opt = 1

if(opt == 1)
{
  ## OPTION 1 :  TAKE DIRECTLY FROM KEW
  
  #  Take all the genus from kew (see below)
  kew = read.csv("vascularKew.csv")
  kew$family[kew$family == "Leguminosae-caesalpinioideae"] = "Fabaceae"
  kew$family[kew$family == "Leguminosae-mimosoideae"] = "Fabaceae"
  kew$family[kew$family == "Leguminosae-papilionoideae"] = "Fabaceae"
  
  # Correct the families (take the apg ones)
  kew = merge(kew, apgFamilies, by.x = "family", by.y = "famSyn", all.x = T)
  kew$family[!is.na(kew$famAPG)] = kew$famAPG[!is.na(kew$famAPG)]
  kew$order = kew$famAPG = NULL
  
  kew = unique(kew[!kew$genus %in% tplList$genus, ])
}
else
{
  ## OPTION 2 : TAKE FROM MAXIME'S DATABASE
  
  # Create a data base with the names absent in genList, to be checked later by TNRS
  
  kew = unique(read.csv("kewGenera/KewGenera.csv")[, c("familyAPGIII", "accepted")])
  colnames(kew) = c("family", "genus")
  kew$genus = gsub("? ", "x ", kew$genus, fixed = T)
  kew = kew[!is.na(kew$genus), ]
  
  # Correct the families (take the apg ones)
  kew = merge(kew, apgFamilies, by.x = "family", by.y = "famSyn", all.x = T)
  kew$family[!is.na(kew$famAPG)] = kew$famAPG[!is.na(kew$famAPG)]
  kew$order = kew$famAPG = NULL
  kew = kew[!kew$genus %in% tplList$genus, ]
  
  #### CHECK KEW
  
  rm(list = setdiff(ls(), c("apgFamilies", "tplList")))
  
  # Keep only the ones that are not in tpl
  kewTNRS = read.csv("kewGenera/KewSubmitResults.csv")[, c("Name_submitted", "Name_matched", 
                                                           "Name_matched_accepted_family", "Accepted_name", 
                                                           "Accepted_name_family", "Selected", "Overall_score")]
  kewTNRS = kewTNRS[kewTNRS$Selected, ]
  kewTNRS = unique(kewTNRS[!is.na(kewTNRS$Name_submitted), ])
  kewTNRS = kewTNRS[kewTNRS$Overall_score >= 0.5, ]
  
  # When no accepted name and name matched == name submitted
  filt = kewTNRS$Accepted_name == "" & kewTNRS$Name_submitted == kewTNRS$Name_matched
  kewTNRS$Accepted_name[filt] = kewTNRS$Name_submitted[filt]
  kewTNRS$Accepted_name_family[filt] = kewTNRS$Name_matched_accepted_family[filt]
  
  filt = kewTNRS$Accepted_name == ""
  kewTNRS$Accepted_name[filt] = kewTNRS$Name_matched[filt]
  
  kewTNRS = kewTNRS[!kewTNRS$Accepted_name %in% tplList$genus, ]
  kewTNRS = kewTNRS[!kewTNRS$Name_matched_accepted_family != kewTNRS$Accepted_name_family, ]
  
  a = kewTNRS[kewTNRS$genus %in% tplList$genus, ]
  
  kew = unique(kewTNRS[, c("Accepted_name", "Accepted_name_family")])
  colnames(kew) = c("genus", "family")
}

#### ADD EVERYTHING

# Add the remaining Kew genera
genusFamily = rbind(tplList, kew)
genusFamily = unique(genusFamily[order(genusFamily$genus), ])

# Do something for the duplicated genus
dup = unique(genusFamily$genus[genusFamily$genus %in% genusFamily$genus[duplicated(genusFamily$genus)]])
# write.csv(dup, "kewGenera/dupGenusToSub.csv", row.names = F)
if(opt == 1)
  dupGen = read.csv("dupGenus_TNRS_kew.csv") # result from TNRS of the duplicated genera (those who have more than 1 family)
else
  dupGen = read.csv("dupGenus_TNRS.csv")
dupGen = unique(dupGen[, c("Name_matched", "Name_matched_accepted_family", "Selected")])
dupGen = dupGen[dupGen$Selected, ] # select valid name from TNRS

genusFamily = genusFamily[!genusFamily$genus %in% dupGen$Name_matched, ]
genusFamily = rbind(genusFamily, data.frame(genus = dupGen$Name_matched, family = dupGen$Name_matched_accepted_family))

genusFamily$family[genusFamily$family %in% "Isoëtaceae"] = "Isoetaceae"

write.csv(genusFamily, file = "genusFamily.csv", row.names = F)
save(genusFamily, file = "genusFamily.rda", compress = 'xz')
#






######## TO UPDATE KEW GENERA

# List of all the families in http://data.kew.org/vpfg1992/genfile.html
listFam = read.csv("famList_kew.csv", header = F)$V1

# And retrieve all the .txt who contains all the genera / family
fam = listFam[1]
file = read.csv(paste("http://data.kew.org/vpfg1992/", fam, ".TXT", sep = ""), header = F)
colnames(file) = c("num","smthg","genus","Author","numAgain")
file$family = paste(substr(fam, 1, 1), tolower(substr(fam, 2, nchar(fam))), sep = "")

for(fam in listFam[2:length(listFam)])
{
  tmp = read.csv(paste("http://data.kew.org/vpfg1992/", fam, ".TXT", sep = ""), header = F)
  colnames(tmp) = c("num","smthg","genus","Author","numAgain")
  tmp$family = paste(substr(fam, 1, 1), tolower(substr(fam, 2, nchar(fam))), sep = "")
  file = rbind(file, tmp)
}
file$Genus[file$smthg %in% "X"] = paste("x", file$Genus[file$smthg %in% "X"])
vascularKew = unique(file[, c("genus", "family")])
write.csv(vascularKew, file = "kewGenera/vascularKew.csv", row.names = F)



# # Compare it to maxime's kew
# a = kewTNRS[!kewTNRS$genus %in% vascularKew$genus, ]
# 
# kewTNRS$gen = sapply(strsplit(kewTNRS$genus, " ", fixed = T), "[", 1)
# kewTNRS[grepl(" ", kewTNRS$gen), ]
# a = kewTNRS[!kewTNRS$gen %in% vascularKew$genus & !kewTNRS$gen %in% tplList$genus, ]
# 
# b = vascularKew[!vascularKew$genus %in% kewTNRS$genus, ]