In this folder you can find all the files needed to process the all the datasets of the BIOMASS package. 

In this file, you have all the file .R which is used to remake the database of the corresponding name. Below the title you have all the file and internet link you need to redo the database.

To remake a database, just go to the end of the file where you see this function :
	usethis::use_data()
and add (or modified) the argument "overwrite" to set this one to "TRUE" in this function.


1. AGP FAMILIES, apgfamily.R

APGIII.csv: comes from the ANGIOSPERM PHYLOGENY WEBSITE, Family names.
	http://www.mobot.org/MOBOT/research/APweb/
	tab families
	You can just copied/paste the text in the corresponding file.

OUTPUT: apgFamilies.csv : a file with 3 columns: 
	- order, 
	- famAPG = family 
	- famSyn = family synonym 



2. GENUS FAMILY CORRESPONDANCE, genusFamily.R

apgFamilies.csv: Output from the first function, it's used for correct the families if they have a family synonym

* THE PLANT LIST

OUTPUT: genList_tpl.csv : comes from THE PLANT LIST website, Browse all Genera 
	http://www.theplantlist.org/1.1/browse/-/-/
	Read directly from the website, to update just remove this file.


* KEW DATABASE

OUTPUT: vascularKew.csv: Take all genus and families from the kew website in two simple steps, first take all the families from http://data.kew.org/vpfg1992/genfile.html and then retrieve the genera thanks to the small script. Can be updated easily by removing this file.



* COMBINE THE DATASETS

The two datasets are then combined, binding them after removing from Kew all genera already in TPL.
OUTPUT: dupGenusToSub.csv: Some genera have two different families, so to determine which family is the correct one, we submit the list of duplicated genera to TNRS. 
	http://tnrs.iplantcollaborative.org/TNRSapp.html
	Take the detailed report, and encoding UTF-8
	Deplace/rename it in the folder 'data-raw/dupGenus_TNRS.txt'

OUTPUT: genusFamily.csv: a file with 2 columns:
	- family
	- genus


3. Global wood density, globalwd.R

OUTPUT: wdData.csv: download the file and read it from the website : http://datadryad.org/bitstream/handle/10255/dryad.235/GlobalWoodDensityDatabase.xls?sequence=1



4. Karnataka forest, KarnatakaForest.R

INPUT: 
	- Macroplot_data_Rev.txt
	- Species_list.txt
	- Site_variables.txt
	
	Those files contain all the data needed for Karnataka Forest database.



5. NouraguesHD

	For this one, we lost the code to reconstruct the database, but this isn't false data



6. sd_10, sd_10.R

INPUT: the data from the table wdData, see 3. 



7. param_4, param_4.R

INPUT: Allometry-database-2012m_data.csv



8. param_7, param_7.R

INPUT:
	- Allometry-database-2012m_data.csv
	- Allometry-database-2012n_envsub.csv


9. Feldpausch Coefficient
	For this one there is no code just it's a copy/paste of the table 3 on the paper: Feldpausch et al. 2012.
