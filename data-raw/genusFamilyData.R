require(rvest)
require(data.table)

fam = read_html("http://www.theplantlist.org/1.1/browse/-/-/")

# extract the list number 7 where we have all the interesting data
subnode = fam %>% html_nodes("ul") %>% .[[7]] %>% html_nodes("li") %>% html_nodes("i")
tab = data.table( class = subnode %>% html_attr("class"),
                  fam = subnode %>% html_text)



# work on the data
tab = data.table( class = tab[class != "family", class], 
                   family = tab[class == "family", fam], 
                   genus = tab[class != "family", fam] )

tab[ , genus := gsub('^.+[^A-z]', '', genus)]


