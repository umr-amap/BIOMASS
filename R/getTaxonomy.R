if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "id", "family"
  ))
}


#' Retrieving the taxonomy
#'
#' From a genus, the function `getTaxonomy` finds the APG III family, and optionally the
#' order, from the [genusFamily] database and the [apgFamilies] dataset
#'
#'
#' @param genus Vector of genus names
#' @param findOrder (Boolean) If `TRUE`, the output will contain the taxonomical orders of the families.
#'
#' @return Data frame with the order (if `findOrder` is `TRUE`), family and genus.
#' @author Ariane TANGUY, Arthur PERE, Maxime REJOU-MECHAIN
#' @export
#'
#' @examples
#' # Find the Family of the Aphelandra genus
#' getTaxonomy("Aphelandra")
#' # ... and the order
#' getTaxonomy("Aphelandra", findOrder = TRUE)
#' @importFrom data.table setDF setDT data.table
getTaxonomy <- function(genus, findOrder = FALSE) {
  ### Find the family (and the order) of a vector of genus

  ################## 1. Retrieve the Family

  # Load taxonomical data (sourced from Angiosperm Phylogeny Website, http://www.mobot.org/MOBOT/research/APweb/)
  genusFamily <- setDT(copy(BIOMASS::genusFamily))
  setkey(genusFamily, genus)

  # Create ids
  inputGenus <- data.table(
    id = 1:length(genus), inputGenus = as.character(genus),
    stringsAsFactors = FALSE, key = "inputGenus"
  )

  # Merge the input genera with the genus family table
  genusFam <- merge(inputGenus, genusFamily, by.x = "inputGenus", by.y = "genus", all.x = TRUE)
  genusFam <- genusFam[, .(id, inputGenus, family)]

  ################## 2. Retrieve the Order

  if (findOrder == TRUE) {
    apgFamilies <- setDT(copy(BIOMASS::apgFamilies))

    genusFam <- merge(genusFam, apgFamilies, by.x = "family", by.y = "famAPG", all.x = TRUE)
    genusFam <- genusFam[, .(id, inputGenus, family, order)]
  }

  genusFam <- genusFam[order(id), ]
  genusFam <- setDF(genusFam[, id := NULL])
  return(genusFam)
}
