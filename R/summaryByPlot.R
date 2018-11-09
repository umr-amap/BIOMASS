#' Mixing the tree
#'
#' This function mixes the AGB of the tree that doesn't have a subplot assigned.
#' The AGB for those trees will be added with a random tree (in the same plot or
#' if the tree haven't a plot in the matrix) on each column of the matrix.
#'
#' @param plot Vector with the code of plot
#' @param subplot vector with the code of the subplot
#' @param AGB_simu Matrix which result for the function \code{\link{AGBmonteCarlo}} on the AGB_simu element of the list
#'
#' @return The matrix corrected with the values of the trees who doesn't have a subplot replaced by NA
#' @export
#'
#' @importFrom data.table data.table :=
#' @examples
summaryByPlot <- function(plot, subplot = plot, AGB_simu) {


  # parameters verification -------------------------------------------------
  if (length(plot) != nrow(AGB_simu)) {
    stop("Your plot vector haven't the same length as your number of row in the matrix")
  }
  if (length(subplot) != nrow(AGB_simu)) {
    stop("Your subplot vector haven't the same length as your number of row in the matrix")
  }


  Plot <- data.table(plot = plot, subplot = subplot)
  indice_tree <- Plot[is.na(subplot), .I, by = plot]

  indice_first <- Plot[!is.na(subplot), .(indice_line = first(.I), plot = unique(plot)), by = subplot]

  if (nrow(indice_tree) != 0) {
    # Create a table with I : indice of the tree to distribute
    # indice_line : a random sample of matrix line inside the plot without the trees to distribute
    # indice_col : the column index of the matrix by I
    mySample <- function(plot1, n) {
      if (!is.na(plot1)) {
        return(indice_first[plot == plot1, sample(indice_line, n, replace = T)])
      } else {
        return(samples = indice_first[, sample(indice_line, n, replace = T)])
      }
    }
    n <- ncol(AGB_simu)
    samples <- indice_tree[, .(indice_line = mySample(plot, n), indice_col = 1:n), by = I]

    # remove the index for the tree to distribute when it is NA
    samples <- samples[ !(I %in% unique(which(is.na(AGB_simu), arr.ind = T)[, 1])) ]
  }


  # to have the summary of trees by subplot
  mySummary <- function(x, matrix) {
    resAGB <- colSums(matrix[x, ], na.rm = T)

    if (nrow(indice_tree) != 0) {
      subsample <- samples[x[1] == indice_line, ]

      if (nrow(subsample) != 0) {
        sums <- subsample[, sum(matrix[I, unique(indice_col)], na.rm = T), by = indice_col]
        sums[is.na(V1), V1 := 0]
        resAGB[sums$indice_col] <- resAGB[sums$indice_col] + sums$V1
      }
    }

    return(list(
      AGB = mean(resAGB),
      Cred_2.5 = quantile(resAGB, probs = 0.025),
      Cred_97.5 = quantile(resAGB, probs = 0.975)
    ))
  }

  AGB <- Plot[!is.na(subplot), mySummary(.I, AGB_simu), by = subplot]


  return(AGB)
}
