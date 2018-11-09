#' Mixing the tree
#'
#' This function mixes the AGB of the tree that doesn't have a subplot assigned.
#' The AGB for those trees will be added with a random tree (in the same plot or
#' if the tree haven't a plot in all the plot) on each column of the matrix.
#'
#' @param plot Vector with the code of plot
#' @param subplot vector with the code of the subplot
#' @param AGB_simu Matrix which result for the function \code{\link{AGBmonteCarlo}} on the AGB_simu element of the list
#'
#' @return a data frame where :
#' \describe{
#'    \item[plot] if you just put the argument plot without the subplot, it's the code of the plot
#'    \item[subplot] if you just put the argument plot and the subplot, it's the code of the subplot
#'    \item[AGB] the mean of AGBfor the plot or subplot
#'    \item[Cred_2.5] the 2.5% credibility intervalle for the plot or subplot
#'    \item[Cred_97.5] the 97.5% credibility intervalle for the plot or subplot
#' }
#'
#' @export
#'
#' @importFrom data.table data.table :=
#' @importFrom stats quantile
#' @examples
#'
#' # Load a database
#' data(NouraguesHD)
#' data(KarnatakaForest)
#'
#' # Modelling height-diameter relationship
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
#'
#' # Retrieving wood density values
#' KarnatakaWD <- getWoodDensity(KarnatakaForest$genus, KarnatakaForest$species,
#'                               stand = KarnatakaForest$plotId)
#'
#' # Propagating errors with a standard error in wood density in one plot
#' filt <- KarnatakaForest$plotId %in% c("BSP20", "BSP14")
#' resultMC <- AGBmonteCarlo(D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt],
#'                           errWD = KarnatakaWD$sdWD[filt], HDmodel = HDmodel)
#'
#' plot <- KarnatakaForest$plotId[ KarnatakaForest$plotId %in% c("BSP20", "BSP14") ]
#'
#' # The summary by plot
#' summaryByPlot(plot, AGB_simu = resultMC$AGB_simu)
#'
summaryByPlot <- function(plot, subplot = plot, AGB_simu) {


  # parameters verification -------------------------------------------------
  if (length(plot) != nrow(AGB_simu)) {
    stop("Your plot vector haven't the same length as your number of row in the matrix")
  }
  if (length(subplot) != nrow(AGB_simu)) {
    stop("Your subplot vector haven't the same length as your number of row in the matrix")
  }



  # function ----------------------------------------------------------------



  Plot <- data.table(plot = plot, subplot = subplot)
  indice_tree <- Plot[is.na(subplot), .I, by = plot]

  # take the first tree in the database by subplot
  indice_first <- Plot[!is.na(subplot), .(indice_line = first(.I), plot = unique(plot)), by = subplot]



  # if there is trees without label -----------------------------------------


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



  # function summary --------------------------------------------------------

  mySummary <- function(x, matrix) {
    resAGB <- colSums(matrix[x, ], na.rm = T)

    # if there is trees without label
    if (nrow(indice_tree) != 0) {
      subsample <- samples[x[1] == indice_line, ]

      # if the tree belong among the subplot
      if (nrow(subsample) != 0) {
        # sum for the trees I whose aren't in the subplot, by column
        sums <- subsample[, sum(matrix[I, unique(indice_col)], na.rm = T), by = indice_col]
        sums[is.na(V1), V1 := 0] # if there is any NA update the table
        resAGB[sums$indice_col] <- resAGB[sums$indice_col] + sums$V1 # sum the result of the table
      }
    }

    return(list(
      AGB = mean(resAGB),
      Cred_2.5 = quantile(resAGB, probs = 0.025),
      Cred_97.5 = quantile(resAGB, probs = 0.975)
    ))
  }

  AGB <- Plot[!is.na(subplot), mySummary(.I, AGB_simu), by = subplot]

  if (all(plot == subplot)) {
    setnames(AGB, "subplot", "plot")
  }

  setDF(AGB)

  return(AGB)
}
