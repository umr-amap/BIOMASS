if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "indice_line", "indice_col", "V1"
  ))
}


#' Summarize by plot (or subplot) the posterior distribution of AGB values
#'
#' @description
#' This function summarize the matrix `AGB_simu` given by the function [AGBmonteCarlo()] by plot.
#'
#' @details
#' If some trees belong to an unknown plot (i.e. NA value in the plot arguments), their AGB values are randomly assigned
#' to a plot at each iteration of the AGB monte Carlo approach.
#'
#' @param AGB_simu Matrix resulting from the function [AGBmonteCarlo()] (AGB_simu element of the list), 
#' or just the result of the function [AGBmonteCarlo()]
#' @param plot Vector with the code of plot
#'
#' @return a data frame where :
#'   - `plot`: the code of the plot
#'   - `AGB`: the mean of AGB for the plot
#'   - `Cred_2.5`: the quantile 2.5\% for the plot
#'   - `Cred_97.5`: the quantile 97.5\% for the plot
#'
#' @export
#'
#' @importFrom data.table data.table := first
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
#'   stand = KarnatakaForest$plotId
#' )
#' 
#' # Propagating errors
#' filt <- KarnatakaForest$plotId %in% c("BSP20", "BSP14")
#' resultMC <- AGBmonteCarlo(
#'   D = KarnatakaForest$D[filt], WD = KarnatakaWD$meanWD[filt],
#'   errWD = KarnatakaWD$sdWD[filt], HDmodel = HDmodel
#' )
#' 
#' plot <- KarnatakaForest$plotId[ filt ]
#' 
#' # The summary by plot
#' summaryByPlot(AGB_simu = resultMC$AGB_simu, plot)
summaryByPlot <- function(AGB_simu, plot) {


  # parameters verification -------------------------------------------------
  if(is.list(AGB_simu)){
    AGB_simu = AGB_simu$AGB_simu
  }
  if (!is.matrix(AGB_simu)) {
    stop("The AGB_simu must be a matrix you have for the result of the function 'AGBmonteCarlo', or just the result of the function")
  }
  if (length(plot) != nrow(AGB_simu)) {
    stop("Your plot vector have not the same length as your number of row in the matrix")
  }
  



  # function ----------------------------------------------------------------



  Plot <- data.table(plot = plot)
  indice_tree <- Plot[is.na(plot), .I, by = plot]

  # filter if there is there is NA in the AGB_simu
  filter <- rowSums(is.na(AGB_simu)) > 0

  # take the first tree in the database by plot
  indice_first <- Plot[!is.na(plot) & !filter, .(indice_line = first(.I), plot = unique(plot)), by = plot]



  # if there is trees without label -----------------------------------------


  if (nrow(indice_tree) != 0) {
    # Create a table with I : indice of the tree to distribute
    # indice_line : a random sample of matrix line inside the plot without the trees to distribute
    # indice_col : the column index of the matrix by I
    mySample <- function(plot1, n) {
      return(samples = indice_first[, sample(indice_line, n, replace = T)])
    }
    n <- ncol(AGB_simu)
    samples <- indice_tree[, .(indice_line = mySample(plot, n), indice_col = 1:n), by = I]

    # remove the index for the tree to distribute when it is NA
    samples <- samples[ !(I %in% filter) ]
  }



  # function summary --------------------------------------------------------

  mySummary <- function(x, matrix) {
    resAGB <- colSums(matrix[x, ], na.rm = T)

    # if there is trees without label
    if (nrow(indice_tree) != 0) {
      subsample <- samples[x[1] == indice_line, ]

      # if the tree belong among the subplot
      if (nrow(subsample) != 0) {
        # sum for the trees I whose are not in the subplot, by column
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

  AGB <- Plot[!is.na(plot), mySummary(.I, AGB_simu), by = plot]

  setDF(AGB)

  return(AGB)
}
