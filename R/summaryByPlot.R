#' Summarise by plot the posterior distribution of AGB values
#'
#' @description
#' This function summarizes the matrix `AGB_val` given by the function [AGBmonteCarlo()] by plot.
#'
#' @details
#' If some trees belong to an unknown plot (i.e. NA value in the plot arguments), their AGB values are randomly assigned
#' to a plot at each iteration of the AGB monte Carlo approach.
#'
#' @param AGB_val Matrix resulting from the [AGBmonteCarlo()] function (AGB_val element of the list), or just the output of the [AGBmonteCarlo()] function.
#' @param plot Vector corresponding to the plots code (plots ID)
#' @param drawPlot A logic indicating whether the graphic should be displayed or not
#'
#' @return a data frame where:
#'   - `plot`: the code of the plot
#'   - `AGB`: AGB value at the plot level
#'   - `Cred_2.5`: the 2.5\% quantile for the plot
#'   - `Cred_97.5`: the 97.5\% quantile for the plot
#'
#' @export
#'
#' @importFrom data.table data.table := first setDT
#' @importFrom grDevices terrain.colors
#' @importFrom graphics segments
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar labs scale_x_discrete theme_minimal
#' @examples
#'
#' # Load a database
#' data(NouraguesHD)
#' data(NouraguesTrees)
#'
#' # Modelling height-diameter relationship
#' HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, method = "log2")
#'
#' # Retrieving wood density values
#' \donttest{
#'   NouraguesWD <- getWoodDensity(NouraguesTrees$Genus, NouraguesTrees$Species,
#'                                 stand = NouraguesTrees$plotId)
#' }
#'
#' # Propagating errors
#' \donttest{
#'   resultMC <- AGBmonteCarlo(
#'     D = NouraguesTrees$D, WD = NouraguesWD$meanWD,
#'     errWD = NouraguesWD$sdWD, HDmodel = HDmodel )
#'   
#'   # The summary by plot
#'   summaryByPlot(AGB_val = resultMC$AGB_simu, plot = NouraguesTrees$Plot)
#' }
#'
summaryByPlot <- function(AGB_val, plot, drawPlot = FALSE) {

  ##### Checking arguments -----------------------------------------------------
  if (is.list(AGB_val)) {
    if(!is.null(AGB_val$AGB_simu)) { # if AGB_val is the output of AGBmonterCarlo
      AGB_val <- AGB_val$AGB_simu
    } else if (!is.null(AGB_val$AGB_pred)) { # in BiomassApp: AGB_val can be a list of 1 element named 'AGB_pred' and containing the output of compute_AGB() in a matrix form
      AGB_val <- AGB_val$AGB_pred
    }
  }
  if (!is.matrix(AGB_val) && !is.vector(AGB_val)) {
    stop(
      "AGB_val must be either the output of AGBmonteCarlo() or a matrix containing individual AGB (one row per tree)."
    )
  }
  if (length(plot) != ifelse(is.matrix(AGB_val), nrow(AGB_val), length(AGB_val))) {
    stop("Your 'plot' vector have not the same length as your number of row in the matrix")
  }

  ##### Data processing --------------------------------------------------------
  Plot <- data.table(plot = plot)
  indice_tree <- Plot[is.na(plot), .I, by = plot]

  # filter if there is there is NA in the AGB_val
  filter <- rowSums(is.na(AGB_val)) > 0

  # take the first tree in the database by plot
  indice_first <- Plot[!is.na(plot) & !filter, .(indice_line = first(.I), plot = unique(plot)), by = plot]

  # if there is trees without label
  if (nrow(indice_tree) != 0) {
    # Create a table with I : indice of the tree to distribute
    # indice_line : a random sample of matrix line inside the plot without the trees to distribute
    # indice_col : the column index of the matrix by I
    mySample <- function(plot1, n) {
      return(samples = indice_first[, sample(indice_line, n, replace = TRUE)])
    }
    n <- ncol(AGB_val)
    samples <- indice_tree[, .(indice_line = mySample(plot, n), indice_col = 1:n), by = I]

    # remove the index for the tree to distribute when it is NA
    samples <- samples[ !(I %in% filter) ]
  }


  ### function summary --------------------------------------------------------

  mySummary <- function(x, matrix) { # x = row numbers of the trees for the current plot ; matrix = AGB_val
    
    current_plot_matrix <- as.matrix(matrix[x,]) # when summaryByPlot is called with AGB_val = computeAGB(), matrix is not a matrix anymore but a vector and we want a one-column matrix (instead of 1000 columns when AGB_val = AGBmonteCarlo()$AGB_simu)
    
    resAGB <- colSums(current_plot_matrix, na.rm = TRUE)
    
    
    # if there is trees without label
    if (nrow(indice_tree) != 0) {
      subsample <- samples[x[1] == indice_line, ]

      # if the tree belong among the subplot
      if (nrow(subsample) != 0) {
        # sum for the trees I whose are not in the subplot, by column
        sums <- subsample[, sum(matrix[I, unique(indice_col)], na.rm = TRUE), by = indice_col]
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

  AGB <- Plot[!is.na(plot), mySummary(.I, AGB_val), by = plot]

  if (drawPlot) {
    AGB_plot <- copy(AGB)
    AGB_plot <- AGB_plot[order(AGB) , i:=.I ]
    print(ggplot(data = AGB_plot, aes(x = as.factor(i), y = AGB)) + 
      geom_point(size=2) + 
      geom_errorbar(aes(ymin=Cred_2.5, ymax=Cred_97.5), width=.2) +
      labs(x = "", title = "AGB by plot") +
      scale_x_discrete(breaks = AGB_plot$i, labels = AGB_plot$plot) +
      theme_minimal())
    AGB_plot$i <- NULL
    
  }

  return(as.data.frame(AGB))
}
