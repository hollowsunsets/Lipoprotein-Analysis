# --------------------- Dependencies ---------------------------
library(dplyr)

# ----------------- Global Variables ---------------------------
# Tracks the scans that have been dropped from the dataset
scansDropped <- c(0)


# -------------------- Functions -------------------------------

# Drops the scan that corresponds with the given scan number from the 
# given dataset. Produces error message if the given scan number does not
# correspond with a column in the dataset.
dropScan <- function(graph.data, scan.number) {
  if (!(scan.number %in% colnames(graph.data))) {
    stop("Dataset does not contain the given scan. Please enter another scan number.")
  }
  # Selects all columns that are not the given scan number 
  graph.data <- select(graph.data, which(colnames(graph.data) != scan.number)) 
  # Tracks the number of the scan that was dropped
  scansDropped[length(scansDropped) + 1] <- scan.number
  return(graph.data)
}

# Adds the given scan number from the original data to the graphed dataset.
# Error message is produced if the scan has not been dropped previously
# from the dataset or if there is no corresponding column
# in the original, unmodified dataset.
addBackScan <- function(graph.data, scan.number, original.data) {
  if ((!(scan.number %in% scansDropped)) || (!(scan.number %in% colnames(original.data)))) {
    stop("Scan has not been dropped previously. Please enter another scan number.")
  }
  # Adds the column that corresponds with the given scan number back to the graphed dataset
  cbind(graph.data, original.data$scan.number)
  # Removes the number tracking the scan that was previously dropped 
  # i.e, scansDropped = 1 3 4, scan.number = 1
  # post: scansDropped = 3 4
  scansDropped <- scansDropped[-which(scansDropped %in% scan.number)]
  return(graph.data)
}

changeGraphScale <- function(graph.data, parameter) {
  # do whatever JC did with the parameter with ggplot2
  # plot.ly also scales so you could easily use that instead
}

