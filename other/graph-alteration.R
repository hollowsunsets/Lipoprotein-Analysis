library(dplyr)

# global var that tracks the scans that were dropped 
scansDropped <- c(0)


dropScan <- function(graph.data, scan.number) {
  if (!(scan.number %in% colnames(graph.data))) {
    stop()
  }
  # Selects all columns that are not the given scan number 
  
  graph.data <- select(graph.data, which(colnames(graph.data) != scan.number)) 
  # Tracks the number of the scan that was dropped
  scansDropped[length(scansDropped) + 1] <- scan.number
  return(graph.data)
}

addScan <- function(graph.data, scan.number, original.data) {
  if (!(scan.number %in% scansDropped)) {
    stop()
  }
  cbind(graph.data, original.data$scan.number)
  return(graph.data)
}

changeGraphScale <- function(graph.data, parameter) {
  # do whatever JC did with the parameter with ggplot2
  # plot.ly also scales so you could easily use that instead
}

