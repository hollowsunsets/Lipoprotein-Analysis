library(dplyr)
source("file-setup.R")

graphPairIsSimilar <- function(graph.data1.x, graph.data1.y, graph.data2.x, graph.data2.y) {
  # Calculate integral between the two graphs
  # If sum of area is > var, then return true. Else, return false.
}

graphSetIsSimilar <- function(graph.data1.x, graph.data2.y, ...) { # ... operator is only useful for other functions 
  return(graphPairIsSimilar(...))
}

# NOTE: Deleted findGraphPeak. This is easy to do with dplyr. Add annotations on with plot_ly.
# See following link for example: https://moderndata.plot.ly/interactive-r-visualizations-with-d3-ggplot2-rstudio/

graph.data <- scanGraphData(read.csv("data\\170522_new_data_format_for_JC_DMA.csv", stringsAsFactors = FALSE))
names(graph.data)
# Assumed data input format is the returned format from scanGraphData(). 
getAverageScans <- function(graph.data, graph.labels = NULL) {
  
  for (i in 1:length(graph.data)){
    
  }
  
  return(avg.scans)
}