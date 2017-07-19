# library(dplyr)
# source("file-setup.R")

graphPairIsSimilar <- function(graph.data1.x, graph.data1.y, graph.data2.x, graph.data2.y) {
  # Calculate integral between the two graphs
  # If sum of area is > var, then return true. Else, return false.
}

graphSetIsSimilar <- function(graph.data1.x, graph.data2.y, ...) { # ... operator is only useful for other functions 
  return(graphPairIsSimilar(...))
}

# NOTE: Deleted findGraphPeak. This is easy to do with dplyr. Add annotations on with plot_ly.
# See following link for example: https://moderndata.plot.ly/interactive-r-visualizations-with-d3-ggplot2-rstudio/




#graph.data <- scanGraphData(read.csv("data\\170522_new_data_format_for_JC_DMA.csv", stringsAsFactors = FALSE))
#test.graph.data <- scanGraphData(read.csv("data\\AIMDataset2.csv", stringsAsFactors = FALSE), 
                                # read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE))
#sample.names <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE) 
#sample.names <- sample.names %>% select(Sample.Name)
#names(graph.data)



new.sparklink.file <- read.csv("data\\170622_Study114_Runlist.csv", stringsAsFactors = FALSE)

graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE), new.sparklink.file)



# Assumed data input format is the returned format from scanGraphData(). 
# Averages the data from all four scans into one dataset for each sample
# Returns in the format of a dataframe.

getAverageScans <- function(graph.data) {
  avg.scans <- data.frame(names(graph.data))
  avg.scans <- avg.scans %>% mutate(n=n())
  for (i in 1:length(graph.data)){
    avg.scans <- avg.scans %>% mutate("")
  }
  colnames(avg.scans) <- colnames(graph.data)
  
  
  return(avg.scans)
}