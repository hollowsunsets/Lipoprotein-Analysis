library(dplyr)
source("file-setup.R")
library(data.table)
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



sparklink.file <- read.csv("data\\170622_Study114_Runlist.csv", stringsAsFactors = FALSE, header = FALSE)

graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE), sparklink.file)



# Assumed data input format is the returned format from scanGraphData(). 
# Generates the averaged dataset which contains the averaged values from the 4 visualized scans for each sample.
# Returns in the format of a dataframe.
getAverageScans <- function(graph.data, sparklink.file = NULL) {
  # Add the overall sample labels to the graph, which should have been generated already for the graph data
  avg.scans <-  rbind(c(" ", names(graph.data)))

  # Remove any columns that contain NA values that may have resulted from given files having irregular sizes (i.e, 46 labels for 47 scans)
  avg.scans <- avg.scans[ , colSums(is.na(avg.scans)) == 0]

  # If a Sparklink file was provided, add sample names. 
  if (!is.null(sparklink.file)) {
    avg.scans <- avg.scans %>% 
      rbind(c("Sample Name", sparklink.file[ ,4]))
  }
  
  # Add a row containing the "Diameter" and matching "inj" labels for each sample
  avg.scans <- avg.scans %>%
    rbind(c("Diameter", paste0("inj", 1:(ncol(avg.scans) - 1))))
  avg.scans <- as.data.frame(avg.scans, stringsAsFactors = FALSE)
    if (!is.null(graph.data)) {
      # Retrieves the diameters from the first set of sample data. The diameters should be the same for every dataset.
      avg.sample.data <- as.character(graph.data[[1]]$sample.diameters)
      graph.index <- 1
      # For loop bound is set to the size of avg.scans so unlabeled samples are not included
      for (i in 1:(ncol(avg.scans) - 1)) {
        current.sample <- graph.data[[graph.index]][,1:4]
        current.sample.means <- rowMeans(current.sample)
        avg.sample.data <- avg.sample.data %>%
          cbind(as.character(current.sample.means))
        graph.index <- graph.index + 1
      }
      avg.sample.data <- as.data.frame(avg.sample.data, stringsAsFactors = FALSE)
      names(avg.sample.data) <- names(avg.scans)
      avg.scans <- bind_rows(avg.scans, avg.sample.data)
    }
  
  return(avg.scans)
}
test4 <- getAverageScans(graph.data, sparklink.file)

