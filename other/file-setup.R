library(dplyr)
library(xlsx)

# test data
sparklink.file <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE)
scans.file <- read.csv("data\\AIMDataset2.csv", stringsAsFactors = FALSE)
amplog.file <- read.xlsx("data\\ampdDataset2.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors=FALSE)


scanGraphData <- function(sparklink.file, scans.file) {
  scan.graph.data <- sparklink.file %>% select(Sample.Name, Sample.Vial)
  scan.graph.data %>% mutate()
}

nmGraphData <- function(amplog.file) {
  nm.graph.data <- amplog.file %>% select(X1, X3) 
}
