# --------------------- Dependencies ---------------------------
# library(dplyr)

# ----------- Global Variables (and Test Variables) ------------
# Tracks the scans that have been dropped from the dataset
scansDropped <- c("None")
# new.scan.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE), new.sparklink.file)
# new.sparklink.file <- read.csv("data\\170622_Study114_Runlist.csv", stringsAsFactors = FALSE)
# old.sparklink.file <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE)
# raw.data <- scanGraphData(read.csv("data\\AIMDataset2.csv", stringsAsFactors=FALSE))$`sample 1`
# smoothing.span <- 0.05
# new.amp.data <- ampGraphData(read.xlsx("data\\170522_new_data_format_for_JC_amplog.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
# old.amp.data <- ampGraphData(read.xlsx("data\\ampdDataset2.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))

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
  scansDropped[length(scansDropped) + 1] <<- scan.number
  
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
  
  # Removes the number tracking the scan that was previously dropped 
  # i.e, scansDropped = 1 3 4, scan.number = 1
  # post: scansDropped = 3 4
  scansDropped <<- scansDropped[-which(scansDropped %in% scan.number)]
  graph.data <- select(original.data, (which(!(colnames(original.data) %in% scansDropped))))
  
  # Sets old graph data to new dataset which contains all data except the datasets that have 
  # not been added back

  return(graph.data)
}

# Calculates SSE for the given dataset, correlated for the chosen referential y-axis. 
# Was planning on using this to recommend an ideal smoothing span but never got around to it
calcSSE <- function(data.set, y.axis){
  data.set.state <- data.set
  data.set.state$index <- 1:nrow(data.set)
  loessMod <- try(loess(y.axis ~ index, data=data.set.state, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}

# Smoothes every column in the given dataset, based on the given smoothing span
applyLoessSmooth <- function(raw.data, smoothing.span) {
  raw.data <- raw.data[complete.cases(raw.data),]    
  vars <- colnames(raw.data)
  vars <- vars[vars != "sample.diameters"] 
  loess.filter <- function (x, given.data, span) loess(
    formula = as.formula(paste(x, "sample.diameters", sep = "~")),
    data = given.data,
    degree = 1,
    span = span)$fitted 
  loess.graph.data <- as.data.frame(lapply(vars, loess.filter, given.data = raw.data, 
                                           span = smoothing.span),
                                    col.names = vars) 
  loess.graph.data$sample.diameters <- raw.data$sample.diameters 
  return(loess.graph.data)      
}
