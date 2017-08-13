current.graph.data <- applyLoessSmooth(graph.data[[17]], 0.05)

findDissimilarScan <- function(current.graph.data, difference.tolerance = 0.90) {
  badScans <- c() # Default value of badScans is NULL

  # Removes all NA values from the scan data frame
  current.graph.data <- current.graph.data[complete.cases(current.graph.data),]
  current.scan.names <- names(select(current.graph.data, starts_with("scan")))

  scan.maxima <- data.frame("maximums" = lapply(current.scan.names, 
                                                  function (x) { 
                                                    findLocalMaxima(
                                                      current.graph.data$sample.diameters, 
                                                      current.graph.data[[x]], 700)[1:5]
                                                  } # 400 and 1:5 are arbitarily defined
                                                ), stringsAsFactors = FALSE)
  
  names(scan.maxima) <- current.scan.names
  
  scan.maxima <- scan.maxima[complete.cases(scan.maxima),]
  
  scan.maxima <- t(scan.maxima)
  
  # this is actually horrible. pretty sure I won't remember how this works tomorrow
  maxima.similarities <- lapply(c(1:ncol(scan.maxima)), function(z) { # for each column in scan.maximums
    lapply(seq_along(scan.maxima[,z]), function(x) { # for each element in the column
        mean( # calculate the mean
          sapply(seq_along(scan.maxima[,z][1: length(scan.maxima[,z]) - 1]), # for the number of elements in the column - 1 (because we are comparing to every element except itself)
                 function(y) {
                   findSimilarity(scan.maxima[,z][x], scan.maxima[,z][-x][[y]]) # the similarity for each element 
                                                                                    # compared to all other elements in the column except itself
                 })
            )
      })
  })
  
  # maxima.similarities2 <- mapply(function(x) {
  #   
  # }, c(1:ncol(scan.maxima)))
  #   
  
  
  
  maxima.similarities.reshaped <- NULL
  
  

  maxima.similarities.reshaped <- as.data.frame(sapply(c(1:length(maxima.similarities)), 
                                                       function(x) { cbind(maxima.similarities.reshaped, 
                                                                           unlist(maxima.similarities[[x]]))}))

  if (ncol(maxima.similarities.reshaped) > 1) {
    similarity.metrics <- as.data.frame(cbind(unlist(lapply(c(1:nrow(maxima.similarities.reshaped)), 
                                                            function(x) mean(as.numeric(maxima.similarities.reshaped[x,]), 
                                                                             na.rm = FALSE)))))
  } else {
    similarity.metrics <- maxima.similarities.reshaped
  }
  # Adds names to the similarity metrics dataframe so they can be accessed
  similarity.metrics$scan.names <- current.scan.names

  # Records the bad scans, if there are any.
  
  for (i in 1:nrow(similarity.metrics)) {
    if (similarity.metrics$V1[1] != min(similarity.metrics$V1)) {
      if (similarity.metrics$V1[i] < similarity.metrics$V1[1]) {
        badScans <- c(similarity.metrics$scan.names[i], badScans)
      }
    } else {
      if (similarity.metrics$V1[i] < mean(similarity.metrics$V1)) {
        badScans <- c(similarity.metrics$scan.names[i], badScans)
      }
    }
  }
  # If there are no bad scans, return a list containing "None" (assuming that the functions using this expect a list)
  # Otherwise, return the list of bad scans.
  if (is.null(badScans)) {
    return(c(""))
  } else {
    return(badScans)
  }
}


findLocalMaxima <- function(x, y, w = 1) {
  n <- length(y)
  y.max <- rollapply(zoo(y), 2 * w + 1, max, align = "center")
  delta <- y.max - y[-c(1:w, n + 1 - 1:w)]
  i.max <- which(delta <= 0) + w
  return(unique(y[i.max]))
}

# Computes the similarity between two numbers, and returns a number representing a metric
# that indicates the level of similarity, with 100 being a perfect match, and numbers closer
# to 0 being extremely dissimilar.
findSimilarity <- function(first.number, second.number) {
  return(min(first.number, second.number)/max(first.number, second.number))
}

# Assumed data input format is the returned format from scanGraphData(). 
# Generates the averaged dataset which contains the averaged values from the 4 visualized scans for each sample.
# Returns in the format of a dataframe.
# sample.flags <- integer(length(graph.data))

 # graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA)), sparklink.file) 
 # sparklink.file <- read.csv("data\\170622_Study114_Runlist.csv", stringsAsFactors = FALSE, header = FALSE)
 # sample.flags <- integer(length(graph.data))
 # names(sample.flags) <- names(graph.data)
getAverageScans <- function(graph.data, sparklink.file = NULL, sample.flags = NULL) {
  
  if (!is.null(sample.flags)) {
    avg.scans <- rbind(c("Scan Flag", sample.flags))
  } else {
    # Otherwise, flags are automatically set to 0 for every existing sample in the dataset.
    avg.scans <- rbind(c("Scan Flag", integer(length(graph.data) - 1)))
  }
  
  
  # Add the overall sample labels to the graph, which should have been generated already for the graph data
  avg.scans <-  avg.scans %>% rbind(c("Sample Label", names(graph.data)))

  # Remove any columns that contain NA values that may have resulted from given files having irregular sizes (i.e, 46 labels for 47 scans)
  avg.scans <- avg.scans[ , colSums(is.na(avg.scans)) == 0]

  # If a Sparklink file was provided, add sample name, diameters and the corresponding
  # inj labels.
  if (!is.null(sparklink.file)) {
    sample.names <- rbind(c("Sample Name", sparklink.file[,4]))
    sample.size.difference <- (length(graph.data) + 1) - length(sample.names)
    if (length(sample.names) < length(graph.data) + 1) {
      sample.names <- cbind(sample.names, 
                            c(paste0("unnamed sample ", 
                                     length(sample.names):
                                     length(sample.names))))
    }
    avg.scans <- avg.scans %>% rbind(sample.names)
    avg.scans <- avg.scans %>% rbind(c("Diameter", paste0("inj", 1:(ncol(avg.scans) - 1))))
  } else {
    # Otherwise, only add the diameters and inj labels.
    avg.scans <- avg.scans %>% rbind(c("Diameter", paste0("inj", 1:(length(avg.scans) - 1))))
    
  }

  avg.scans <- as.data.frame(avg.scans, stringsAsFactors = FALSE)
    if (!is.null(graph.data)) {
      # Retrieves the diameters from the first set of sample data. The diameters should be the same for every dataset.
      avg.sample.data <- as.character(graph.data[[1]]$sample.diameters)
      graph.index <- 1
      # For loop bound is set to the size of avg.scans so unlabeled samples are not included
      for (i in 1:(ncol(avg.scans) - 1)) {
        current.sample <- graph.data[[graph.index]][,1:4]
        current.sample.means <- rowMeans(current.sample)
        # If the sample is flagged, the average scan for that sample will be set to the 4th 
        # scan, which is scan #6 
        if (sample.flags[graph.index] == 1) {
          current.sample.means <- current.sample[[4]]
        }
        avg.sample.data <- avg.sample.data %>%
          cbind(as.character(current.sample.means))
        graph.index <- graph.index + 1
      }
      avg.sample.data <- as.data.frame(avg.sample.data, stringsAsFactors = FALSE)
      names(avg.sample.data) <- names(avg.scans)
      avg.scans <- bind_rows(avg.scans, avg.sample.data)
    }
  
  avg.samples <- avg.scans[,2:ncol(avg.scans)]
  avg.samples <- avg.samples[,order((colnames(avg.samples)), decreasing =TRUE)]
  avg.scans[,2:ncol(avg.scans)] <- avg.samples
  avg.scans <- cbind("V1" = avg.scans[,1], avg.samples)
  
  avg.scans <- avg.scans[complete.cases(avg.scans),]
  return(avg.scans)
}


