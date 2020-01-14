# Currently not in use - needs to be fixed as it currently disproportionately favors scan1 as not bad, regardless of the dataset.
findDissimilarScan <- function(current.graph.data, difference.tolerance = 0.90) {
  badScans <- c() # Default value of badScans is NULL

  # Removes all NA values from the scan data frame, if any
  current.graph.data <- current.graph.data[complete.cases(current.graph.data),]
  
  # Retrieves the scan names from the given data 
  current.scan.names <- names(select(current.graph.data, starts_with("scan")))

  # Finds up to 5 local maxima for each scan in the scan dataset.
  # 700 and 1:5 are arbitarily defined. 
  # 700 is a metric that impacts the sensitivity of the algorithm
  # that finds the maximums.
  scan.maxima <- data.frame("maximums" = lapply(current.scan.names, 
                                                  function (x) { 
                                                    findLocalMaxima(
                                                      current.graph.data$sample.diameters, 
                                                      current.graph.data[[x]], 700)[1:5]
                                                  } 
                                                ), stringsAsFactors = FALSE)
  
  # Names are added to the scan maximums to distinguish the maxima sets from one another
  names(scan.maxima) <- current.scan.names
  
  # Removes NA values (these seem to show up in some cases, haven't had the time to check why)
  scan.maxima <- scan.maxima[complete.cases(scan.maxima),]
  
  # 
  scan.maxima <- t(scan.maxima)
  
  # this is actually horrible. pretty sure I won't remember how this works tomorrow
  # TO-DO: reimplement this flaming garbage heap w/ mapply when time is available
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
  
  
  maxima.similarities.reshaped <- NULL
  
  # this just reorganizes the data so all of the corresponding maxima are in one column, rather 
  # than having all of the maxima for one scan be represented in one column
  # prev format: scan1: 8.9 10.4 15.6
  # new format: maxima 1: 8.9 8.11 9.1
  # this makes the maxima easier to compare
  maxima.similarities.reshaped <- as.data.frame(sapply(c(1:length(maxima.similarities)), 
                                                       function(x) { cbind(maxima.similarities.reshaped, 
                                                                           unlist(maxima.similarities[[x]]))}))
  # Calculates the mean of each maxima for each scan
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
    if (similarity.metrics$V1[i] < mean(similarity.metrics$V1)) {
      badScans <- c(similarity.metrics$scan.names[i], badScans)
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


# Using the given x, finds all local maxima from the given y.
# w impacts the "sensitivity" of the algorithm - higher values of w find more maxima, while lower values find less
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
getAverageScans <- function(graph.data, sparklink.file = NULL, dissimilar.scans = NULL, sample.flags = NULL) {
  print(dissimilar.scans[[1]])
  if (!(missing(sample.flags))) {
    sample.flags <- as.data.frame(t(c("Sample Flags", sample.flags)))
    avg.scans <- sample.flags
  } else {
    avg.scans <- rbind(c("Sample Flags", integer(length(graph.data) - 1)))
  }

  # Add the overall sample labels to the graph, which should have been generated already for the graph data
  sample.labels <- as.data.frame(t(c("Sample Label", names(graph.data))))
  names(sample.labels) <- names(avg.scans)
  avg.scans <-  avg.scans %>% rbind(sample.labels)
  
  if (!(missing(dissimilar.scans))) {
    dissimilar.counts <- sapply(seq(length(dissimilar.scans)), function(x) { length(dissimilar.scans[[x]]) })
    names(dissimilar.counts) <- names(graph.data)
    dissimilar.counts <- as.data.frame(t(c("# of Bad Scans", dissimilar.counts)), stringsAsFactors = FALSE)
    avg.scans <- bind_rows(avg.scans, dissimilar.counts)
  } else {
    # Otherwise, the number of bad scans are automatically set to 0 for every existing sample in the dataset.
    avg.scans <- avg.scans %>% rbind(c("# of Bad Scans", integer(length(graph.data) - 1)))
  }
  
  # Remove any columns that contain NA values that may have resulted from given files having irregular sizes (i.e, 46 labels for 47 scans)
  avg.scans <- avg.scans[ , colSums(is.na(avg.scans)) == 0]

  # If a Sparklink file was provided, add sample name, diameters and the corresponding
  # inj labels.
  if (!is.null(sparklink.file)) {
    sample.names <- rbind(sparklink.file[,4])
    if (length(graph.data) > length(sample.names)) {
      unlabeled.sample.names <- c(paste("unnamed sample ", (length(sample.names) + 1): length(graph.data)))
      sample.names <- cbind(sample.names, unlabeled.sample.names)
    }
    names(sample.names) <- names(graph.data)
    sample.names <- c("Sample Names", sample.names)
    avg.scans <- avg.scans %>% rbind(sample.names)
    avg.scans <- avg.scans %>% rbind(c("Diameter", paste0("inj", 1:(ncol(avg.scans) - 1))))
  } else {
    # Otherwise, only add the diameters and inj labels.
    avg.scans <- avg.scans %>% rbind(c("Diameter", paste0("inj", 1:(length(avg.scans) - 1))))
    
  }
    print(sample.names)
  avg.scans <- as.data.frame(avg.scans, stringsAsFactors = FALSE)
    if (!is.null(graph.data)) {
      # Retrieves the diameters from the first set of sample data. The diameters should be the same for every dataset.
      avg.sample.data <- as.character(graph.data[[1]]$sample.diameters)
      graph.index <- 1
      # For loop bound is set to the size of avg.scans so unlabeled samples are not included
      for (i in 1:(ncol(avg.scans) - 1)) {
        current.sample <- graph.data[[graph.index]][,1:4]
        
        # Selects all of the columns of scan data that are not contained in the list of dissimilar scans
        # This discludes the "bad" data from being averaged 
        current.sample <- current.sample %>% select(names(current.sample)[!names(current.sample) %in% dissimilar.scans[[graph.index]]])
        # Averages all of the scan data that remains 
        current.sample.means <- rowMeans(current.sample)
        # If the sample is flagged, the averaged scan data for that sample will be set to 0s
        if (!missing(sample.flags)) {
          if (sample.flags[graph.index + 1] == "rejected") {
            current.sample.means <- rep(0, length(current.sample.means))
          }
        }
          
        if (dissimilar.counts[names(graph.data)[graph.index]] > 1) {
          print("Dissimilar counts were found to be above 1. Averages will be set to 0")
          # Assuming that "Sample Flags" are located at row 1.
          print(graph.index)
          print(avg.scans[1, graph.index + 1])
          if (avg.scans[1, graph.index + 1] == "normal") {
            avg.scans[1, graph.index + 1] <- "rejected"
          }
          current.sample.means <- rep(0, length(current.sample.means))
        }
        # Binds the finalized average data for the current sample to the average sample data table in the form of a column
        avg.sample.data <- avg.sample.data %>% cbind(as.character(current.sample.means))
        graph.index <- graph.index + 1
      }
      
      avg.sample.data <- as.data.frame(avg.sample.data, stringsAsFactors = FALSE)
      names(avg.sample.data) <- names(avg.scans)
      avg.scans <- bind_rows(avg.scans, avg.sample.data)
    }
  # 
  avg.samples <- avg.scans[,2:ncol(avg.scans)]
  avg.samples <- avg.samples[,order((colnames(avg.samples)), decreasing = TRUE)]
  avg.scans[,2:ncol(avg.scans)] <- avg.samples
  avg.scans <- cbind("V1" = avg.scans[,1], avg.samples)

  avg.scans <- avg.scans[complete.cases(avg.scans),]
  return(avg.scans)
}

