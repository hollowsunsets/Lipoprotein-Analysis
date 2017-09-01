library(dplyr)
library(readxl)
library(plotly)
library(ggplot2)
library(ggvis)
library(pracma)
library(zoo)

graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA))) 
loess.graph.data <- applyLoessSmooth(graph.data[[17]], 0.05)

dissimilar.scans <- vector(mode = "list", length = length(graph.data))
dissimilar.scans[[1]] <- c("scan2", "scan1")
names(dissimilar.scans) <- names(graph.data)
subset.test <- c("scan1", "scan2")
graph.data.subset <- loess.graph.data %>% select(dissimilar.scans[["sample 1"]])

sample.test <- rep("normal", 10)

dissimilar.counts <- sapply(seq(length(dissimilar.scans)), function(x) { length(dissimilar.scans[[x]]) })

test <- dissimilar.scans

test <- rep(mode = "list", 10)

dissimilar.scans[["sample 1"]] <- c("scan1", "scan2")

## CURRENT PROGRESS: TRYING TO TEST IF yOU CAN SELECT ALL COiLUMN NAMES NOT IN LIST
## SEE AVG SCANS FUNCTION PLS DON'T FORGET
current.sample <- graph.data[[1]] %>% select(names(graph.data[[1]][!names(graph.data[[1]]) %in% dissimilar.scans[[1]]]))
current.sample <- rep(0, length(current.sample[[1]]))
names(graph.data[[1]]) %in% dissimilar.scans[[1]]
dissimilar.scans[[1]]
names(graph.data[[1]])

scan.plot <- ggplot(NULL, aes(sample.diameters, value)) +
  geom_line(data = scan.plot.data, aes(colour = scans, linetype = dissimilar)) +
  xlab("Diameters (nm)") +
  ylab("Concentration (dN#/cm^2)") +
  theme(plot.title = element_text(hjust = 0.5)) # Centers graph title

scan.plot

# idea - store two sets of data that will be mapped based on whether or not the user wants to see the visual distinguisher
    ## also include ability to remove or add back scans
    ## include ability to do this only for 

selected.amp.data

amp.plot <- ggplot(data = selected.amp.data) +
                geom_line(aes(x = X1, y= X3, group = 1)) + 
                xlab("Time (PST, Standard Time)") +
                ylab("Amperage (amp)") +
                ggtitle(paste0("Amperage Data")) + 
                theme(plot.title = element_text(hjust = 0.5))

amp.plot

