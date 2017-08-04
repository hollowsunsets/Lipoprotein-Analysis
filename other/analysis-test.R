library(dplyr)
library(readxl)
library(plotly)
library(ggplot2)
library(ggvis)
library(pracma)
library(zoo)


graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA))) 
loess.graph.data <- applyLoessSmooth(graph.data[[17]], 0.05)
sample.flags <- integer(length(graph.data))

names(sample.flags) <- names(graph.data)

scan.plot.data <- melt(loess.graph.data, id.vars = "sample.diameters", variable.name = 'scans')
scan.plot.data <- scan.plot.data %>% mutate("dissimilar" = scan.plot.data$scans %in% dissimilar.scans)
  
# Steps:
## For marked: Pull out all of the bad data
## Melt the rest of the data normally

dissimilar.scans <- vector(mode = "list", length = length(graph.data))
names(dissimilar.scans) <- names(graph.data)

dissimilar.scans[["sample 17"]] <- findDissimilarScan(loess.graph.data)

current.scan.similarities <- dissimilar.scans[["sample 17"]]

current.scan.similarities <- current.scan.similarities[-which(current.scan.similarities == "scan1")]

current.scan.similarities <- append(current.scan.similarities, "scan2")

current.scan.names <- colnames(select(loess.graph.data, starts_with("scan")))
flagged.names <- current.scan.names[(current.scan.names %in% dissimilar.scans[["sample 17"]])]
unflagged.names <- current.scan.names[!(current.scan.names %in% dissimilar.scans[["sample 17"]])]


dissimilar.scans["sample 1"] <- findDissimilarScan(applyLoessSmooth(graph.data[[1]], 0.05))

dissimilar.scans <- lapply(c(1:length(graph.data)), function(x) { findDissimilarScan(applyLoessSmooth(graph.data[[x]], 0.05)) } )

names(dissimilar.scans) <- names(graph.data)
c(1:length(graph.data))
length(graph.data)

test <- dissimilar.scans["sample 1"]

is.null(test[[1]])
test$x

test$i

print(test)
print(test2)
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

