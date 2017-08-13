library(dplyr)
library(readxl)
library(plotly)
library(ggplot2)
library(ggvis)
library(pracma)
library(zoo)

noise <- function(x, y, z) {
  mapply(mean, sum(x, y, z))
}

mapply(findSimilarity, scan.maxima[,1], scan.maxima)

graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA))) 
loess.graph.data <- applyLoessSmooth(graph.data[[17]], 0.05)
sample.flags <- integer(length(graph.data))

names(sample.flags) <- names(graph.data)

scan.plot.data <- melt(loess.graph.data, id.vars = "sample.diameters", variable.name = 'scans')
scan.plot.data <- scan.plot.data %>% mutate("dissimilar" = scan.plot.data$scans %in% dissimilar.scans)

cars <- mtcars



scan.maxima[] <- lapply(current.scan.names, 
                                              function (x) { 
                                                findLocalMaxima(
                                                  current.graph.data$sample.diameters, 
                                                  current.graph.data[[x]],
                                                  700)[1:5]}) # 400 and 1:5 are arbitarily defined

lapply(current.graph.data)


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

