library(dplyr)
library(readxl)
library(plotly)
library(ggplot2)
library(ggvis)
library(pracma)

test2 <- ampGraphData(read_excel("data\\170522_new_data_format_for_JC_amplog.xlsx", col_names = FALSE))
test2 <- scanGraphData(read.csv("data\\170522_new_data_format_for_JC_DMA.csv", stringsAsFactors = FALSE))
test3 <- scanGraphData(read.csv("data\\AIMDataset2.csv", stringsAsFactors=FALSE))

graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA)))


val1 <- trapz(scan.test$sample.diameters, scan.test$scan1)
val2 <- trapz(scan.test$sample.diameters, scan.test$scan2)
val3 <- trapz(scan.test$sample.diameters, scan.test$scan3)
val4 <- trapz(scan.test$sample.diameters, scan.test$scan4)


testIf <- function(n) {
  if (n == 0) {
    print('1')
  }
  if (n != 2) {
    print('2')
  }
  if (n < 1) {
    print('3')
  }
}

testIf(0)


avg <- mean(val1, val2, val3, val4, na.rm = FALSE)


dissimilar.scans <- findDissimilarScan(loess.graph.data)

# similar.plot.data <- selected.scan.data[, !names(selected.scan.data) %in% dissimilar.scans]

# dissimilar.plot.data <- selected.scan.data[dissimilar.scans]

# dissimilar.plot.data <- cbind(dissimilar.plot.data, "sample.diameters" = selected.scan.data$sample.diameters)

scan.plot.data <- melt(loess.graph.data, id.vars = "sample.diameters", variable.name = 'scans')
scan.plot.data <- scan.plot.data %>% mutate("dissimilar" = scan.plot.data$scans %in% dissimilar.scans)
  
# Steps:
## For marked: Pull out all of the bad data
## Melt the rest of the data normally
## Plot the lines separately 

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

