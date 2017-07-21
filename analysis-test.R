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

avg <- mean(val1, val2, val3, val4, na.rm = FALSE)

selected.scan.data <- graph.data[[47]]
selected.scan.data <- selected.scan.data[complete.cases(selected.scan.data),]

selected.scan.data <- applyLoessSmooth(selected.scan.data, 0.05)

selected.scan.data <- test.loess

scan.plot.data <- melt(selected.scan.data, id.vars = "sample.diameters", variable.name = 'series')
scan.plot.data <- scan.plot.data %>% 
  mutate("marked" = (scan.plot.data$series == "scan2"))


# Steps:
## For marked: Pull out all of the bad data
## Melt the rest of the data normally
## Plot the lines separately 
scan.plot <- ggplot(data = scan.plot.data, aes(sample.diameters, value)) +
  geom_line(aes(colour = series)) +
  scale_size_manual(values = c(0.1, 1.5)) +
  xlab("Diameters (nm)") +
  ylab("Concentration (dN#/cm^2)")


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

