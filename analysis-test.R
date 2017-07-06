library(dplyr)
library(xlsx)
library(plotly)
library(ggplot2)
library(ggvis)

# read.xlsx is super slow. Perhaps only read in a subset?
test2 <- ampGraphData(read.xlsx("data\\170522_new_data_format_for_JC_amplog.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
test2 <- scanGraphData(read.csv("data\\170522_new_data_format_for_JC_DMA.csv", stringsAsFactors = FALSE))
test3 <- scanGraphData(read.csv("data\\AIMDataset2.csv", stringsAsFactors=FALSE))


scan.data.test <- test2[,1:6]
scan.data.test2 <- test3[,1:6]

test3$`sample 1`

test4 <- test3$`sample 1`
# TO-DO:
# Test multi-line plots on plotly
# Create average data function

na.omit(test4) %>%
    ggvis() %>%
      layer_paths(~sample.diameters, ~scan1, stroke = "red") %>%
      layer_paths(~sample.diameters, ~scan2, stroke = "blue") %>%
      layer_paths(~sample.diameters, ~scan3, stroke = "green") %>%
      layer_paths(~sample.diameters, ~scan4, stroke = "pink")


scan.plot <- ggplot(data=curr.scan.state, aes(sample.diameters)) + 
  geom_line(aes(color="scan 1",y=curr.scan.state[[1]])) + 
  geom_line(aes(color="scan 2",y=curr.scan.state[[2]])) + 
  geom_line(aes(color="scan 3",y=curr.scan.state[[3]])) +
  geom_line(aes(color="scan 4",y=curr.scan.state[[4]])) + 
  xlab("Diameters (nm)") +
  ylab("Concentration (dN#/cm^2)") +
  ggtitle(paste0(input$sampleSelect)) + 
  theme(plot.title = element_text(hjust = 0.5))