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

foo <- input_slider(6, 24, c(1,6))

scan.plot <- ggplot(data=curr.scan.state, aes(sample.diameters)) + 
  geom_line(aes(color="scan 1",y=curr.scan.state[[1]])) + 
  geom_line(aes(color="scan 2",y=curr.scan.state[[2]])) + 
  geom_line(aes(color="scan 3",y=curr.scan.state[[3]])) +
  geom_line(aes(color="scan 4",y=curr.scan.state[[4]])) + 
  xlab("Diameters (nm)") +
  ylab("Concentration (dN#/cm^2)") +
  ggtitle(paste0(input$sampleSelect)) + 
  theme(plot.title = element_text(hjust = 0.5))

test2 <- test2 %>% filter()

na.omit(test2) %>%
  ggvis() %>%
    layer_paths(~X1, ~X3)
selected.amp.data

amp.plot <- ggplot(data = selected.amp.data) +
                geom_line(aes(x = X1, y= X3, group = 1)) + 
                xlab("Time (PST, Standard Time)") +
                ylab("Amperage (amp)") +
                ggtitle(paste0("Amperage Data")) + 
                theme(plot.title = element_text(hjust = 0.5))

amp.plot
