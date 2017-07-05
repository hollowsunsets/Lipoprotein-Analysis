library(dplyr)
library(xlsx)
library(plotly)
library(ggplot2)

# read.xlsx is super slow. Perhaps only read in a subset?
test2 <- ampGraphData(read.xlsx("data\\170522_new_data_format_for_JC_amplog.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
test2 <- scanGraphData(read.csv("data\\170522_new_data_format_for_JC_DMA.csv", stringsAsFactors = FALSE))
test3 <- scanGraphData(read.csv("data\\AIMDataset2.csv", stringsAsFactors=FALSE))


scan.data.test <- test2[,1:6]
scan.data.test2 <- test3[,1:6]

test2$`sample 1`

test4 <- test2$`sample 1`
# TO-DO:
# Test multi-line plots on plotly
# Create average data function

amp.plot <- ggplot(data=test2, aes(x = X1, y = X3, group=1)) + geom_line()

amp.plot


                                               