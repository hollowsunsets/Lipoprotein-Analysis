library(plotly)
library(ggplot2)

# read.xlsx is super slow. Perhaps only read in a subset?
test <- ampGraphData(read.xlsx("data\\170522_new_data_format_for_JC_amplog.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
test2 <- scanGraphData(read.csv("data\\170522_new_data_format_for_JC_DMA.csv", stringsAsFactors = FALSE))
test3 <- scanGraphData(read.csv("data\\AIMDataset2.csv", stringsAsFactors=FALSE))

scan.data.test <- test2[,1:6]
scan.data.test2 <- test3[,1:6]

# TO-DO:
# Test multi-line plots on plotly
# Create average data function

p <- plot_ly(nm.graph.data, x = ~X1, y = ~X3, name = "graph", type = "scatter", mode = "lines")
p2 <- plot_ly(scan.data.test, y = ~`Counts..3`, x = ~`Diameter..1`, name = 'scan1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~`Counts..4`, name = 'scan2') %>%
  add_trace(y = ~`Counts..5`, name = 'scan3') %>%
  add_trace(y = ~`Counts..6`, name= 'scan4')

p3 <- plot_ly(scan.data.test2, y = ~`Counts..3`, x = ~`Diameter..1`, name = 'scan1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~`Counts..4`, name = 'scan2') %>%
  add_trace(y = ~`Counts..5`, name = 'scan3') %>%
  add_trace(y = ~`Counts..6`, name= 'scan4')



p2g <- ggplot(data=scan.data.test,aes(`Diameter..1`)) + geom_line(aes(y=Counts..3))

p2g
p3g <- ggplot(data=scan.data.test2,aes(`Diameter..1`)) + geom_line(aes(y=Counts..3))
p3g
p3
                                               