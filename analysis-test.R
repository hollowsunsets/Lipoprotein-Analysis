library(plotly)

source("other\\file-setup.R")
source("other\\graph-alteration.R")
source("other\\graph-analysis.R")

test <- nm.graph.data

# TO-DO:
# Test multi-line plots on plotly
# Create average data function

p <- plot_ly(nm.graph.data, x = ~X1, y = ~X3, name = "graph", type = "scatter", mode = "lines")
p

