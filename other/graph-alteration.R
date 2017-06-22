library(dplyr)

dropScan <- function(graph.data, scan.number) {
  # is a separate function really necessary for this? 
  # I guess it's nice to just have all the things you want to include in one place.
  graph.data %>% filter() 
  # filter works with filtering rows. How do we filter columns aside from tediously using select? 
}

changeGraphScale <- function(graph.data, parameter) {
  # do whatever JC did with the parameter with ggplot2
  # plot.ly also scales so you could easily use that instead
}