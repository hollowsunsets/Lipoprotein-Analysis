dat <- read.csv("before_loess.csv", stringsAsFactors = FALSE, na.strings = c("", NA))

applyLoessSmooth <- function(raw.data, smoothing.span) {
  raw.data <- raw.data[complete.cases(raw.data),]
  
  ## response
  vars <- colnames(raw.data)
  ## covariate
  id <- 1:nrow(raw.data)
  ## define a loess filter function (fitting loess regression line)
  loess.filter <- function (x, given.data, span) loess(formula = as.formula(paste(x, "id", sep = "~")),
                                           data = given.data,
                                           degree = 1,
                                           span = span)$fitted 
  ## apply filter column-by-column
  loess.graph.data <- as.data.frame(lapply(vars, loess.filter, given.data = raw.data, span = smoothing.span),
                           col.names = colnames(raw.data))
  sample.rows <- length(loess.graph.data[1])
  loess.graph.data <- loess.graph.data %>% mutate("sample.diameters" = raw.data$sample.diameters[1:nrow(raw.data)])

}
smoothed.data <- applyLoessSmooth(dat, 0.05)
  theme(plot.title = element_text(hjust = 0.5))

scan.plot.data <- melt(smoothed.data, id.vars = "sample.diameters", variable.name = 'series')

scan.plot <- ggplot(data = scan.plot.data, aes(sample.diameters, value)) +
  geom_line(aes(colour = series)) +
  xlab("Diameters (nm)") +                                                                                                                
  ylab("Concentration (dN#/cm^2)") +

