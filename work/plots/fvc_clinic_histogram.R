source("include/read_severity_data.R")

library(ggplot2)
library(plyr)
library(reshape2)

severity.data.m <- melt(severity.data, id.vars = c("perc.FVC.of.predicted"), measure.vars = clinic.vars[2:4])
median.data <- ddply(severity.data.m, ~ variable + value, summarize, fvc.median = median(perc.FVC.of.predicted, na.rm = TRUE))

p <- ggplot(severity.data.m, aes(x = perc.FVC.of.predicted)) +
    geom_vline(aes(xintercept = fvc.median), data = median.data, color = "red", linetype = "dashed") +
    facet_grid(value ~ variable) +
    labs(title = "FVC Against Clinical Measurements (Red = Median)")

p + geom_histogram(binwidth = 2, alpha = 0.5) + xlim(50, 100)
