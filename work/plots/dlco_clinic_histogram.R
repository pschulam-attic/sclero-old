source("include/read_severity_data.R")

library(ggplot2)
library(plyr)
library(reshape2)

severity.data.m <- melt(severity.data, id.vars = c("perc.DLCO.of.predicted"), measure.vars = clinic.vars[2:4])
median.data <- ddply(severity.data.m, ~ variable + value, summarize, dlco.median = median(perc.DLCO.of.predicted, na.rm = TRUE))

p <- ggplot(severity.data.m, aes(x = perc.DLCO.of.predicted)) +
    geom_vline(aes(xintercept = dlco.median), data = median.data, color = "red", linetype = "dashed") +
    facet_grid(value ~ variable) +
    labs(title = "DLCO Against Clinical Measurements (Red = Median)")

p + geom_histogram(binwidth = 2, alpha = 0.5) + xlim(50, 100)
