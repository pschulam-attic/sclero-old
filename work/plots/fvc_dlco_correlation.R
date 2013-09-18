source("include/read_severity_data.R")

library(ggplot2)
library(GGally)

df <- severity.data[, c("perc.FVC.of.predicted", "perc.DLCO.of.predicted")]

p <- ggpairs(na.omit(df), axisLabels = "show", title = "Lab Variable Interactions",
        diag = list(continuous = "bar"),
        upper = list(continuous = "points"),
        lower = list(continuous = "density"))

p.1.2 <- ggplot(na.omit(df), aes(x = perc.DLCO.of.predicted, y = perc.FVC.of.predicted)) +
    geom_point(alpha = 0.2)

putPlot(p, p.1.2, 1, 2)
