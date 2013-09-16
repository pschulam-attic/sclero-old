library(plyr)

data <- arrange(read.csv("../../data/train.csv", stringsAsFactors = FALSE), PtID, year)
data$PtID <- as.factor(data$PtID)

patient.ids <- unique(data$PtID)

set.seed(1)
some.patients <- sample(patient.ids, 20)

library(ggplot2)

ggplot(subset(data, PtID %in% some.patients), aes(x = year, y = fvc)) +
    geom_line() +
    stat_smooth(method = lm, alpha = 0.5) +
    facet_wrap(~ PtID)
