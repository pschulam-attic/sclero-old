main <- function(data.file) {
  library(plyr)

  data <- arrange(read.csv(data.file, stringsAsFactors = FALSE), PtID, year)
  data$PtID <- as.factor(data$PtID)
  data$rp <- as.factor(data$rp)

  library(reshape2)

  data.m <- melt(data, measure.vars = c("fvc", "dlco"))

  library(ggplot2)

  p <- ggplot(data.m, aes(x = rp, y = value))
  p + geom_jitter(alpha = 0.2) + facet_wrap(~ variable, nrow = 2)
}

main("../../data/train.csv")
