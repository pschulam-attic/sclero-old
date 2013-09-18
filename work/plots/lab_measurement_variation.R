load("../../data/merged_dev.RData")

get_msmt_data <- function(msmt.name) {
  df <- merged.train[, c("PtID", "Visit.Date", msmt.name)]
  df <- na.omit(df)

  require(plyr)

  df <- transform(df, Visit.Date = as.Date(Visit.Date))
  arrange(df, PtID, Visit.Date)
}

fvc.data <- get_msmt_data("perc.FVC.of.predicted")
dlco.data <- get_msmt_data("perc.DLCO.of.predicted")

msmt_diffs <- function(df, msmt.name) {
  df <- arrange(df, Visit.Date)
  date.diff <- as.integer(diff(df$Visit.Date)) / 365
  var.diff <- diff(df[[msmt.name]])

  if (length(date.diff) == 0 && length(var.diff) == 0) {
    return(data.frame())
  } else {
    data.frame(date.diff = date.diff, var.diff = var.diff, measurement = msmt.name)    
  }
}

fvc.diff.data <- ddply(fvc.data, ~ PtID, msmt_diffs, "perc.FVC.of.predicted")
dlco.diff.data <- ddply(dlco.data, ~ PtID, msmt_diffs, "perc.DLCO.of.predicted")

diff.data <- rbind(fvc.diff.data, dlco.diff.data)

library(ggplot2)

p <- ggplot(subset(diff.data, date.diff < 0.25), aes(x = var.diff))
p <- p + geom_histogram(binwidth = 2) + facet_wrap(~ measurement, ncol = 1) + xlim(-75, 75)
p + labs(title = "Consecutive Residuals within 3 Months")

p <- ggplot(subset(diff.data, date.diff < 0.5), aes(x = var.diff))
p <- p + geom_histogram(binwidth = 2) + facet_wrap(~ measurement, ncol = 1) + xlim(-75, 75)
p + labs(title = "Consecutive Residuals within 6 Months")
