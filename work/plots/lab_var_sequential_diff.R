#lab.vars <- c("FEV1.Pre", "FVC.Pre", "TLC_HE", "VC_HE", "FRC_HE", "RV_HE", "DLCO", "VI", "ALV")
lab.vars <- c("FVC.Pre", "perc.FVC.of.predicted", "DLCO", "perc.DLCO.of.predicted")

na_outliers <- function(x) {
  q1.q3 <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q1.q3)

  lb <- q1.q3[1] - 1.5 * iqr
  ub <- q1.q3[2] + 1.5 * iqr

  x[x < lb | ub < x] <- NA

  return(x)
}

read_data <- function(pft.filename) {
  pft.data <- read.csv(pft.filename, stringsAsFactors = FALSE)

  pft.data <- pft.data[, c("PtID", "Date", lab.vars)]
  pft.data[["Date"]] <- as.Date(pft.data[["Date"]])

  library(plyr)
  
  return(arrange(pft.data, PtID, Date))
}

consecutive_diff <- function(df, var.name) {
  df <- arrange(df, Date)
  df <- df[, c("Date", var.name)]

  date.diff <- diff(df[["Date"]])
  var.diff <- diff(df[[var.name]])

  diff.data <- data.frame(date.diff = date.diff, var.diff = var.diff)

  return(diff.data)
}

all_diff <- function(df, var.name) {
  df <- arrange(df, Date)
  df <- df[, c("Date", var.name)]

  date.diff <- outer(df[["Date"]], df[["Date"]], "-")
  var.diff <- outer(df[[var.name]], df[[var.name]], "-")

  dim(date.diff) <- NULL
  dim(var.diff) <- NULL

  diff.data <- data.frame(date.diff = date.diff, var.diff = var.diff)
}

main <- function(pft.filename, var.name) {
  pft.data <- read_data(pft.filename)

  diff.data <- ddply(pft.data, ~ PtID, consecutive_diff, var.name)

  thresholds <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)

  compute_thresh_data <- function(thresh) {
    thresh.data <- subset(diff.data, 0 < date.diff & date.diff < thresh * 365)
    #thresh.data[["var.diff"]] <- na_outliers(thresh.data[["var.diff"]])
    thresh.data[["threshold"]] <- thresh
    thresh.data
  }

  all.data <- do.call(rbind, lapply(thresholds, compute_thresh_data))

  library(ggplot2)

  p <- ggplot(all.data, aes(x = var.diff)) + labs(title = var.name)
  p + geom_histogram() + facet_wrap(~ threshold, scales = "free_x")
}
