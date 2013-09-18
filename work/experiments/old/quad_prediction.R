read_data <- function(dev.rd) {
  load(dev.rd)

  diagnosed <- structure(as.Date(patient.train[["DateDiagnosis"]]),
                         names = patient.train[["PtID"]])

  library(plyr)
  
  fvc.data <- pft.train[, c("PtID", "Date", "FVC.Pre")]
  fvc.data[["Date"]] <- as.Date(fvc.data[["Date"]])
  fvc.data <- arrange(fvc.data, PtID, Date)
  fvc.data <- transform(fvc.data, since.diagnosis = Date - diagnosed[as.character(PtID)])
  fvc.data <- transform(fvc.data, since.diagnosis = as.numeric(since.diagnosis) / 365)

  return(na.omit(fvc.data))
}

num_visits <- function(fvc.data) {
  library(plyr)

  df <- ddply(fvc.data, ~ PtID, summarize, n.visits = length(FVC.Pre))
  structure(df$n.visits, names = df$PtID)
}

prep_data <- function(fvc.data, year.thresh) {
  fvc.train <- subset(fvc.data, since.diagnosis < year.thresh)
  fvc.test <- subset(fvc.data, since.diagnosis >= (year.thresh + 1))

  num.train <- num_visits(fvc.train)
  num.test <- num_visits(fvc.test)

  valid.train <- as.integer(names(num.train[num.train >= 3]))
  valid.test <- as.integer(names(num.test[num.test >= 1]))

  patient.ids <- intersect(valid.train, valid.test)

  subset(fvc.data, PtID %in% patient.ids)
}

quad_predict <- function(df) {
  train <- subset(df, since.diagnosis < 2)
  test <- subset(df, since.diagnosis >= 3)

  lm.fit <- lm(FVC.Pre ~ poly(since.diagnosis, degree = 2), data = train)
  test[["fvc.hat"]] <- predict(lm.fit, test)

  mse <- with(test, mean((fvc.hat - FVC.Pre) ^ 2))

  data.frame(fvc.mse = mse)
}

year.thresh <- 2

fvc.data <- prep_data(read_data("../../data/dev.RData"), year.thresh)

library(plyr)

quad.data <- ddply(fvc.data, ~ PtID, quad_predict)

library(ggplot2)

fvc.sd <- sd(fvc.data[["FVC.Pre"]])

with(quad.data, quantile(fvc.mse / fvc.sd))
qplot(sqrt(fvc.mse) / fvc.sd, data = linear.data)

most.accurate <- arrange(quad.data, fvc.mse)$PtID[1:16]
ggplot(subset(fvc.data, PtID %in% most.accurate),
       aes(x = since.diagnosis, y = FVC.Pre)) +
    geom_vline(xintercept = 2, color = "red", linetype = "longdash") +
    stat_smooth(data = subset(fvc.data, since.diagnosis < 2 & PtID %in% most.accurate),
                method = lm, formula = y ~ poly(x, 2)) + 
    geom_line() + facet_wrap(~ PtID, nrow = 4, scales = "free")

least.accurate <- arrange(quad.data, desc(fvc.mse))$PtID[1:16]
ggplot(subset(fvc.data, PtID %in% least.accurate),
       aes(x = since.diagnosis, y = FVC.Pre)) +
    geom_vline(xintercept = 2, color = "red", linetype = "longdash") +
    stat_smooth(data = subset(fvc.data, since.diagnosis < 2 & PtID %in% least.accurate),
                method = lm, formula = y ~ poly(x, 2)) + 
    geom_line() + facet_wrap(~ PtID, nrow = 4, scales = "free")
