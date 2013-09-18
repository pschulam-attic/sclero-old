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

mem_predict <- function(df, lme.fit) {
  train <- subset(df, since.diagnosis < 2)
  test <- subset(df, since.diagnosis >= 3)

  test[["fvc.hat"]] <- predict(lme.fit, test)

  mse <- with(test, mean((fvc.hat - FVC.Pre) ^ 2))

  data.frame(fvc.mse = mse)
}

fvc.data <- prep_data(read_data("../../data/dev.RData"), 2)

library(nlme)

fvc.train <- subset(fvc.data, since.diagnosis < 2)
lme.fit <- lme(FVC.Pre ~ 1 + since.diagnosis, random = ~ 1 + since.diagnosis | PtID, data = fvc.train)

library(plyr)

mem.results <- ddply(fvc.data, ~ PtID, mem_predict, lme.fit)
