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

MemPredictor <- function(train.data) {
  library(lme4)

  fit <- lmer(FVC.Pre ~ poly(since.diagnosis, degree = 1) + (poly(since.diagnosis, degree = 1) | PtID),
              data = train.data)

  structure(list(lme.fit = fit, data = train.data), class = "MemPredictor")
}

resid.MemPredictor <- function(mp.fit) {
  resid(mp.fit$lme.fit)
}

predict.MemPredictor <- function(mp.fit, test.data) {
  rf <- ranef(mp.fit[["lme.fit"]])$PtID
  
  b0 <- structure(rf[, 1], names = rownames(rf))
  b1 <- structure(rf[, 2], names = rownames(rf))
  #b2 <- structure(rf[, 3], names = rownames(rf))

  test.data <- transform(test.data, PtID = as.character(PtID))

  test.data <- transform(test.data,
                         fvc.hat = b0[PtID] + b1[PtID] * since.diagnosis)

  test.data <- transform(test.data, PtID = as.integer(PtID))

  test.data
}

plot.MemPredictor <- function(mp.fit) {
  train.data <- mp.fit[["data"]]
  lme.fit <- mp.fit[["lme.fit"]]

  patient.ids <- unique(train.data[["PtID"]])

  since.range <- range(train.data[["since.diagnosis"]])
  fit.points <- seq(0, since.range[2], length.out = 100)
  
  fit.data <- data.frame(PtID = rep(patient.ids, each = 100),
                         since.diagnosis = fit.points)

  fit.data <- predict(mp.fit, fit.data)
  fit.data <- transform(fit.data, FVC.Pre = fvc.hat)
  fit.data <- transform(fit.data, curve.type = "fit")

  train.data <- transform(train.data, curve.type = "actual")

  some.patients <- sample(patient.ids, 16)

  keep.cols <- c("PtID", "since.diagnosis", "FVC.Pre", "curve.type")

  plot.data <- rbind(fit.data[, keep.cols], train.data[, keep.cols])
  plot.data <- subset(plot.data, PtID %in% some.patients)
  plot.data <- transform(plot.data, curve.type = as.factor(curve.type))

  ggplot(plot.data, aes(x = since.diagnosis, y = FVC.Pre, group = curve.type, color = curve.type)) +
      geom_line() + facet_wrap(~ PtID, nrow = 4)
}

compu_confidence <- function(df) {
  lm.fit <- lm(FVC.Pre ~ since.diagnosis, data = na.omit(df))
  predictions <- as.data.frame(predict(lm.fit, df, interval = "confidence"))
  mean.conf <- with(predictions, mean(upr - lwr))

  data.frame(mean.conf = mean.conf)
}

