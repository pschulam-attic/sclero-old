read_data <- function(dev.rd) {
  load(dev.rd)

  diagnosed <- structure(as.Date(patient.train[["DateDiagnosis"]]),
                         names = patient.train[["PtID"]])

  library(plyr)
  
  fvc.data <- pft.train[, c("PtID", "Date", "perc.FVC.of.predicted")]
  fvc.data[["Date"]] <- as.Date(fvc.data[["Date"]])
  fvc.data <- arrange(fvc.data, PtID, Date)
  fvc.data <- transform(fvc.data, since.diagnosis = Date - diagnosed[as.character(PtID)])
  fvc.data <- transform(fvc.data, since.diagnosis = as.numeric(since.diagnosis) / 365)

  return(na.omit(fvc.data))
}

num_visits <- function(fvc.data) {
  library(plyr)

  df <- ddply(fvc.data, ~ PtID, summarize, n.visits = length(perc.FVC.of.predicted))
  structure(df$n.visits, names = df$PtID)
}

prep_data <- function(fvc.data, year.thresh) {
  fvc.train <- subset(fvc.data, since.diagnosis < year.thresh)
  fvc.test <- subset(fvc.data, since.diagnosis >= (year.thresh + 1) & since.diagnosis < (year.thresh + 5))

  num.train <- num_visits(fvc.train)
  num.test <- num_visits(fvc.test)

  valid.train <- as.integer(names(num.train[num.train >= 3]))
  valid.test <- as.integer(names(num.test[num.test >= 1]))

  patient.ids <- intersect(valid.train, valid.test)

  subset(fvc.data, PtID %in% patient.ids)
}

MemPredictor <- function(train.data) {
  library(nlme)

  fit <- lme(perc.FVC.of.predicted ~ 1 + since.diagnosis,
             random = ~ 1 + since.diagnosis | PtID,
             data = train.data)

  structure(list(lme.fit = fit, data = train.data), class = "MemPredictor")
}

resid.MemPredictor <- function(mp.fit) {
  resid(mp.fit$lme.fit)
}

predict.MemPredictor <- function(mp.fit, test.data) {
  lme.fit <- mp.fit[["lme.fit"]]

  test.data[["y.hat"]] <- predict(lme.fit, test.data)

  test.data
}

coef.MemPredictor <- function(mp.fit) {
  fe <- fixef(mp.fit[["lme.fit"]])
  re <- ranef(mp.fit[["lme.fit"]])
  re[, 1] <- re[, 1] + fe[1]
  re[, 2] <- re[, 2] + fe[2]

  re
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
  fit.data <- transform(fit.data, perc.FVC.of.predicted = fvc.hat)
  fit.data <- transform(fit.data, curve.type = "fit")

  train.data <- transform(train.data, curve.type = "actual")

  some.patients <- sample(patient.ids, 16)

  keep.cols <- c("PtID", "since.diagnosis", "perc.FVC.of.predicted", "curve.type")

  plot.data <- rbind(fit.data[, keep.cols], train.data[, keep.cols])
  plot.data <- subset(plot.data, PtID %in% some.patients)
  plot.data <- transform(plot.data, curve.type = as.factor(curve.type))

  ggplot(plot.data, aes(x = since.diagnosis, y = perc.FVC.of.predicted, group = curve.type, color = curve.type)) +
      geom_line() + facet_wrap(~ PtID, nrow = 4)
}

compu_confidence <- function(df) {
  lm.fit <- lm(perc.FVC.of.predicted ~ since.diagnosis, data = na.omit(df))
  predictions <- as.data.frame(predict(lm.fit, df, interval = "confidence"))
  mean.conf <- with(predictions, mean(upr - lwr))

  data.frame(mean.conf = mean.conf)
}

plot_patients <- function(data, patient.ids, patient.coef = NULL) {
  require(ggplot2)
  patient.ids <- unique(patient.ids)
  data <- subset(data, PtID %in% patient.ids)

  n <- ceiling(sqrt(length(patient.ids)))

  p <- ggplot(data, aes(x = since.diagnosis, y = perc.FVC.of.predicted))
  p <- p + geom_point()
  p <- p + geom_vline(xintercept = 2, color = "blue", linetype = "dashed")
  p <- p + geom_vline(xintercept = 3, color = "red", linetype = "dashed")
  p <- p + geom_vline(xintercept = 7, color = "red", linetype = "dashed")

  if (!is.null(patient.coef)) {
    names(patient.coef) <- c("intercept", "slope")
    patient.coef[["PtID"]] <- as.integer(rownames(patient.coef))

    patient.coef <- subset(patient.coef, PtID %in% patient.ids)

    p <- p + geom_abline(aes(intercept = intercept, slope = slope), data = patient.coef,
                         color = "green", linetype = "dashed")
  }
  
  p + facet_wrap(~ PtID, nrow = n)
}

data <- prep_data(read_data("../../data/dev.RData"), 2)
train <- subset(data, since.diagnosis < 2)
test <- subset(data, 3 <= since.diagnosis & since.diagnosis <= 7)

mp.fit <- MemPredictor(train)
results <- predict(mp.fit, test)

fvc.sd <- sd(data[["perc.FVC.of.predicted"]])

qplot((y.hat - perc.FVC.of.predicted) / fvc.sd, data = results,
      main = "Normalized Residuals for Mixed Effects Model")

compu_mae <- function(y, y.hat) mean(abs(y.hat - y))

patient.mae <- ddply(results, ~ PtID, summarize, mae = compu_mae(perc.FVC.of.predicted, y.hat))

best.patients <- arrange(patient.mae, mae)$PtID[1:16]
plot_patients(data, best.patients, coef(mp.fit)) +
              labs(title = "Patients with Best Predictive Accuracy")

mid.patients <- arrange(subset(patient.mae, 0.5 <= (mae / fvc.sd) & (mae / fvc.sd) < 1), mae)$PtID[1:16]
plot_patients(data, mid.patients, coef(mp.fit)) +
    labs(title = "Patients with N-MAE >= 0.5 and N-MAE < 1")

low.patients <- arrange(subset(patient.mae, 1 <= (mae / fvc.sd)), mae)$PtID[1:16]
plot_patients(data, low.patients, coef(mp.fit)) +
    labs(title = "Patients with N-MAE >= 1")
