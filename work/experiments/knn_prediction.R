interpolate_trajectory <- function(x, y) {
    if (length(x) != length(y)) {
        stop("x and y are different sizes")
    }

    n <- length(x)

    if (length(x) == 1) {
        y
    } else {

        if (x[2] - x[1] == 1) {
            c(y[1], interpolate_trajectory(x[2:n], y[2:n]))
        } else {
            int.y <- approx(x[1:2], y[1:2], xout = x[1]:(x[2]-1))$y
            c(int.y, interpolate_trajectory(x[2:n], y[2:n]))
        }
        
    }
}

interpolate_patient <- function(df) {
    pid <- df[["PtID"]][1]
    x <- df$quarter
    y <- df$fvc

    if (x[1] > 0) {
        y <- c(rep(y[1], x[1]), y)
        x <- c(seq_len(x[1]) - 1, x)
    }
    
    int.x <- 0:max(x)
    int.y <- interpolate_trajectory(x, y)
    
    data.frame(PtID = pid, quarter = int.x, fvc = int.y)
}


compute_patient_dist <- function(df1, df2) {
    pid1 <- df1[["PtID"]][1]
    pid2 <- df2[["PtID"]][1]

    min.length <- min(length(df1$fvc), length(df2$fvc))
    y1 <- df1$fvc[1:min.length]
    y2 <- df2$fvc[1:min.length]

    d <- (y1 - y2) ^ 2
    d <- mean(d)

    data.frame(distance = d)
}

KnnPredictor <- function(train, k) {
    library(plyr)
    
    data <- transform(train, quarter = year %/% 0.25)
    data <- ddply(data, .(PtID, quarter), summarize, fvc = mean(fvc))

    data <- ddply(data, .(PtID), interpolate_patient)

    structure(list(data=data, k=k), class = "KnnPredictor")
}

get_nearest <- function(patient, others, k) {
    SimilarityTo <- function(df) compute_patient_dist(patient, df)

    distances <- ddply(others, .(PtID), SimilarityTo)
    k.nearest <- arrange(distances, distance)[["PtID"]][1:k]

    k.nearest
}

predict_patient <- function(df, train, test, k) {
    pid <- df[["PtID"]][1]
    
    patient <- subset(train, PtID == pid)
    others <- subset(train, PtID != pid)

    similarity_to <- function(df) compute_patient_dist(patient, df)
    
    distances <- ddply(others, .(PtID), similarity_to)
    k.nearest <- arrange(distances, distance)[["PtID"]][1:k]
    
    k.nearest.data <- subset(test, PtID %in% k.nearest)
    k.nearest.data <- k.nearest.data[, setdiff(names(k.nearest.data), "fvc.hat")]
    
    quarterly.predictions <- ddply(k.nearest.data, .(quarter), summarize, fvc.hat = mean(fvc))
    quarterly.predictions <- quarterly.predictions[, c("quarter", "fvc.hat")]

    last.prediction <- arrange(quarterly.predictions, desc(quarter))[["fvc.hat"]][1]

    patient.predictions <- merge(df, quarterly.predictions, by = "quarter", all.x = TRUE)

    patient.predictions <- within(patient.predictions, {
        fvc.hat[is.na(fvc.hat)] <- last.prediction
    })

    if (nrow(patient.predictions) != nrow(df)) {
        print(df)
        stop(sprintf("Patient %d not the same", pid))
    }

    patient.predictions
}


predict.KnnPredictor <- function(model, test) {
    train <- model$data
    test <- transform(test, quarter = year %/% 0.25)
    interp.test <- ddply(test, .(PtID, quarter), summarize, fvc = mean(fvc))
    interp.test <- ddply(interp.test, .(PtID), interpolate_patient)
    k <- model$k

    test <- ddply(test, .(PtID), predict_patient, train, interp.test, k)

    arrange(test, PtID, year)
}

coef.KnnPredictor <- function(model, pid) {
    train <- model$data
    k <- model$k

    patient <- subset(train, PtID == pid)
    others <- subset(train, PtID != pid)

    get_nearest(patient, others, k)
}

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

  names(fvc.data) <- c("PtID", "Date", "fvc", "year")

  return(na.omit(fvc.data))
}

num_visits <- function(fvc.data) {
  library(plyr)

  df <- ddply(fvc.data, ~ PtID, summarize, n.visits = length(fvc))
  structure(df$n.visits, names = df$PtID)
}

prep_data <- function(fvc.data, year.thresh) {
  fvc.train <- subset(fvc.data, year < year.thresh)
  fvc.test <- subset(fvc.data, year >= (year.thresh + 1) & year < (year.thresh + 5))

  num.train <- num_visits(fvc.train)
  num.test <- num_visits(fvc.test)

  valid.train <- as.integer(names(num.train[num.train >= 3]))
  valid.test <- as.integer(names(num.test[num.test >= 1]))

  patient.ids <- intersect(valid.train, valid.test)

  subset(fvc.data, PtID %in% patient.ids)
}

data <- prep_data(read_data("../../data/dev.RData"), 2)
train <- subset(data, year >= 0 & year < 2)
test <- subset(data, 3 <= year & year <= 7)

train.pids <- unique(train$PtID)
test <- subset(test, PtID %in% train.pids)

knn.fit <- KnnPredictor(train, 5)
results <- predict(knn.fit, test)

fvc.sd <- sd(data[["fvc"]])

qplot((fvc.hat - fvc) / fvc.sd, data = results,
      main = "Normalized Residuals for KNN Model")

compu_mae <- function(y, y.hat) mean(abs(y.hat - y))

patient.mae <- ddply(results, ~ PtID, summarize, mae = compu_mae(fvc, fvc.hat))

qplot(mae / fvc.sd, data = patient.mae)

plot_neighbors <- function(pid) {
  k.nearest <- coef(knn.fit, pid)
  k.nearest.data <- subset(data, PtID %in% k.nearest)
  k.nearest.data <- transform(k.nearest.data,
                              neigh.PtID = PtID,
                              PtID = pid)

  stopifnot(nrow(k.nearest.data) > 0)

  geom_point(aes(x = year, y = fvc, color = factor(neigh.PtID)),
             alpha = 0.33, data = k.nearest.data)
}

plot_patients <- function(data, patient.ids) {
  require(ggplot2)
  patient.ids <- unique(patient.ids)
  data <- subset(data, PtID %in% patient.ids)

  n <- ceiling(sqrt(length(patient.ids)))

  p <- ggplot(data, aes(x = year, y = fvc))
  p <- p + geom_point()
  p <- p + geom_vline(xintercept = 2, color = "blue", linetype = "dashed")
  p <- p + geom_vline(xintercept = 3, color = "red", linetype = "dashed")
  p <- p + geom_vline(xintercept = 7, color = "red", linetype = "dashed")

  for (pid in patient.ids) {
    p <- p + plot_neighbors(pid)
  }

  p + facet_wrap(~ PtID, nrow = n)
}

best.patients <- arrange(patient.mae, mae)$PtID[1:16]
plot_patients(data, best.patients) +
    theme(legend.position = "none") + labs(title = "Patients with Best Predictive Accuracy")

mid.patients <- arrange(subset(patient.mae, 0.5 <= (mae / fvc.sd) & (mae / fvc.sd) < 1), mae)$PtID[1:16]
plot_patients(data, mid.patients) +
    theme(legend.position = "none") + labs(title = "Patients with N-MAE >= 0.5 and N-MAE < 1")

low.patients <- arrange(subset(patient.mae, 1 <= (mae / fvc.sd)), mae)$PtID[1:16]
plot_patients(data, low.patients) +
    theme(legend.position = "none") + labs(title = "Patients with N-MAE >= 1")
