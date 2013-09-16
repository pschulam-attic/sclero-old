InterpolateTrajectory <- function(x, y) {
    if (length(x) != length(y)) {
        stop("x and y are different sizes")
    }

    n <- length(x)

    if (length(x) == 1) {
        y
    } else {

        if (x[2] - x[1] == 1) {
            c(y[1], InterpolateTrajectory(x[2:n], y[2:n]))
        } else {
            int.y <- approx(x[1:2], y[1:2], xout = x[1]:(x[2]-1))$y
            c(int.y, InterpolateTrajectory(x[2:n], y[2:n]))
        }
        
    }
}

InterpolatePatient <- function(df) {
    pid <- df[["PtID"]][1]
    x <- df$quarter
    y <- df$fvc

    if (x[1] > 0) {
        y <- c(rep(y[1], x[1]), y)
        x <- c(seq_len(x[1]) - 1, x)
    }
    
    int.x <- 0:max(x)
    int.y <- InterpolateTrajectory(x, y)
    
    data.frame(PtID = pid, quarter = int.x, fvc = int.y)
}


ComputePatientDist <- function(df1, df2) {
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

    data <- ddply(data, .(PtID), InterpolatePatient)

    structure(list(data=data, k=k), class = "KnnPredictor")
}

GetNearest <- function(patient, others, k) {
    SimilarityTo <- function(df) ComputePatientDist(patient, df)

    distances <- ddply(others, .(PtID), SimilarityTo)
    k.nearest <- arrange(distances, distance)[["PtID"]][1:k]

    k.nearest
}

PredictPatient <- function(df, train, test, k) {
    pid <- df[["PtID"]][1]
    
    patient <- subset(train, PtID == pid)
    others <- subset(train, PtID != pid)

    SimilarityTo <- function(df) ComputePatientDist(patient, df)
    
    distances <- ddply(others, .(PtID), SimilarityTo)
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
    interp.test <- ddply(interp.test, .(PtID), InterpolatePatient)
    k <- model$k

    test <- ddply(test, .(PtID), PredictPatient, train, interp.test, k)

    arrange(test, PtID, year)
}

coef.KnnPredictor <- function(model, pid) {
    train <- model$data
    k <- model$k

    patient <- subset(train, PtID == pid)
    others <- subset(train, PtID != pid)

    GetNearest(patient, others, k)
}

