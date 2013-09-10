PlotExtremes <- function(data, test, extreme = "low", k = 5) {
    library(ggplot2)
    library(plyr)
    
    error.summary <- ddply(test, .(PtID), summarize, mse = mean(resid ** 2))
    
    if (extreme == "low") {
        top.k.patients <- arrange(error.summary, mse)$PtID[1:k]
    } else {
        top.k.patients <- arrange(error.summary, desc(mse))$PtID[1:k]
    }
    
    patient.data <- subset(data, PtID %in% top.k.patients)
    
    p <- ggplot(patient.data, aes(x = year, y = fvc,
                                  group = factor(PtID),
                                  color = factor(PtID)))
    p + geom_point() + geom_line()
}