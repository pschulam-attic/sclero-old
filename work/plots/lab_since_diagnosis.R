main <- function(patient.filename, pft.filename) {
  patient.data <- read.csv(patient.filename, stringsAsFactors = FALSE)
  pft.data <- read.csv(pft.filename, stringsAsFactors = FALSE)

  patient.ids <- unique(patient.data$PtID)

  pft.data <- pft.data[, c("PtID", "Date")]
  pft.data[["Date"]] <- as.Date(pft.data[["Date"]])

  pft.data <- subset(pft.data, PtID %in% patient.ids)

  library(plyr)

  patient.first.seen <- as.Date(patient.data[["DateDiagnosis"]])
  names(patient.first.seen) <- patient.data[["PtID"]]

  pft.diff.data <- transform(pft.data, diff = Date - patient.first.seen[as.character(PtID)])


  time.since <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)

  get.patient.visits <- function(yr) {
    n.days <- 365 * yr

    pft.diff.data <- subset(pft.diff.data, diff < n.days)

    n.visits <- ddply(pft.diff.data, ~ PtID, summarize, n.visits = length(diff))
    n.visits[["year"]] <- yr

    n.visits
  }

  all.data <- do.call("rbind", lapply(time.since, get.patient.visits))

  library(ggplot2)
  ggplot(all.data, aes(x = factor(n.visits))) + geom_bar() + facet_wrap(~ year, ncol = 1)
}
