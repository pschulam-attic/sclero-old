main <- function(patient.filename, visit.filename) {
  patient.data <- read.csv(patient.filename, stringsAsFactors = FALSE)
  visit.data <- read.csv(visit.filename, stringsAsFactors = FALSE)

  patient.ids <- unique(patient.data$PtID)

  visit.data <- visit.data[, c("PtID", "Visit.Date")]
  visit.data[["Date"]] <- as.Date(visit.data[["Visit.Date"]])

  visit.data <- subset(visit.data, PtID %in% patient.ids)

  library(plyr)

  patient.first.seen <- as.Date(patient.data[["DateFirstSeen"]])
  names(patient.first.seen) <- patient.data[["PtID"]]

  visit.diff.data <- transform(visit.data, diff = Date - patient.first.seen[as.character(PtID)])


  time.since <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)

  get.patient.visits <- function(yr) {
    n.days <- 365 * yr

    visit.diff.data <- subset(visit.diff.data, diff < n.days)

    n.visits <- ddply(visit.diff.data, ~ PtID, summarize, n.visits = length(diff))
    n.visits[["year"]] <- yr

    n.visits
  }

  all.data <- do.call("rbind", lapply(time.since, get.patient.visits))

  library(ggplot2)
  ggplot(all.data, aes(x = factor(n.visits))) + geom_bar() + facet_wrap(~ year, ncol = 1)
}
