load("../../data/merged_dev.RData")

year_bw_dates <- function(from.date, to.date) as.integer(to.date - from.date) / 365

fvc.data <- merged.train[, c("PtID", "Visit.Date", "perc.FVC.of.predicted", "DateFirstSeen")]
fvc.data <- na.omit(fvc.data)

fvc.data <- transform(fvc.data, year = year_bw_dates(as.Date(DateFirstSeen), as.Date(Visit.Date)))

library(plyr)

n_visits_before <- function(before.year, fvc.data) {
  fvc.data <- subset(fvc.data, year < before.year)
  n.visits.data <- ddply(fvc.data, ~ PtID, summarize, n.visits = length(Visit.Date))

  patients.per.nvisits <- ddply(n.visits.data, ~ n.visits, summarize, n.patients = length(PtID))
  patients.per.nvisits <- transform(patients.per.nvisits, p.patients = (n.patients / sum(n.patients)) * 100)
  patients.per.nvisits[["before"]] <- before.year

  patients.per.nvisits
}

before.years <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)

n.visits.data <- lapply(before.years, function(y) n_visits_before(y, fvc.data))
n.visits.data <- do.call(rbind, n.visits.data)
n.visits.data <- arrange(n.visits.data, before, n.visits)
n.visits.data <- ddply(n.visits.data, ~ before, transform, pr.greater.or.eq = p.patients + (100 - cumsum(p.patients)))

library(ggplot2)

p <- ggplot(n.visits.data, aes(x = factor(n.visits), y = pr.greater.or.eq))
p <- p + geom_bar() + facet_wrap(~ before, ncol = 1)
p + labs(title = "% Patients Since First Seen", x = "At Least X Visits", y = "% Patients")
