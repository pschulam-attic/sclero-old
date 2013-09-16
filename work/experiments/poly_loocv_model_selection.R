library(plyr)

data <- arrange(read.csv("../../data/train.csv", stringsAsFactors = FALSE), PtID, year)
data$PtID <- as.factor(data$PtID)

formulas <- c(
    formula(fvc ~ 1 + (1 | PtID)),
    formula(fvc ~ poly(year, degree = 1) + (poly(year, degree = 1) | PtID)),
    formula(fvc ~ poly(year, degree = 2) + (poly(year, degree = 2) | PtID)),
    formula(fvc ~ poly(year, degree = 3) + (poly(year, degree = 3) | PtID)),
    formula(fvc ~ poly(year, degree = 4) + (poly(year, degree = 4) | PtID))
    )

n.leave.out <- 100
leave.out <- sample(nrow(data), n.leave.out)

run_loocv <- function(form, data) {
  sub.data <- lapply(leave.out, function(i) data[-i, ])
  models <- lapply(sub.data, function(d) lmer(form, data = d))

  patient.ids <- vapply(leave.out)
  model.coef <- lapply(patient.ids, )
}
