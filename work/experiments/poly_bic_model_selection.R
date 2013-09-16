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

library(lme4)

models <- lapply(formulas, function(form) lmer(form, data = data))
scores <- vapply(models, BIC, numeric(1))

qplot(0:(length(scores) - 1), scores, geom = "line")

best.model <- models[order(scores)][[1]]

library(ggplot2)

results <- transform(data, resid = resid(best.model))

ggplot(results, aes(year, resid)) +
    geom_point(alpha = 0.1)
