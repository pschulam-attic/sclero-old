library(plyr)
library(ggplot2)
library(GGally)

load("../../data/merged_dev.RData")

data_for_var <- function(var.name) {
  df <- merged.train[, c("PtID", "Visit.Date", var.name, "DateDiagnosis")]
  df <- transform(na.omit(df), years = as.Date(Visit.Date) - as.Date(DateDiagnosis))
  n.visits <- daply(df, ~ PtID, function(d) nrow(d))

  subset(df, n.visits[as.character(PtID)] >= 3)
}

df <- data_for_var(c("perc.FVC.of.predicted", "perc.DLCO.of.predicted"))

library(lme4)

lme.fvc <- lmer(perc.FVC.of.predicted ~ years + (1 + years | PtID), data = df)
lme.dlco <- lmer(perc.DLCO.of.predicted ~ years + (1 + years | PtID), data = df)

fvc.slope <- ranef(lme.fvc)$PtID
fvc.slope <- data.frame(PtID = as.integer(rownames(fvc.slope)), fvc.slope = fvc.slope[, 2])

dlco.slope <- ranef(lme.dlco)$PtID
dlco.slope <- data.frame(PtID = as.integer(rownames(dlco.slope)), dlco.slope = dlco.slope[, 2])

slopes <- merge(fvc.slope, dlco.slope, by = "PtID")

p <- ggpairs(slopes[, c("fvc.slope", "dlco.slope")], axisLabels = "internal", title = "Lab Variable Slope Interactions",
             diag = list(continuous = "bar"),
             upper = list(continuous = "points"),
             lower = list(continuous = "density"))

p.1.2 <- ggplot(slopes, aes(x = dlco.slope, y = fvc.slope))
p.1.2 <- p.1.2 + geom_point(alpha = 0.2) +
    geom_abline(aes(slope = 1), alpha = 0.5, color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = 0), alpha = 0.5, color = "red") +
    geom_vline(aes(xintercept = 0), alpha = 0.5, color = "red")

p <- putPlot(p, p.1.2, 1, 2)

p
