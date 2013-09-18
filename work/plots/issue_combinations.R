source("include/read_severity_data.R")

library(plyr)
library(ggplot2)
library(GGally)

var.names <- c("Skin.Sev.Score", "RP.Sev.Score", "GI.Sev.Score", "perc.FVC.of.predicted")
df <- severity.data[, var.names]

df <- within(df, {
  Skin.Sev.Score <- factor(Skin.Sev.Score, levels = 0:4, ordered = TRUE)
  RP.Sev.Score <- factor(RP.Sev.Score, levels = 0:4, ordered = TRUE)
  GI.Sev.Score <- factor(GI.Sev.Score, levels = 0:4, ordered = TRUE)
})

df <- transform(df, severe.skin = Skin.Sev.Score %in% c(3, 4),
                severe.rp = RP.Sev.Score %in% c(3, 4),
                severe.gi = GI.Sev.Score %in% c(3, 4),
                severe.fvc = perc.FVC.of.predicted < 70)

df <- subset(df, select = -c(Skin.Sev.Score, RP.Sev.Score, GI.Sev.Score, perc.FVC.of.predicted))
df <- na.omit(df)

