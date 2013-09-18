source("include/read_severity_data.R")

library(plyr)
library(ggplot2)
library(GGally)

df <- severity.data[, c("Skin.Sev.Score", "RP.Sev.Score", "GI.Sev.Score", "perc.FVC.of.predicted", "perc.DLCO.of.predicted")]
df <- within(df, {
  Skin.Sev.Score <- factor(Skin.Sev.Score, levels = 0:4, ordered = TRUE)
  RP.Sev.Score <- factor(RP.Sev.Score, levels = 0:4, ordered = TRUE)
  GI.Sev.Score <- factor(GI.Sev.Score, levels = 0:4, ordered = TRUE)
})

names(df) <- c("Skin", "RP", "GI", "pFVC", "pDLCO")

ggpairs(na.omit(df), axisLabels = "show",
        title = "Clinical Var. Interaction with FVC/DLCO",
        diag = list(continuous = "bar", discrete = "bar"),
        upper = list(discrete = "ratio", combo = "facethist", continuous = "points"),
        lower = list(discrete = "blank", combo = "blank", continuous = "cor")
        )
