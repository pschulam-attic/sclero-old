source("include/read_severity_data.R")

library(plyr)
library(ggplot2)
library(GGally)

df <- severity.data[, c("Skin.Sev.Score", "RP.Sev.Score", "GI.Sev.Score")]
df <- within(df, {
  Skin.Sev.Score <- factor(Skin.Sev.Score, levels = 0:4, ordered = TRUE)
  RP.Sev.Score <- factor(RP.Sev.Score, levels = 0:4, ordered = TRUE)
  GI.Sev.Score <- factor(GI.Sev.Score, levels = 0:4, ordered = TRUE)
})

names(df) <- c("Skin", "RP", "GI")
df <- na.omit(df)

p1 <- ggplot(df, aes(x = Skin, y = RP)) + geom_jitter(alpha = 0.2)
p2 <- ggplot(df, aes(x = Skin, y = GI)) + geom_jitter(alpha = 0.2)
p3 <- ggplot(df, aes(x = RP, y = GI)) + geom_jitter(alpha = 0.2)
p4 <- ggplot(df, aes(x = Skin, y = GI, size = RP)) + geom_jitter(alpha = 0.2)

grid.arrange(p1, p2, p3, nrow = 2, ncol = 2, main = "Clinical Var. Interactions")
