source("include/read_severity_data.R")

library(ggplot2)

p <- ggplot(severity.data, aes(x = Skin.Sev.Score, y = FVC.Pre, color = factor(GI.Sev.Score)))
p + geom_jitter(height = 0, alpha = 0.5)
