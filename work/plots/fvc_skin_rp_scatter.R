source("include/read_severity_data.R")

library(ggplot2)

p <- ggplot(severity.data, aes(x = Skin.Sev.Score, y = RP.Sev.Score, color = FVC.Pre))
p + geom_jitter(alpha = 0.9)
