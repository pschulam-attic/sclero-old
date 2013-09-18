merged.filename <- "../../data/merged_dev.RData"

load(merged.filename)  # Loads merged.train data frame

library(plyr)

merged.train <- arrange(merged.train, PtID, Visit.Date)

clinic.vars <- c("Total.Skin.Score", "Skin.Sev.Score", "RP.Sev.Score", "GI.Sev.Score")
lab.vars <- c("FVC.Pre", "perc.FVC.of.predicted", "DLCO", "perc.DLCO.of.predicted")

worst_measurements <- function(df) {
  all.vars <- c(clinic.vars, lab.vars)
  
  worst <- vector(mode = "list", length = length(all.vars))
  names(worst) <- all.vars

  for (i in seq_along(clinic.vars)) {
    v <- clinic.vars[i]
    worst[[v]] <- floor(median(quantile(df[[v]], 0.9, na.rm = TRUE)))
    worst[[v]] <- if (is.infinite(worst[[v]])) NA else worst[[v]]
  }

  for (i in seq_along(lab.vars)) {
    v <- lab.vars[i]
    worst[[v]] <- median(quantile(df[[v]], 0.10, na.rm = TRUE))
    worst[[v]] <- if (is.infinite(worst[[v]])) NA else worst[[v]]
  }
  
  as.data.frame(worst)
}

severity.data <- ddply(merged.train, ~ PtID, worst_measurements)
