clinic.vars <- c("Total.Skin.Score", "Skin.Sev.Score", "RP.Sev.Score", "GI.Sev.Score")
lab.vars <- c("FVC.Pre", "perc.FVC.of.predicted", "DLCO", "perc.DLCO.of.predicted")

read_visit_data <- function(visit.filename) {
  visit.data <- read.csv(visit.filename, stringsAsFactors = FALSE)
  visit.data <- visit.data[, c("PtID", "Visit.Date", clinic.vars)]

  visit.data[["PtID"]] <- as.factor(visit.data[["PtID"]])
  visit.data[["Visit.Date"]] <- as.Date(visit.data[["Visit.Date"]])

  ## visit.data[["Skin.Sev.Score"]] <- as.factor(visit.data[["Skin.Sev.Score"]])
  ## visit.data[["RP.Sev.Score"]] <- as.factor(visit.data[["RP.Sev.Score"]])
  ## visit.data[["GI.Sev.Score"]] <- as.factor(visit.data[["GI.Sev.Score"]])

  library(plyr)
  return(arrange(visit.data, PtID, Visit.Date))
}

read_pft_data <- function(pft.filename) {
  pft.data <- read.csv(pft.filename, stringsAsFactors = FALSE)
  pft.data <- pft.data[, c("PtID", "Date", lab.vars)]

  pft.data[["PtID"]] <- as.factor(pft.data[["PtID"]])
  pft.data[["Date"]] <- as.Date(pft.data[["Date"]])

  names(pft.data) <- c("PtID", "Pft.Date", lab.vars)

  library(plyr)
  return(arrange(pft.data, PtID, Pft.Date))
}

main <- function(visit.filename, pft.filename) {
  visit.data <- read_visit_data(visit.filename)
  pft.data <- read_pft_data(pft.filename)

  merged.data <- merge(visit.data, pft.data,
                       by.x = c("PtID", "Visit.Date"),
                       by.y = c("PtID", "Pft.Date"),
                       all = TRUE)

  worst_measurements <- function(df) {
    all.vars <- c(clinic.vars, lab.vars)
    
    worst <- vector(mode = "list", length = length(all.vars))
    names(worst) <- all.vars

    for (i in seq_along(clinic.vars)) {
      v <- clinic.vars[i]
      worst[[v]] <- floor(median(quantile(df[[v]], 0.9, na.rm = TRUE)))
      #worst[[v]] <- max(df[[v]], na.rm = TRUE)
      worst[[v]] <- if (is.infinite(worst[[v]])) NA else worst[[v]]
    }

    for (i in seq_along(lab.vars)) {
      v <- lab.vars[i]
      worst[[v]] <- median(quantile(df[[v]], 0.10, na.rm = TRUE))
      #worst[[v]] <- min(df[[v]], na.rm = TRUE)
      worst[[v]] <- if (is.infinite(worst[[v]])) NA else worst[[v]]
    }

    as.data.frame(worst)
  }

  library(plyr)
  
  worst.data <- ddply(merged.data, ~ PtID, worst_measurements)
  worst.data[["Skin.Sev.Score"]] <- as.factor(worst.data[["Skin.Sev.Score"]])
  worst.data[["RP.Sev.Score"]] <- as.factor(worst.data[["RP.Sev.Score"]])
  worst.data[["GI.Sev.Score"]] <- as.factor(worst.data[["GI.Sev.Score"]])

  worst.data
}

plot_categorical <- function(worst.data) {
  library(GGally)
  ggpairs(worst.data, columns = c(3, 4, 5), title = "Clinical Variable Interactions",
          axisLabels = "internal",
          upper = list(discrete = "ratio"),
          lower = list(discrete = "blank")
          )  
}

plot_continuous <- function(worst.data) {
  library(GGally)
  worst.data <- na.omit(worst.data[, c(6, 8)])
  ggpairs(worst.data, axisLabels = "show", title = "Lab Variable Interactions",
          diag = list(continuous = "bar"),
          upper = list(continuous = "points"),
          lower = list(continuous = "cor")
          )
}

plot_combo_fvc <- function(worst.data) {
  library(GGally)
  worst.data <- na.omit(worst.data[, c(3, 4, 5, 6)])
  ggpairs(worst.data, axisLabels = "show",
          title = "Clinical Var. Interaction with FVC",
          diag = list(continuous = "bar", discrete = "bar"),
          upper = list(discrete = "ratio", combo = "box"),
          lower = list(discrete = "blank", combo = "blank")
          )
}

plot_combo_dlco <- function(worst.data) {
  library(GGally)
  worst.data <- na.omit(worst.data[, c(3, 4, 5, 8)])
  ggpairs(worst.data, axisLabels = "show",
          title = "Clinical Var. Interaction with DLCO",
          diag = list(continuous = "bar", discrete = "bar"),
          upper = list(discrete = "ratio", combo = "box"),
          lower = list(discrete = "blank", combo = "blank")
          )
}

plot_fvc_skin_corr <- function(worst.data) {
  qs <- ddply(worst.data, ~ Skin.Sev.Score, summarize, FVC.Pre = quantile(FVC.Pre, c(0.25, 0.5, 0.75), na.rm = TRUE), quant = c(0.25, 0.5, 0.75))
  ggplot(na.omit(qs), aes(x = Skin.Sev.Score, y = FVC.Pre)) + geom_point() + facet_wrap(~ quant) + labs(title = "Quartiles Against Skin Severity")
}

plot_fvc_gi_corr <- function(worst.data) {
  qs <- ddply(worst.data, ~ GI.Sev.Score, summarize, FVC.Pre = quantile(FVC.Pre, c(0.25, 0.5, 0.75), na.rm = TRUE), quant = c(0.25, 0.5, 0.75))
  ggplot(na.omit(qs), aes(x = GI.Sev.Score, y = FVC.Pre)) + geom_point() + facet_wrap(~ quant) + labs(title = "Quartiles Against GI Severity")
}
