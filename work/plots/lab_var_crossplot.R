main <- function(pft.filename) {
  data <- read.csv(pft.filename, stringsAsFactors = FALSE)

  vars <- c("FEV1.Pre", "FVC.Pre", "TLC_HE", "VC_HE", "FRC_HE", "RV_HE", "DLCO", "VI", "ALV")
  pft.data <- data[, vars]
  plot(pft.data)
}
