comp_fvc_dlco_coef <- function(df) {
  lm1 <- lm(fvc ~ year, data = df)
  lm2 <- lm(dlco ~ year, data = df)

  c1 <- coef(lm1)[2]
  c2 <- coef(lm2)[2]

  data.frame(fvc.coef = c1, dlco.coef = c2)
}

na_outliers <- function(x) {
  q1.q3 <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q1.q3)

  lb <- q1.q3[1] - 1.5 * iqr
  ub <- q1.q3[2] + 1.5 * iqr

  x[x < lb | ub < x] <- NA

  return(x)
}

library(plyr)

data <- arrange(read.csv("../../data/old/train.csv", stringsAsFactors = FALSE), PtID, year)
data$PtID <- as.factor(data$PtID)

has.missing.dlco <- ddply(data, ~ PtID, summarize, has.missing = any(is.na(dlco)))
cat(sum(!has.missing.dlco$has.missing), "patients have missing DLCO.\n")

coef.data <- ddply(data, ~ PtID, comp_fvc_dlco_coef)
coef.data$fvc.coef <- na_outliers(coef.data$fvc.coef)
coef.data$dlco.coef <- na_outliers(coef.data$dlco.coef)
coef.data <- na.omit(coef.data)

library(ggplot2)

ggplot(coef.data, aes(x = fvc.coef, y = dlco.coef)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = lm)
