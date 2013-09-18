pft.data <- read.csv("../../data/sclerodata/tPFT_dev.csv", stringsAsFactors = FALSE)
pft.data <- transform(pft.data, Date = as.Date(Date))
pft.data <- arrange(pft.data, PtID, Date)
    
patient.data <- read.csv("../../data/sclerodata/tPtData_dev.csv", stringsAsFactors = FALSE)

patient.data <- arrange(patient.data, PtID)
since.diagnosis <- structure(as.Date(patient.data$DateDiagnosis), names = patient.data$PtID)

pft.data <- transform(pft.data, years.since.diag = as.integer(Date - since.diagnosis[as.character(PtID)]) / 365)

q1.q3 <- with(pft.data, quantile(years.since.diag, c(0.25, 0.75), na.rm = TRUE))
iqr <- with(pft.data, IQR(years.since.diag, na.rm = TRUE))

has.missing.fvc <- with(pft.data, is.na(FVC.Pre))
is.outlier <- with(pft.data, years.since.diag < q1.q3[1] - 1.5 * iqr | q1.q3[2] + 1.5 * iqr < years.since.diag)

pft.data <- pft.data[!has.missing.fvc & !is.outlier, ]

num.visits <- ddply(pft.data, ~ PtID, summarize, visits = length(Date))
num.visits <- structure(num.visits$visits, names = num.visits$PtID)

pft.data <- subset(pft.data, num.visits[as.character(PtID)] > 3)

library(lme4)

lme.fit <- lmer(FVC.Pre ~ years.since.diag + (1 + years.since.diag | PtID), data = pft.data)
patient.coef <- coef(lme.fit)$PtID
names(patient.coef) <- c("intercept", "slope")
patient.coef <- as.data.frame(patient.coef)
patient.coef$PtID <- as.integer(rownames(patient.coef))

patient.ids <- unique(pft.data$PtID)
some.patients <- sample(patient.ids, 16)

library(ggplot2)

qplot(slope, data = patient.coef, main = "Slope Histogram")

p <- ggplot(subset(pft.data, PtID %in% some.patients), aes(x = years.since.diag, y = FVC.Pre))
p + geom_point() + geom_abline(aes(intercept = intercept, slope = slope),
                               data = subset(patient.coef, PtID %in% some.patients),
                               color = "blue", linetype = "dashed") +
    facet_wrap(~ PtID, nrow = 4) + labs(title = "Sample Trajectories with MEM Fit")
