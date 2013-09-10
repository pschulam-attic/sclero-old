MixedModelPredictor <- function(train) {
    library(lme4)
    library(plyr)

    m.fit <- lmer(fvc ~ year + (1 + year|PtID), data = data)
    p.coef <- coef(m.fit)[["PtID"]]

    names(p.coef) <- c("intercept", "slope")
    p.coef[["PtID"]] <- rownames(p.coef)

    structure(list(p.coef=p.coef), class="MixedModelPredictor")
}

predict.MixedModelPredictor <- function(model, test) {
    p.coef <- model[["p.coef"]]
    test <- merge(test, p.coef, by = "PtID")

    test <- transform(test, fvc.hat = intercept + slope * year)
    
    arrange(test, PtID, year)
}

coef.MixedModelPredictor <- function(model) {
    subset(model[["p.coef"]], select = c(PtID, intercept, slope))
}
