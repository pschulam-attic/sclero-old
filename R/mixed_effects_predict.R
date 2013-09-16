MixedModelPredictor <- function(train, x.var, y.var) {
    library(lme4)
    library(plyr)

    v.env <- list(x.var = as.name(x.var), y.var = as.name(y.var))
    formula <- substitute(y.var ~ x.var + (1 + x.var|PtID), v.env)
    
    m.fit <- lmer(formula, data = train)
    p.coef <- coef(m.fit)[["PtID"]]

    names(p.coef) <- c("intercept", "slope")
    p.coef[["PtID"]] <- rownames(p.coef)

    structure(list(fit=m.fit, p.coef=p.coef, x.var=x.var, y.var=y.var), class="MixedModelPredictor")
}

predict.MixedModelPredictor <- function(model, test) {
    x.var <- model[["x.var"]]
    y.var <- model[["y.var"]]
    p.coef <- model[["p.coef"]]
    test <- merge(test, p.coef, by = "PtID")

    y.var.hat <- paste0(y.var, ".hat")
    test[[y.var.hat]] <- test$intercept + test$slope * test[[x.var]]

    test[with(test, order(PtID, eval(as.name(x.var)))), ]
}

coef.MixedModelPredictor <- function(model) {
    subset(model[["p.coef"]], select = c(PtID, intercept, slope))
}
