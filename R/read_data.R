ReadData <- function(filename) {
    library(plyr)
    
    data <- arrange(read.csv(filename), PtID, year)
    
    has.missing.data <- ddply(data, .(PtID), summarize,
                              has.missing = any(any(is.na(aca)),
                                                any(is.na(topo)),
                                                any(is.na(rnp)),
                                                any(is.na(pol3)),
                                                any(is.na(dlco))))
    
    complete.pids <- unique(subset(has.missing.data, !has.missing)$PtID)
    
    clean.data <- subset(data, PtID %in% complete.pids,
                         select = c(PtID, year, fvc, dlco,
                                    strFemale, strRaceId1,
                                    aca, topo, rnp, pol3))
    clean.data <- arrange(clean.data, PtID, year)
    
    data.factors <- c("PtID", "strFemale", "strRaceId1",
                      "aca", "topo", "rnp", "pol3")
    
    for (coln in data.factors) {
        clean.data[[coln]] <- as.factor(clean.data[[coln]])
    }

    clean.data[["fvc.sd"]] <- sd(clean.data[["fvc"]])
    arrange(clean.data, PtID, year)
}
