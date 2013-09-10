library(plyr)

split_data <- function(data, p_train = 0.5) {
    patient_ids <- sort(unique(data$PtID))
    N <- length(patient_ids)
    n_train <- floor(N * p_train)
    
    train_ids <- sample(patient_ids, n_train)
    test_ids <- patient_ids[! patient_ids %in% train_ids]
    
    train <- subset(data, PtID %in% train_ids)
    test <- subset(data, PtID %in% test_ids)
    
    list(train = train, test = test)
}

write_splits <- function(data_filename, train_filename, test_filename) {
    data <- arrange(read.csv(data_filename), PtID, year)
    
    set.seed(999)
    splits <- split_data(data)
    
    write.csv(splits$train, train_filename)
    write.csv(splits$test, test_filename)
}
