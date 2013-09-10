BASE_DIR = .

splits: $(BASE_DIR)/data/fvc.5.visits.csv
	Rscript -e 'source("inst/R/split_data.R"); write_splits("$<", "data/train.csv", "data/test.csv")'
