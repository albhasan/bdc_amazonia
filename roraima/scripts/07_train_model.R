library(dplyr)
library(sits)

ml_method <- sits_rfor(trees = 2000)
samples_file <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/samples/samples.rds"
stopifnot(file.exists(samples_file))

samples_tb <- samples_file %>%
    readRDS()

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)

saveRDS(ml_model,
        "/home/alber.ipia/Documents/bdc_amazonia/roraima/results/active_learning/ml_model.rds")
