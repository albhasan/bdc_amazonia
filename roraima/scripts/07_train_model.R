library(dplyr)
library(sits)

ml_method <- sits_rfor(trees = 2000)

# Train for classification
samples_file <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/samples/samples.rds"
model_file   <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/results/active_learning/ml_model_active_learning.rds"

stopifnot(file.exists(samples_file))

samples_tb <- samples_file %>%
    readRDS()
samples_tb %>%
    dplyr::count(label)

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)

saveRDS(object = ml_model,
        file = model_file)
