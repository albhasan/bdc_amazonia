library(dplyr)
library(ensurer)
library(lubridate)
library(purrr)
library(sits)



#--- Configuration ----

samples_file <- "/home/alber.ipia/Documents/bdc_amazonia/data/samples/samples_tb.rds"
model_file   <- "/home/alber.ipia/Documents/bdc_amazonia/results/first_classification/ml_model.rds"
stopifnot(file.exists(samples_file))
stopifnot(dir.exists(dirname(model_file)))

ml_method <- sits::sits_rfor(trees = 2000)

source("./scripts/00_util.R")



#---- Script ----

samples_tb <- samples_file %>%
    readRDS() %>%
    is_sits_valid()
samples_tb %>%
    dplyr::count(label)
# label             n
# 1 Deforestation   190
# 2 Forest          473
# 3 NatNonForest    237
# 4 Pasture         546

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)

saveRDS(object = ml_model,
        file = model_file)
