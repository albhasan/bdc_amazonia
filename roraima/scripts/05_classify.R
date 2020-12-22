# Run the classification.

library(magrittr)
library(dplyr)
library(sf)
library(sits)

source("./roraima/scripts/util.R")

num_of_trees <- 1000
my_model <- paste("rf", num_of_trees, sep = "-")
my_bands <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")
my_cube <- "S2_10_16D_STK"
my_tiles <- "079082"

my_version <- "v005_2"
#cube_dir <- "/http/s2/S2_10_16D_STK/v001/079082"              # Whole cube
#cube_dir <- "/http/s2/mini/brick_1/S2_10_16D_STK/v001/079082" # Mini cube 1
cube_dir <- "/http/s2/mini/brick_2/S2_10_16D_STK/v001/079082" # Mini cube 2


#---- Get the samples ----

train_tb <- "./roraima/data/samples/samples_train.rds" %>%
    readRDS()
test_tb <- "./roraima/data/samples/samples_test.rds" %>%
    readRDS()
samples_tb <- train_tb %>%
    dplyr::bind_rows(test_tb) %>%
    sits::sits_select(my_bands)

# NOTE: Is the dataset balanced?
samples_tb %>%
    dplyr::count(label)

# minimum_n <- samples_tb %>%
#     dplyr::pull(label) %>%
#     table() %>%
#     min()
#
# set.seed(666)
# samples_tb <- samples_tb %>%
#     dplyr::group_by(label) %>%
#     dplyr::sample_n(size = minimum_n) %>%
#     dplyr::ungroup()
# rm(minimum_n)

class(samples_tb) <- class(cerrado_2classes)
samples_tb %>%
    is_sits_valid()

rm(train_tb, test_tb)

#---- Build a classification model ---

rfor_model <- sits::sits_train(samples_tb,
                         ml_method = sits::sits_rfor(num_trees = num_of_trees))

stack_cube <- sits::sits_cube(type        = "RASTER",
                              name        = "roraima",
                              satellite   = "SENTINEL-2",
                              sensor      = "MSI",
                              resolution  = "10m",
                              data_dir    = cube_dir,
                              parse_info  = c("x1", "x2", "x3", "x4", "x5", "x6",
                                              "date", "x8", "band"),
                              delim       = "_")

# Prepare a directory to store results.
dest_dir <- file.path(getwd(), "results",
                      my_model,
                      paste(sort(my_bands), collapse = "-"),
                      my_version)
dir.create(dest_dir, recursive = TRUE)

# Classify the cube.
print("Classification start time: ")
(start_time <- Sys.time())
s2_probs <- sits::sits_classify(stack_cube,
                                ml_model = rfor_model,
                                memsize = 2,
                                multicores = 1,
                                output_dir = dest_dir)
end_time <- Sys.time()
print(paste("Classification start time: ", start_time))
print(paste("Classification end time: ", end_time))
print("Classification duration: ")
(end_time - start_time)


# ALL BANDS
# Starting classification at 2020-10-01 22:11:28
# end                       "2020-10-02 21:22:59 UTC"
# 7 BANDS & CLOUD INTERPOLATION
# Classification finished at 2020-10-11 18:26:22. Total elapsed time: 2607.3 minute(s).
# [1] "Classification start time:  2020-10-09 22:59:01"
# [1] "Classification end time:  2020-10-11 18:34:41"
# [1] "Classification duration: "
# Time difference of 1.816435 days

print("Bayesian start time: ")
(start_time <- Sys.time())
s2_label <- sits::sits_label_classification(s2_probs,
                                            smoothing = "bayesian",
                                            output_dir = dest_dir)
print(paste("Bayesian start time: ", start_time))
print(paste("Bayesian end time: ", end_time))
print("Bayesian duration: ")
(end_time - start_time)

print("Classification finished. The resutls are stored at:")
print(dest_dir)
