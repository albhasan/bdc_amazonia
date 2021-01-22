# Run the classification.

library(magrittr)
library(dplyr)
library(sf)
library(sits)

source("./roraima/scripts/00_util.R")

my_cube <- "mini_3"
num_of_trees <- 1000
my_model <- paste("rf", num_of_trees, sep = "-")
my_bands <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")
my_tiles <- "079082"
my_version <- "v010_2b"



#---- Get the samples ----

# Add only smapl
samples_tb <-
    "./roraima/data/samples/samples_ts.rds" %>%
    readRDS() %>%
    sits::sits_select(my_bands) %>%
    dplyr::filter(label != "unknown") %>%
    magrittr::set_class(class(cerrado_2classes)) %>%
    print(n = Inf)
    dplyr::filter(label != label_pred)

# NOTE: Is the dataset balanced?
samples_tb %>%
    dplyr::count(label)

samples_tb %>%
    is_sits_valid()



#---- Build a classification model ----

rfor_model <- sits::sits_train(samples_tb,
                         ml_method = sits::sits_rfor(num_trees = num_of_trees))



#---- Classify the cube ----

data_cube <- get_cube(my_cube)

# Prepare a directory to store results.
dest_dir <- file.path(getwd(), "roraima", "results",
                      my_model,
                      paste(sort(my_bands), collapse = "-"),
                      my_version)
dir.create(dest_dir, recursive = TRUE)

print("Classification start time: ")
(start_time <- Sys.time())
s2_probs <- sits::sits_classify(data_cube,
                                ml_model = rfor_model,
                                memsize = 2,
                                multicores = 1,
                                output_dir = dest_dir)
label_tb <- sits::sits_labels(s2_probs) %>%
    tibble::as_tibble() %>%
    dplyr::rename(label = "value") %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    dplyr::select(id, label)
label_tb %>%
    write.csv(file.path(dest_dir, "sits_labels.csv"))

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



#---- Post processing ----

print("Bayesian start time: ")
(start_time <- Sys.time())
s2_bayes <- sits::sits_smooth_bayes(s2_probs,
                                    output_dir = dest_dir)
s2_label <- sits::sits_label_classification(s2_bayes,
                                            output_dir = dest_dir)
print(paste("Bayesian start time: ", start_time))
print(paste("Bayesian end time: ", end_time))
print("Bayesian duration: ")
(end_time - start_time)

print("Classification finished. The resutls are stored at:")
print(dest_dir)

# Compute the entropy
prob_file <- paste0(file.path(dest_dir, s2_probs$bands[[1]]), "_v1.tif")
entropy_file <- paste0(file.path(dest_dir, s2_probs$bands[[1]]), "_entropy_v1.tif")
compute_entropy(prob_file, entropy_file)


#---- Choose new samples for the oracle ----

class_file <- s2_label$file_info[[1]]$path

oracle_samples <- class_file %>%
    raster::raster() %>%
    raster::sampleStratified(size = 400, xy = TRUE, sp = TRUE) %>%
    (function(y) {
        return(raster::extract(x = raster::raster(entropy_file),
                               y = y, sp = TRUE))
    }) %>%
    sf::st_as_sf() %>%
    dplyr::rename_with(.fn = function(x){return(c("cell", "x", "y",
                                                  "label_pred", "entropy",
                                                  "geometry"))})
oracle_samples <- oracle_samples %>%
    dplyr::group_by(label_pred) %>%
    dplyr::arrange(entropy,
                   .by_group = TRUE) %>%
    dplyr::slice_tail(prop = 0.10) %>%
    dplyr::slice_head(prop = 0.5) %>%
    dplyr::sample_n(5) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cell, -x, -y) %>%
    sf::st_transform(4326) %>%
    dplyr::left_join(y = label_tb,
                     by = c("label_pred" = "id")) %>%
    dplyr::select(-label_pred) %>%
    dplyr::rename(label_pred = label)
oracle_samples %>%
    dplyr::mutate(label = NA_character_,
                  iteration = NA_integer_) %>%
    sf::write_sf("./roraima/data/samples/samples_for_oracle.shp")


