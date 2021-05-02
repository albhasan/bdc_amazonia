.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

library(dplyr)
library(readr)
library(sits)



#---- set up level classification ----

classification_name <- "first_classification"
my_tiles  <- c("077095", "079082")

project_dir   <- "/home/alber.ipia/Documents/bdc_amazonia"
out_dir <- paste0(project_dir, "/results/", classification_name)

cube_name <- paste0("amazon_S2_10_16D_STK-1")
model_file    <- paste0("/home/alber.ipia/Documents/bdc_amazonia/results/",
                        classification_name,
                        "/ml_model.rds")
samples_file <- "./data/samples/samples_tb.rds"

stopifnot(file.exists(model_file))
stopifnot(file.exists(samples_file))
stopifnot(dir.exists(out_dir))


#---- Script ----

my_labels <- model_file %>%
    readRDS(model_file) %>%
    environment() %>%
    magrittr::extract2("data") %>%
    dplyr::pull(label) %>%
    unique() %>%
    sort()
my_labels %>%
    readr::write_lines(file = file.path(out_dir, "labels.txt"))

date_range <- samples_file %>%
    readRDS() %>%
    dplyr::select(start_date, end_date) %>%
    dplyr::distinct() %>%
    ensurer::ensure_that(nrow(.) == 1,
                         err_desc = "Invalid date range.")

data_cube <- sits::sits_cube(source = "BDC",
                             name = cube_name,
                             url = "http://datacube-005.dpi.inpe.br:8010/stac/",
                             collection = "S2_10_16D_STK-1",
                             tiles = my_tiles,
                             start_date = date_range$start_date[[1]],
                             end_date   = date_range$end_date[[1]])

prob_files <- "/home/alber.ipia/Documents/bdc_amazonia/results" %>%
    file.path(classification_name) %>%
    list.files(pattern = paste0("^", cube_name, ".+tif$"),
               full.names = TRUE) %>%
    ensurer::ensure_that(length(.) == length(my_tiles),
                         err_desc = "Probability file not found!")

for (my_file in prob_files) {
    probs_cube <- sits::sits_cube(source = "PROBS",
                                  name = paste0(cube_name, "_",
                                   tools::file_path_sans_ext(basename(my_file))),
                                  satellite = "SENTINEL-2",
                                  sensor = "MSI",
                                  start_date = date_range$start_date[[1]],
                                  end_date   = date_range$end_date[[1]],
                                  probs_labels = my_labels,
                                  probs_files = my_file)

    bayesian <- sits::sits_smooth(probs_cube,
                                  type = "bayes",
                                  window_size = 5,
                                  multicores = 10,
                                  memsize = 2,
                                  output_dir = out_dir)

    sits::sits_label_classification(bayesian,
                                    multicores = 10,
                                    memsize = 2,
                                    output_dir = out_dir)
}
