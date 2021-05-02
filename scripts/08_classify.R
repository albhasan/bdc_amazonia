.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

Sys.setenv("__SITS_DEBUG__" = TRUE)
Sys.setenv("__SITS_RESUME__" = TRUE)

library(sits)
library(dplyr)



#---- Configuration ----

# Level model (this is for the BIOME)
classification_name <- "first_classification"
my_tiles  <- c("077095", "079082")

## Level data (for list of tiles in the BIOME)
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



#---- Classify ----
start_time <- Sys.time()

samples_tb <- samples_file %>%
    readRDS()

date_range <- samples_tb %>%
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

probs <- sits::sits_classify(data_cube,
                             ml_model = readRDS(model_file),
                             memsize = 15,
                             multicores = 10,
                             output_dir = out_dir)
probs <- dplyr::mutate(probs,
                       processing = tibble::tibble(start_time = start_time,
                                                   end_time = Sys.time()))
saveRDS(probs, file = file.path(out_dir, paste0(cube_name, "_results.rds")))
