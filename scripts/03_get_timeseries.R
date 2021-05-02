source("~/Documents/bdc_access_key.R")

# Get time series for the sample points.
library(dplyr)
library(ensurer)
library(readr)
library(sits)
library(sf)
library(tibble)


#---- Set up ----

cube_name <- paste0("amazon_S2_10_16D_STK-1")
my_tiles  <- c("077095", "079082")
samples_file <- "./data/samples/samples_tb.rds"

stopifnot(file.exists(samples_file))

source("./scripts/00_util.R")



#---- Script -----

samples_tb <- samples_file %>%
    readRDS() %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    dplyr::mutate(id_coords = stringr::str_c(round(longitude, digits = 10),
                                             round(latitude,  digits = 10),
                                             sep = "_")) %>%
    # NOTE: Remove duplicates
    dplyr::distinct(id_coords, .keep_all = TRUE) %>%
    (function(x){
        my_count <- dplyr::count(x, label)
        print(my_count, n = Inf)
        print(sum(dplyr::pull(my_count, n)))
        invisible(x)
    })

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

label_tb <- samples_tb %>%
    dplyr::select(id_coords, label)

samples_tmp <- tempfile(pattern = "samples_", fileext = ".shp")

samples_sf <- samples_tb %>%
    sf::st_as_sf(coords = c("longitude", "latitude")) %>%
    sf::st_set_crs(value = 4326) %>%
    dplyr::mutate(start_date = dplyr::first(sits::sits_timeline(data_cube)) - 1,
                  end_date   = dplyr::last(sits::sits_timeline(data_cube)) + 1) %>%
    sf::write_sf(samples_tmp)

samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = samples_tmp)
samples_ts <- samples_ts %>%
    dplyr::mutate(id_coords = stringr::str_c(round(longitude, digits = 10),
                                             round(latitude,  digits = 10),
                                             sep = "_")) %>%
    dplyr::select(-label) %>%
    dplyr::left_join(label_tb, by = "id_coords") %>%
    dplyr::select("longitude", "latitude", "start_date", "end_date", "label",
                  "cube", "time_series")
samples_ts %>%
    dplyr::count(label)
# label             n
# 1 Deforestation   190
# 2 Forest          473
# 3 NatNonForest    237
# 4 Pasture         546

# Check the number of lines and columns on each time series.
samples_tb2 <- samples_ts %>%
    #NOTE: sits is retrivein the SCL column full of NAs.
    dplyr::mutate(time_series = purrr::map(time_series, function(x){
        x %>%
            dplyr::select(-SCL) %>%
            return()
        })) %>%
    (function(x){
        x %>%
            clean_ts(report = TRUE) %>%
            dplyr::group_by(label) %>%
            dplyr::summarise(ncol_mean = mean(n_cols),
                             nrol_mean = mean(n_rows),
                             sum_na = sum(has_na),
                             sum_null = sum(has_null),
                             sum_overflow = sum(has_overflow)) %>%
            print()
        invisible(x)
    })

# Remove names from longitude & latitude columns
for (i in 1:ncol(samples_tb2)) {
    x <- samples_tb2[[i]]
    names(x) <- NULL
    samples_tb2[[i]] <- x
}

samples_tb2 %>%
    is_sits_valid() %>%
    saveRDS("./data/samples/samples_tb.rds")
