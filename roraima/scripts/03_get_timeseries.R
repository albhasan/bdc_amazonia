# Get time series for the sample points.
library(dplyr)
library(readr)
library(sits)
library(sf)
library(tidyr)

#---- Set up ----

#samples_file <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/samples/samples.shp"
samples_dir <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/samples"

source("/home/alber.ipia/Documents/bdc_amazonia/roraima/scripts/00_util.R")

stopifnot(dir.exists(samples_dir))


#---- Script -----

data_cube <- get_cube("cube")

cube_timeline <- data_cube %>%
    sits::sits_timeline()

samples_tb <- samples_dir %>%
    list.files(pattern = "*.shp",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(sf_obj = purrr::map(value, function(x){
        x %>%
            sf::read_sf() %>%
            add_coords() %>%
            sf::st_set_geometry(NULL) %>%
            tibble::as_tibble() %>%
            dplyr::mutate(start_date =  cube_timeline %>%
                              dplyr::first() %>%
                              as.character(),
                          end_date = cube_timeline %>%
                              dplyr::last() %>%
                              as.character(),
                          cube = NA_character_,
                          time_series = NA_character_) %>%
            dplyr::select(longitude, latitude, start_date,
                          end_date, label, cube, time_series)
        })) %>%
    dplyr::select(sf_obj) %>%
    tidyr::unnest(cols = c(sf_obj))
samples_tb %>%
    dplyr::count(label)

label_tb <- samples_tb %>%
    dplyr::mutate(id_coords = stringr::str_c(
        round(longitude, digits = 6),
        round(latitude, digits = 6),
        sep = "_")) %>%
    dplyr::select(id_coords, label)

samples_tmp <- tempfile(pattern = "samples_", fileext = ".csv")
samples_tb %>%
   readr::write_csv(samples_tmp)
stopifnot(file.exists(samples_tmp))

samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = samples_tmp)
samples_ts <- samples_ts %>%
    dplyr::mutate(id_coords = stringr::str_c(
        round(longitude, digits = 6),
        round(latitude, digits = 6),
        sep = "_")) %>%
    dplyr::select(-label) %>%
    dplyr::left_join(label_tb, by = "id_coords") %>%
    dplyr::select("longitude", "latitude", "start_date", "end_date", "label",
                  "cube", "time_series")

# Check the number of lines and columns on each time series.
samples_tb2 <- samples_ts %>%
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
    saveRDS("./roraima/data/samples/samples.rds")
