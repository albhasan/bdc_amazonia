# Get the time series of each sample.

library(dplyr)
library(sf)
library(sits)


#---- Set up ----

my_cube <- "mini_3"
samples_dir <- "./roraima/data/samples"

stopifnot(dir.exists(samples_dir))


#---- Util ----

# Checks the of the sample shapefiles.
format_sf <- function(x){
    expected_vars <- c("longitude", "latitude", "start_date",
                       "end_date", "label", "iteration", "label_pred",
                       "entropy", "id_coord")
    res <- x %>%
        dplyr::bind_cols(magrittr::set_colnames(tibble::as_tibble(sf::st_coordinates(.)),
                                                c("longitude", "latitude"))) %>%
        dplyr::mutate(start_date = "2018-08-01",
                      end_date   = "2019-07-31") %>%
        id_from_coords(col = id_coord)
    missing_vars <- setdiff(expected_vars, colnames(res))
    for(i in missing_vars){
        res[i] <- NA
    }
    return(res[expected_vars])
}



#---- Script ----

source("./roraima/scripts/00_util.R")

# Get a sits cube.
data_cube <- get_cube(my_cube)


samples_tb <- samples_dir %>%
    list.files(pattern = "samples_v[[:digit:]]+[.]shp",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename("file_path" = "value") %>%
    dplyr::mutate(sf = purrr::map(file_path, sf::read_sf)) %>%
    dplyr::mutate(sf = purrr::map(sf, format_sf))

samples_sf <- do.call(rbind, samples_tb$sf)

samples_file <- tempfile(pattern = "samples_", fileext = ".shp")
samples_sf %>%
    sf::write_sf(samples_file)

label_tb <- samples_sf %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::select(id_coord, label, label_pred, iteration)

# Get the samples' time series
samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = samples_file) %>%
    id_from_coords(col = id_coord) %>%
    dplyr::select(-label) %>%
    dplyr::left_join(label_tb, by = "id_coord") %>%
    dplyr::select(longitude, latitude, start_date, end_date,
                  label, label_pred, cube, time_series, iteration)

# Save to file
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
        return(x)
    })

# Remove names from longitude & latitude columns
for (i in 1:ncol(samples_tb2)) {
    x <- samples_tb2[[i]]
    names(x) <- NULL
    samples_tb2[[i]] <- x
}

samples_tb2 %>%
    is_sits_valid() %>%
    saveRDS(file = "./roraima/data/samples/samples_ts.rds")
