library(dplyr)
library(ensurer)
library(readr)
library(sf)
library(sits)

samples_csv <- "./data/samples/alber3_bdc077095.csv"
samples_shp <- "./data/samples/samples_079082.shp"
stopifnot(file.exists(samples_csv))
stopifnot(file.exists(samples_shp))

# Join the samples from different tiles.

samples_077095_tb <- samples_csv %>%
    readr::read_csv(
        col_types = list(
            id         = col_integer(),
            longitude  = col_double(),
            latitude   = col_double(),
            label      = col_character(),
            start_date = col_date(format = ""),
            end_date   = col_date(format = "")
        )) %>%
    dplyr::mutate(cube = NA_character_,
                  time_series = NA_character_) %>%
    dplyr::select(longitude, latitude, start_date, end_date,
                  label, cube, time_series)

# NOTE: Use the samples of 077095 as reference.
date_range <- samples_077095_tb %>%
    dplyr::select(start_date, end_date) %>%
    dplyr::distinct() %>%
    ensurer::ensure_that(nrow(.) == 1,
                         err_desc = "Sample dates must be unique!") %>%
    dplyr::summarise(start = min(start_date),
                     end   = max(end_date))

samples_079082_tb <- samples_shp %>%
    sf::read_sf() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::rename(longitude = longitd,
                  latitude = latitud,
                  start_date = strt_dt,
                  end_date   = end_dat,
                  time_series = tim_srs) %>%
    dplyr::mutate(start_date = as.Date(start_date),
                  end_date   = as.Date(end_date)) %>%
    dplyr::select(longitude, latitude, start_date, end_date,
                  label, cube, time_series)

samples_tb <- samples_077095_tb %>%
    dplyr::bind_rows(samples_079082_tb) %>%
    mutate(cube = "bdc_amazon",
           start_date = dplyr::first(date_range),
           end_date   = dplyr::last(date_range))

class(samples_tb) <- class(cerrado_2classes)

samples_tb %>%
    saveRDS(file = "./data/samples/samples_tb.rds")
