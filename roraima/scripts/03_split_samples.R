# Split the samples into train, test, and validation roles.

library(magrittr)
library(dplyr)
library(sf)
library(sits)

source("./roraima/scripts/util.R")

grid_size <- 1024

#---- Configuration ----


#---- Get the samples ----

samples_tb <- "./roraima/data/samples/samples_ts.rds" %>%
    readRDS() %>%
    is_sits_valid() %>%
    id_from_coords()

# Split the samples spatially
samples_sf <- samples_tb %>%
    dplyr::select(id_coords, longitude, latitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326)
#set.seed(123) # all bands
#set.seed(234) # 7 bands
set.seed(345)  # samples including the label "other".
samples_grid <- samples_sf %>%
    sf::st_make_grid(n = rep(sqrt(grid_size), 2)) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(bin_id = sample(1:grid_size, size = grid_size))
# Give each polygon a bin
bin_break <- floor(seq(from = 1,
                 to = grid_size,
                 by = grid_size / 4))
samples_sf <- samples_sf %>%
    sf::st_join(samples_grid) %>%
    ensurer::ensure_that(sum(is.na(.$bin_id)) == 0,
                         err_desc = "Some samples are missing a bin_id") %>%
    dplyr::mutate(role = dplyr::case_when(bin_id %in% 1:bin_break[2]            ~ "train",
                                          bin_id %in% bin_break[2]:bin_break[3] ~ "train",
                                          bin_id %in% bin_break[3]:bin_break[4] ~ "test",
                                          TRUE                                  ~ "validation"))
samples_tb <- samples_tb %>%
    dplyr::left_join(samples_sf %>%
                         sf::st_drop_geometry() %>%
                         dplyr::select(id_coords, bin_id, role),
                     by = "id_coords") %>%
    dplyr::select(-id_coords)
samples_tb  %T>%
    (function(x){
        x %>%
            sf::st_as_sf(coords = c("longitude", "latitude"),
                         crs = 4326) %>%
            dplyr::select(label, role, bin_id) %>%
            plot()
        return(x)
    }) %>%
    dplyr::select(-time_series) %>%
    dplyr::group_by(label, role) %>%
    dplyr::summarise(samples = n())

#---- Initial split ----

train_tb <- samples_tb %>%
    dplyr::filter(role == "train") %>%
    dplyr::select(-bin_id, -role) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Missing training samples!") %>%
    (function(x){
        x %>%
            saveRDS(file = "./roraima/data/samples/samples_train.rds")
        invisible(x)
    })
test_tb <- samples_tb %>%
    dplyr::filter(role == "train") %>%
    dplyr::select(-bin_id, -role) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Missing testing samples!") %>%
    (function(x){
        x %>%
            saveRDS(file = "./roraima/data/samples/samples_test.rds")
        invisible(x)
    })
samples_tb %>%
    dplyr::filter(role == "validation") %>%
    dplyr::select(-bin_id, -role) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Missing validation samples!") %>%
    saveRDS(file = "./roraima/data/samples/samples_validation.rds")
