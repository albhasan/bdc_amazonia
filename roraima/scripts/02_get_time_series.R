# Get the time series of each sample.

library(dplyr)
library(sf)
library(sits)

source("/home/alber.ipia/Documents/bdc_amazonia/roraima/scripts/util.R")

points_file <- "./data/samples/points_tb.rds"
stopifnot(file.exists(points_file))

#---- Read data ----

points_tb <- points_file %>%
    readRDS()

#----- Pre-process samples ----

# Build an sf object with the sample points.
# Remove duplicates using coordintes as ID.
samples_sf <- points_tb %>%
     id_from_coords() %>%
     (function(x){
         id_mean <- mean(table(x$id_coords))
         if(id_mean != 1)
             warning("Duplicated points found! Removing duplicates....")
         return(x)
     }) %>%
     dplyr::distinct(id_coords,label,
                     .keep_all = TRUE) %>%
     ensurer::ensure_that(mean(table(.$id_coords)) == 1,
                          err_desc = "Found duplicated points with different \
                          labels!") %>%
     dplyr::select(-id_coords) %>%
     sf::st_as_sf(coords = c("longitude", "latitude"),
                  crs = 4326)

# Build a sits cube
stack_cube <- sits::sits_cube(type        = "BDC_TILE",
                              name        = "roraima",
                              satellite   = "SENTINEL-2",
                              sensor      = "MSI",
                              cube        = "S2_10_16D_STK",
                              tiles       = "079082",
                              version     = "v001",
                              data_access = "local",
                              bands       = c("B01", "B02", "B03", "B04", "B05",
                                              "B06",  "B08", "B8A", "B07",
                                              "B11", "B12",
                                              #"EVI", "NDVI",
                                              "FMASK"
                              ),
                              .cloud_band = TRUE,
                              start_date  = as.Date("2018-08-01"),
                              end_date    = as.Date("2019-07-31"))

shp_file <- tempfile(pattern = "samples_",
                     fileext = ".shp")
samples_sf %>%
    sf::write_sf(dsn = shp_file)

#---- Get the samples' time series
samples_ts <- sits::sits_get_data(cube = stack_cube,
                                  file = shp_file)

#---- Save to file ----

samples_tb <- samples_sf %>%
    add_coords() %>%
    sf::st_drop_geometry() %>%
    id_from_coords() %>%
    dplyr::select(id_coords, label) %>%
    dplyr::rename(expert_label = label)

samples_tb2 <- samples_ts %>%
    id_from_coords() %>%
    dplyr::left_join(samples_tb, by = "id_coords") %>%
    dplyr::mutate(label = expert_label) %>%
    dplyr::select(-expert_label, -id_coords) %>%
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
    saveRDS(file = "./data/samples/samples_ts.rds")
