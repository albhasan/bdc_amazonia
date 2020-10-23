# Produce sample points from sample polygons.

library(dplyr)
library(lubridate)
library(sf)

#---- Set up ----

mapbiomas_file <- "./data/vector/mapbiomas_alerta/dashboard_alerts-shapefile.shp"
prodes_forest_file    <- "./data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif_forest.tif"
stopifnot(all(file.exists(mapbiomas_file, prodes_forest_file)))

# Lenght of the diagonal of a pixel.
pixel_diag_mts <- 10 * sqrt(2)

# Time interval of interest.
time_interval <- lubridate::interval(lubridate::ymd("2018-08-01"),
                                     lubridate::ymd("2019-07-31"))

# Coordinate system used to compute areas and distances. UTM zone of the cube.
projected_crs <- 32620

stopifnot(all(file.exists(mapbiomas_file, prodes_forest_file)))

#---- Util ----

#' Remove points which are too close to one another.
#'
#' @param sf_obj    A sf object of POINT geometry type.
#' @param threshold A length-one numeric. The minimum distance between points.
#' @return          A sf object.
remove_close_points <- function(sf_obj, threshold){
    stopifnot(threshold > 0)
    g_type <- sf_obj %>%
        sf::st_geometry_type() %>%
        unique() %>%
        as.character() %>%
        ensurer::ensure_that(. == "POINT",
                             err_desc = "Points expected!")

    invalid_points <- sf_obj %>%
        sf::st_distance() %>%
        units::set_units(NULL) %>%
        magrittr::is_less_than(threshold) %>%
        colSums() %>%
        magrittr::equals(1) %>%
        magrittr::not() %>%
        (function(x){
            which(x %in% TRUE)
        })

    return(sf_obj[-invalid_points])
}

#---- Deforestation samples ----

mapbiomas_sf <- mapbiomas_file %>%
    sf::read_sf()

# NOTE: Mapbiomas alerta data starts at January 2019!
mapbiomas_sf %>%
    dplyr::pull(DataDetec) %>%
    range()

deforestation_polygons <- mapbiomas_sf %>%
    dplyr::mutate(DataDetec = lubridate::as_date(DataDetec)) %>%
    dplyr::filter(DataDetec %within% time_interval) %>%
    # Avoid sampling borders.
    sf::st_transform(crs = projected_crs) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_buffer(dist = -1 * pixel_diag_mts) %>%
    # Remove small polygons.
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>%
    # 0.25 Ha ~ 5x5 10mts pixels
    dplyr::filter(area_ha > 0.25)

# Get sample points in the polygons.
set.seed(123)
deforestation_samples <- deforestation_polygons %>%
    dplyr::mutate(n_sample = 4) %>%
    sf::st_sample(size = .$n_sample) %>%
    remove_close_points(threshold = pixel_diag_mts) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::rename(longitude = "X", latitude = "Y") %>%
    dplyr::mutate(label = "Deforestation")

#---- Forest samples ----

forest_r <- prodes_forest_file %>%
    raster::raster()

set.seed(234)
forest_samples <-  raster::sampleRandom(forest_r,
                                        size = nrow(deforestation_samples),
                                        sp = TRUE) %>%
    sf::st_as_sf() %>%
    remove_close_points(threshold = pixel_diag_mts) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::rename(longitude = "X", latitude = "Y") %>%
    dplyr::mutate(label = "Forest")

deforestation_samples %>%
    dplyr::bind_rows(forest_samples) %>%
    dplyr::mutate(start_date = lubridate::as_date("2018-08-01"),
                  end_date   = lubridate::as_date("2019-07-31"),
                  cube = NA,
                  time_series = NA) %>%
    dplyr::select(longitude, latitude, start_date,
                  end_date, label, cube, time_series) %>%
    saveRDS("./data/samples/points_tb.rds")


#---- Non-Forest samples ----

