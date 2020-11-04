# Produce sample points from sample polygons.

library(dplyr)
library(lubridate)
library(sf)

source("./roraima/scripts/util.R")

#---- Set up ----

mapbiomas_file <- "./roraima/data/vector/mapbiomas_alerta/dashboard_alerts-shapefile.shp"
prodes_file    <- "./roraima/data/vector/prodes/PDigital2000_2019_AMZ.shp"
stopifnot(all(file.exists(mapbiomas_file, prodes_file)))

# Length of the diagonal of a pixel.
pixel_diag_mts <- 10 * sqrt(2)

# Time interval of interest.
time_interval <- lubridate::interval(lubridate::ymd("2018-08-01"),
                                     lubridate::ymd("2019-07-31"))

# UTM zone of the Roraima cube.
projected_crs <- 32620


#---- Util ----

#' Check if two geometries intersect.
#'
#' @param x A sf object of POINT geometry type.
#' @param y A sf object of POLYGON geometry type.
#' @return  A logical of the same length of x.
f_intersect <- function(x, y){
    sf::st_intersects(x, y, sparse = FALSE) %>%
        apply(1, any) %>%
        return()
}

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

# NOTE: This code throws a warning because exploding polygons duplicates
#       attributes.
deforestation_polygons <- mapbiomas_sf %>%
    dplyr::mutate(DataDetec = lubridate::as_date(DataDetec)) %>%
    dplyr::filter(DataDetec %within% time_interval) %>%
    # Remove small polygons.
    # Avoid sampling borders.
    sf::st_transform(crs = projected_crs) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_buffer(dist = -1 * pixel_diag_mts) %>%
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


#---- Forest & non-forest samples ----

prodes_sf <- prodes_file %>%
    sf::read_sf()

prodes_polygons <- prodes_sf %>%
    dplyr::mutate(label = as.character(DN)) %>%
    dplyr::mutate(label = dplyr::recode(label,
                                        `1`  = "FLORESTA",
                                        `2`  = "HIDROGRAFIA",
                                        `3`  = "NAO_FLORESTA",
                                        `4`  = "NAO_FLORESTA2",
                                        `5`  = "NUVEM",
                                        `6`  = "d2007",
                                        `7`  = "d2008",
                                        `8`  = "d2009",
                                        `9`  = "d2010",
                                        `10` = "d2011",
                                        `11` = "d2012",
                                        `12` = "d2013",
                                        `13` = "d2014",
                                        `14` = "d2015",
                                        `15` = "d2016",
                                        `16` = "d2017",
                                        `17` = "d2018",
                                        `18` = "r2010",
                                        `19` = "r2011",
                                        `20` = "r2012",
                                        `21` = "r2013",
                                        `22` = "r2014",
                                        `23` = "r2015",
                                        `24` = "r2016",
                                        `25` = "r2017",
                                        `26` = "r2018",
                                        `27` = "d2019",
                                        `28` = "r2019")) %>%
    # NOTE: This line throws a warning because of labels without year,
    #       producing NAs which are then replaced with 2019.
    dplyr::mutate(event_year = as.integer(stringr::str_sub(label, start = 2)),
                  event_year = tidyr::replace_na(event_year, 2019)) %>%
    dplyr::select(-DN)

# Samples deforestation from former years and other classes to Other.
forest_other_polygons <- prodes_polygons %>%
    dplyr::filter(label %in% c("FLORESTA", "NAO_FLORESTA", "NAO_FLORESTA2",
                               "d2007", "d2008", "d2009", "d2010", "d2011",
                               "d2012", "d2013", "d2014", "d2015", "d2016",
                               "d2017", "d2018", "r2010", "r2011", "r2012",
                               "r2013", "r2014", "r2015", "r2016", "r2017",
                               "r2018")) %>%
    dplyr::mutate(label = dplyr::recode(label,
                                        "FLORESTA" = "Forest",
                                        .default   = "Other")) %>%
    ensurer::ensure_that(all(unique(.$label) %in% c("Forest", "Other")),
                         err_desc = "Unexpected labels found!") %>%
    # Avoid sampling borders.
    sf::st_transform(crs = projected_crs) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_buffer(dist = -1 * pixel_diag_mts) %>%
    # Remove small polygons.
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>%
    # 0.25 Ha ~ 5x5 10mts pixels
    dplyr::filter(area_ha > 0.25)

# TODO:
# - Remove MAPBIOMAS deforestation 2019 polygons
# NOTE: Alternatively, sample and discount samples inside
set.seed(234)
forest_other_samples <- forest_other_polygons %>%
    dplyr::mutate(n_samples = case_when(label == "Forest" ~ 2,
                                        label == "Other"  ~ 1)) %>%
    sf::st_sample(size = .$n_samples) %>%
    remove_close_points(threshold = pixel_diag_mts) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(in_deforestation = f_intersect(x = .,
                                                 y = sf::st_buffer(deforestation_polygons,
                                                                   # NOTE: Positive buffer.
                                                                   dist = 2 * pixel_diag_mts))) %>%
    dplyr::filter(in_deforestation == FALSE) %>%
    forest_other_samples %>%
    dplyr::select(-in_deforestation) %>%
    sf::st_join(forest_other_polygons) %>%
    sf::st_transform(crs = 4326) %>%
    add_coords() %>%
    tibble::as_tibble() %>%
    dplyr::select(latitude, longitude, label)

# Join samples.
points_tb <- forest_other_samples %>%
    dplyr::bind_rows(deforestation_samples) %>%
    dplyr::mutate(start_date = lubridate::as_date("2018-08-01"),
                  end_date   = lubridate::as_date("2019-07-31"),
                  cube = NA,
                  time_series = NA) %>%
    dplyr::select(longitude, latitude, start_date,
                  end_date, label, cube, time_series) %>%
    ensurer::ensure_that(!any(is.na(.$longitude),
                              is.na(.$latitude),
                              is.na(.$label),
                              is.na(.$start_date),
                              is.na(.$end_date)),
                         err_desc = "Missing values found!")

deforestation_n <- points_tb %>%
    dplyr::filter(label == "Deforestation") %>%
    nrow()

set.seed(345)
points_tb <- points_tb %>%
    dplyr::group_by(label) %>%
    dplyr::sample_n(deforestation_n) %>%
    dplyr::ungroup()

points_tb %>%
    count(label)

points_tb %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326) %>%
    sf::write_sf("./roraima/data/samples/points_tb.shp")

points_tb %>%
    saveRDS("./roraima/data/samples/points_tb.rds")
