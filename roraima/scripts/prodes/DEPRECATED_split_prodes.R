# Split a PRODES shapefile.

stop("DEPRECATED: Use the vectorize_prodes.sh")

library(dplyr)
library(sf)
library(terra)

prodes_dir <- "~/Documents/data/vector/prodes"
raster_file <- "./results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v003/roraima_probs_class_bayesian_2018_8_2019_7_v1.tif"
stopifnot(dir.exists(prodes_dir))

path_row <- c("23159", "23258", "23259", "23358")
out_dir <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/vector/prodes"

# Table of PRODES shapefiles.
prodes_tb <- prodes_dir %>%
    list.files(pattern = "*.shp", full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(type = tools::file_path_sans_ext(basename(file_path)),
                  out_file = file.path(out_dir, paste0(type, ".shp")))

# Get PRODES' reference system.
prodes_crs <- prodes_tb %>%
    dplyr::filter(type == "hydrography_biome") %>%
    dplyr::pull(file_path) %>%
    sf::read_sf() %>%
    as("Spatial")

# Get the extent of a reference image and project to PRODES' crs.
my_raster <- raster_file %>%
    raster::raster()
crop_extent <- my_raster %>%
    raster::extent() %>%
    as("SpatialPolygons") %>%
    sf::st_as_sf()
sf::st_crs(crop_extent) <- raster::crs(my_raster)

#' Helper for cropping PRODES data.
#'
#' @param file_path   A lenght-one character. Path to a PRODES shapefile.
#' @param path_row    A lenght-one character. A path row for filtering PRODES' polygons.
#' @param crop_extent An extent objext used to intersect PRODES' polygons.
#' @param out_file    A lenght-one character. A path to a file.
#' @return The path to the input shp file.
crop_prodes <- function(file_path, out_file, path_row, crop_extent){
    obj_sf <- file_path %>%
        sf::read_sf() %>%
        dplyr::filter(PATH_ROW %in% path_row)
    my_ext <- crop_extent %>%
        sf::st_transform(sf::st_crs(obj_sf))
    obj_sf %>%
        sf::st_intersection(my_ext) %>%
        #ensurer::ensure_that(nrow(.) > 0, err_desc = "No features found!") %>%
        sf::write_sf(out_file)
    invisible(file_path)
}

# Crop and save to disc.
prodes_tb %>%
    dplyr::select(file_path, out_file) %>%
    dplyr::mutate(obj_sf = purrr::map2(file_path, out_file, crop_prodes,
                                       path_row = path_row,
                                       crop_extent = crop_extent))
