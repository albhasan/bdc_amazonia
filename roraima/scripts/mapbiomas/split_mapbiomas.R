# Split MAPBIOMAS alerta shapefile.

library(dplyr)
library(sf)

mapbiomas_file <- "~/Documents/data/vector/mapbiomas_alerta/dashboard_alerts-shapefile.shp"
raster_file <- "./results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v003/roraima_probs_class_bayesian_2018_8_2019_7_v1.tif"
stopifnot(all(file.exists(mapbiomas_file, raster_file)))

out_file <- "./data/vector/mapbiomas_alerta/dashboard_alerts-shapefile.shp"

# Get the extent of a reference image and project to PRODES' crs.
my_raster <- raster_file %>%
    raster::raster()
crop_extent <- my_raster %>%
    raster::extent() %>%
    as("SpatialPolygons") %>%
    sf::st_as_sf()
sf::st_crs(crop_extent) <- raster::crs(my_raster)

mapbiomas_sf <- mapbiomas_file %>%
    sf::read_sf()

my_ext <- crop_extent %>%
    sf::st_transform(sf::st_crs(mapbiomas_sf))

mapbiomas_sf %>%
    dplyr::filter(Estado == "RORAIMA") %>%
    # Fix self-intersection geometries
    sf::st_buffer(dist = 0) %>%
    sf::st_intersection(my_ext) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = "No features found!") %>%
    sf::write_sf(out_file)