#
library(dplyr)
library(sf)

probability_file <- "./results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v003/roraima_probs_2018_8_2019_7_v1.tif"
bayesian_file    <- "./results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v003/roraima_probs_class_bayesian_2018_8_2019_7_v1.tif"
prodes_file      <- "./data/vector/yearly_deforestation.shp"
stopifnot(all(file.exists(probability_file, bayesian_file)))

# Mask
prodes_sf <- prodes_file %>%
    sf::read_sf()
