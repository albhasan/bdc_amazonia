# Produce sample points from sample polygons.

library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stringr)
library(tidyr)
library(purrr)

source("./roraima/scripts/00_util.R")



#---- Set up ----

class_file   <- "./roraima/results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v010_2a/S2_10_16D_STK_079082_probs_class_2018_8_2019_7_v1.tif"
entropy_file <- "./roraima/results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v010_2a/S2_10_16D_STK_079082_probs_2018_8_2019_7_entropy_v1.tif"
label_file   <- "./roraima/results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v010_2a/sits_labels.csv"



#---- Deforestation samples ----

class_r   <- raster::raster(class_file)
entropy_r <- raster::raster(entropy_file)
label_vec <- label_file %>%
    readr::read_csv() %>%
    magrittr::extract2("label") %>%
    magrittr::set_names(1:length(.))


TODO: do we select samples with most or fewest entropy????????


#----------

oracle_samples <- class_file %>%
    raster::raster() %>%
    raster::sampleStratified(size = 400,
                             xy = TRUE,
                             sp = TRUE) %>%
    (function(y) {
        return(raster::extract(x = raster::raster(entropy_file),
                               y = y, sp = TRUE))
    }) %>%
    sf::st_as_sf() %>%
    dplyr::rename_with(.fn = function(x){return(c("cell", "x", "y",
                                                  "label_pred", "entropy",
                                                  "geometry"))})
oracle_samples <- oracle_samples %>%
    dplyr::group_by(label_pred) %>%
    dplyr::arrange(entropy,
                   .by_group = TRUE) %>%
    dplyr::slice_tail(prop = 0.10) %>%
    dplyr::slice_head(prop = 0.5) %>%
    dplyr::sample_n(10) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cell, -x, -y) %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(label_pred = dplyr::recode(label_pred, !!!label_vec))

oracle_samples %>%
    dplyr::mutate(label = NA_character_,
                  iteration = NA_integer_) %>%
    sf::write_sf("./roraima/data/samples/samples_for_oracle.shp")




#------------------------------------------------------------------------------
prob_file    <- "./roraima/results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v010_2a/S2_10_16D_STK_079082_probs_2018_8_2019_7_v1.tif"
prob_s    <- raster::stack(prob_file)
# Invert the parameter order of raster::extract
extract_inv <- function(obj_sp, obj_r){
    return(raster::extract(obj_r, obj_sp, sp = TRUE))
}
# Compute the variance of the given columns in x
my_var <- function(x, col_names){
    x %>%
        dplyr::mutate(variance = j)

}

samples_sf <- raster::sampleStratified(class_r,
                                       size  = 400,
                                       sp = TRUE) %>%
    extract_inv(obj_r = entropy_r) %>%
    extract_inv(obj_r = prob_s) %>%
    magrittr::set_names(c("cell", "pred_label", "entropy",
                          paste0("p_", label_vec))) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(pred_label = dplyr::recode(pred_label, !!!label_vec))

plot_tb <- samples_sf %>%
    sf::st_set_geometry(NULL) %>%
    tidyr::nest(probs = tidyselect::starts_with("p_")) %>%
    dplyr::mutate(p_var = purrr::map_dbl(probs, function(x){
        return(var(unlist(x)))
    })) %>%
    dplyr::mutate(pred_label = as.factor(pred_label)) %>%
    dplyr::select(-probs)

plot_tb %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = p_var,
                                     y = entropy,
                                     color = pred_label)) +
    # ggplot2::geom_hex(ggplot2::aes(x = p_var,
    #                                y = entropy))
    ggplot2::facet_wrap(vars(pred_label))

samples_sf %>%
    dplyr::group_by(pred_label) %>%
    dplyr::arrange(entropy) %>%
    dplyr::slice_tail(prop = 0.1) %>%
    dplyr::slice_head(prop = 0.5) %>%
    (function(x){
        # print the number of samples
        x %>%
            sf::st_set_geometry(NULL) %>%
            dplyr::count() %>%
            print(n = Inf)
        invisible(x)
    })



data_tb <- samples_sf %>%
    sf::st_set_geometry(NULL) %>%
    tidyr::nest(probs = tidyselect::starts_with("p_")) %>%
    tidyr::unnest()
