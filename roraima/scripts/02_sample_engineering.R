# Produce sample points from sample polygons.

library(dplyr)
library(ensurer)
library(ggplot2)
library(purrr)
library(raster)
library(readr)
library(sf)
library(sits)
library(tidyr)

source("/home/alber.ipia/Documents/bdc_amazonia/roraima/scripts/00_util.R")


#---- Set up ----

samples_dir  <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/samples"
prodes_file  <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif_079082.tif"
class_file   <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/results/rf-1000/B02-B03-B04-B08-B11-B12-B8A/v010_2a/S2_10_16D_STK_079082_probs_class_2018_8_2019_7_v1.tif"

# Labels used in the classification raster.
class_labels <- c(`1` = "deforestation",
                  `2` = "forest",
                  `3` = "natNonForest",
                  `4` = "pasture",
                  `5` = "other")

data_cube <- get_cube("cube")

stopifnot(dir.exists(samples_dir))
stopifnot(file.exists(prodes_file))
stopifnot(file.exists(class_file))


#---- Get time series for the original samples ----

csv_file <- tempfile(pattern = "samples_",
                     fileext = ".csv")

# Merge the samples' shapefiles.
samples_tb <- samples_dir %>%
  list.files(pattern = "*[.]shp$",
             full.names = TRUE) %>%
  tibble::as_tibble() %>%
  rename(file_path = value) %>%
  dplyr::mutate(sf_obj = purrr::map(file_path, function(x) {
    x %>%
      sf::read_sf() %>%
      dplyr::select(label) %>%
      return()
  }))
samples_tb

samples_sf <- do.call(rbind, dplyr::pull(samples_tb, sf_obj)) %>%
  add_coords() %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::mutate(start_date = dplyr::first(sits::sits_timeline(data_cube)),
                end_date   = dplyr::last(sits::sits_timeline(data_cube)),
                cube = "",
                time_series = "") %>%
  dplyr::select(longitude, latitude, start_date, end_date,
                label, cube, time_series) %>%
  ensurer::ensure_that(sum(.$label == "") == 0,
                       sum(is.na(.$label)) == 0,
                       all(.$label %in% class_labels),
                       err_des = "Missing labels!") %>%
  readr::write_csv(file = csv_file)
samples_sf %>%
  dplyr::count(label)

samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = csv_file)



#----- Get new samples for the oracle ----

# Randomly select new samples for the oracle.
prodes_r <- raster::raster(prodes_file)

reclass_mat <-  matrix(
c(1,  2, 3, 4,  5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,  # PRODES class
  2, NA, 3, 3, NA, 4, 4, 4, 4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  1,  4), # New label
ncol = 2)

# Recode PRODES to matcht the classification results
reclass_file <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/prodes/2019/PDigital2000_2019_AMZ_gtif_079082_reclass.tif"
prodes_reclass <- raster::reclassify(prodes_r,
                                     rcl = reclass_mat,
                                     filename = reclass_file,
                                     overwrite = TRUE)

# Project PRODES raster to BDC
class_crs <- raster::crs(raster::raster(class_file))
prodes_reclass_proj <- raster::projectRaster(from = prodes_reclass,
                                             crs  = class_crs)

# Get new points from BDC using PRODES strata
points_4_oracle <- raster::sampleStratified(prodes_reclass_proj,
                         size = 120,
                         sp = TRUE)
points_4_oracle <- points_4_oracle %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_as_sf() %>%
  add_coords() %>%
  sf::st_set_geometry(NULL) %>%
  tibble::as_tibble() %>%
  dplyr::select(-cell) %>%
  magrittr::set_colnames(c("label", "longitude", "latitude")) %>%
  dplyr::mutate(start_date = dplyr::first(sits::sits_timeline(data_cube)),
                end_date   = dplyr::last(sits::sits_timeline(data_cube)),
                cube = "",
                time_series = "",
                label = dplyr::recode(label,
                                      !!!class_labels,
                                      .default = NA_character_)) %>%
  dplyr::select(longitude, latitude, start_date,
                end_date, label, cube, time_series) %>%
  dplyr::filter(label != "other")
points_4_oracle %>%
  dplyr::count(label)

points_csv <- tempfile(pattern = "points_",
                       fileext = ".csv")

readr::write_csv(points_4_oracle,
                 file = points_csv)



#---- Get time series for the oracle's samples ----

points_ts <- sits::sits_get_data(cube = data_cube,
                                 file = points_csv)

# Number of time steps in each time series
points_ts %>%
  dplyr::pull(time_series) %>%
  purrr::map_int(nrow) %>%
  unique() %>%
  ensurer::ensure_that(length(.) == 1,
                       err_desc = "Length of time series do not match!")



#---- Process new samples ----

ml_model <- sits::sits_train(samples_ts,
                             ml_method = sits::sits_rfor(trees = 2000))

#res <- lapply(1:nrow(points_ts), function(x, sits_tibble){
points_classified <- lapply(1:nrow(points_ts), function(x, sits_tibble){
  res <- sits::sits_classify(dplyr::slice(sits_tibble, x),
                             ml_model)
  probs <- res$predicted[[1]]$probs[[1]][[1]]
  probs <- as.vector(probs)
  names(probs) <- colnames(res$predicted[[1]]$probs[[1]][[1]])
  res$label_pred <- res$predicted[[1]]$class[[1]]
  res$predicted <- list(bind_rows(probs))
  return(res)
}, sits_tibble = points_ts)
points_classified <-  points_classified %>%
  dplyr::bind_rows() %>%
  tidyr::unnest(predicted)

#---- Compute entropy -----

points_entropy <- points_classified %>%
  tidyr::nest(probs = !tidyselect::one_of("longitude", "latitude", "start_date",
                                          "end_date",   "label",  "cube",
                                          "time_series",  "label_pred")) %>%
  dplyr::mutate(entropy = purrr::map_dbl(probs, function(x){
    return(-1 * sum(unlist(x) * log(unlist(x))))
  }))

# Label PRODES versus Classification
points_tab <- points_entropy %>%
  dplyr::select(label_pred, label) %>%
  table()
# Overall accuracy
acc_overall <- diag(points_tab) / sum(colSums(points_tab))
names(acc_overall) <-  colnames(points_tab)
acc_overall
# Producer accuracy
acc_prod <- diag(points_tab) / colSums(points_tab)
names(acc_prod) <-  colnames(points_tab)
acc_prod
# User accuracy
acc_user <- diag(points_tab) / rowSums(points_tab)
names(acc_user) <-  colnames(points_tab)
acc_user

points_entropy %>%
  dplyr::select(label_pred, label, entropy) %>%
  dplyr::mutate(match = if_else(label == label_pred, TRUE, FALSE)) %>%
  (function(x) {
    x %>%
      dplyr::group_by(match) %>%
      dplyr::summarise(n = n(),
                       entropy_mean = mean(entropy, na.rm = TRUE),
                       entropy_sd = sd(entropy, na.rm = TRUE)) %>%
      print()
    invisible(x)
  }) %>%
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = entropy, fill = match))

# Iteration 0
# match     n entropy_mean entropy_sd
# 1 FALSE 203         1.06       0.181
# 2 TRUE  178        0.856       0.237
#
# Iteration 1
# match     n entropy_mean entropy_sd
# 1 FALSE 196        0.964      0.210
# 2 TRUE  255        0.831      0.222
#
# Iteration 2
# match     n entropy_mean entropy_sd
# 1 FALSE 207        0.918      0.206
# 2 TRUE  242        0.776      0.231
#
# Iteration 3
# match     n entropy_mean entropy_sd
# 1 FALSE 205        0.952      0.205
# 2 TRUE  255        0.827      0.243
#
# Iteration 4
# match     n entropy_mean entropy_sd
# 1 FALSE 183        0.961      0.187
# 2 TRUE  274        0.858      0.200
#
# Iteration 5
# match     n entropy_mean entropy_sd
# 1 FALSE 169        1.01       0.197
# 2 TRUE  283        0.922      0.192
#
# Iteration 6
# match     n entropy_mean entropy_sd
# 1 FALSE 187        0.987      0.212
# 2 TRUE  267        0.870      0.213
#
stop()
# Iteration 7
# match     n entropy_mean entropy_sd
# 1 FALSE 183        0.971      0.209
# 2 TRUE  276        0.929      0.202

#---- Select samples for the oracle ----

points_entropy %>%
  dplyr::select(longitude, latitude, label, label_pred, entropy) %>%
  dplyr::group_by(label) %>%
  dplyr::arrange(dplyr::desc(entropy)) %>%
  dplyr::slice(1:15) %>%
  dplyr::ungroup() %>%
  dplyr::rename(label_prodes = label) %>%
  dplyr::mutate(label = NA,
                start_date = as.character(dplyr::first(sits::sits_timeline(data_cube))),
                end_date   = as.character(dplyr::last(sits::sits_timeline(data_cube))),
                label = "",
                cube = "",
                time_series = "") %>%
  dplyr::select(longitude, latitude, start_date, end_date, label, cube,
                time_series,
                prodes = label_prodes,
                predic = label_pred) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326,
               remove = TRUE) %>%
  #stop("change out file name") %>%
  sf::write_sf("/home/alber.ipia/Documents/bdc_amazonia/roraima/data/samples/samples_07.shp",
               delete_layer = FALSE)
