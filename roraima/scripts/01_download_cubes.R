# Download images from the Brazil Data Cubes website.

library(dplyr)
library(readr)
library(sits)
library(tidyr)



#---- Setup ----

source("~/Documents/bdc_access_key.R")
access_key <- Sys.getenv("BDC_ACCESS_KEY")
stopifnot(access_key != "")



#---- Script ----

image_tb <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/scripts/S2_10_16D_STK-1_079082.txt" %>%
    readr::read_delim(delim = " ",
                      col_names = FALSE,
                      col_types = "c") %>%
    magrittr::set_names("img_url") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(img_url))) %>%
    tidyr::separate(file_name, sep = "_",
                    into = c("mission", "sp_resolution", "time_resolution",
                             "type", "version", "tile", "start_date",
                             "end_date", "band")) %>%
    dplyr::filter(band %in% c("band1", "band11", "band12", "band2", "band3",
                              "band4", "band5", "band6", "band7", "band8",
                              "band8a", "EVI", "Fmask4", "NDVI")) %>%
    dplyr::mutate(url = stringr::str_c(img_url, "?access_token=",
                                           access_key),
                  destfile = stringr::str_c(
"/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/S2_10_16D_STK/v001/",
                                               tile, "/", basename(img_url)))
image_tb <- image_tb %>%
    dplyr::mutate(downloaded = purrr::map2_int(url, destfile, download.file,
                                         method = "auto", quiet = TRUE))

image_tb %>%
        tidyr::nest(bands = c(band, img_url, url, destfile, dowloaded)) %>%
    dplyr::mutate(nfiles = purrr::map_int(bands, nrow)) %>%
    ensurer::ensure_that(length(unique(.$nfiles)) == 1,
                         err_desc = "Missmatch in the number of files per image.") %>%
    ensurer::ensure_that(nrow(.) == 24,
                         err_desc = "Unexpected number of images")
