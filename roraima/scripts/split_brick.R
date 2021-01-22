# Split brick into mini brick for testing models.
library(dply)
library(lubridate)

brick_dir <- "/http/s2/S2_10_16D_STK/v001/079082"
stopifnot(dir.exists(brick_dir))

time_interval <- lubridate::interval(start = lubridate::as_date("2018-08-01"),
                                     end   = lubridate::as_date("2019-07-31"))



#---- Util ----

# Cut images.
crop_images <- function(in_file, out_file, xmin, xmax, ymin, ymax){
    string <- "gdalwarp -te $xmin $ymin $xmax $ymax -co 'COMPRESS=LZW' -co 'BIGTIFF=YES' $in_file $out_file"
    replace_tb <- tibble::tibble(pattern = paste0("$", c("xmin", "xmax", "ymin",
                                                         "ymax", "in_file",
                                                         "out_file")),
                                 replacement = as.character(c(xmin, xmax, ymin,
                                                              ymax, in_file,
                                                              out_file)))
    for (i in seq_along(replace_tb$pattern)) {
        string <- stringr::str_replace(string,
                                       pattern = stringr::fixed(replace_tb$pattern[[i]]),
                                       replacement = replace_tb$replacement[[i]])
    }
    system(string)
    invisible(out_file)
}



#---- Script -----

files_vec <- brick_dir %>%
    list.files(pattern = "*.tif",
               recursive = TRUE,
               full.names = TRUE)

images_tb <- files_vec %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(file_path))) %>%
    tidyr::separate(file_name,
                    into = c("satellite", "spatial_res", "time_res", "type",
                             "version", "tile", "start_date", "end_date",
                             "band"),
                    sep = "_") %>%
    dplyr::mutate(start_date = lubridate::as_date(start_date),
                  end_date   = lubridate::as_date(end_date)) %>%
    dplyr::filter(start_date %within% time_interval)

xmin <- 4236870
ymin <- 11549900
xmax <- xmin + 10000
ymax <- ymin + 10000
mini_images <- images_tb %>%
    dplyr::select(in_file = file_path) %>%
    dplyr::mutate(out_file = stringr::str_replace(in_file,
                                                  pattern = stringr::fixed("/http/s2/S2_10_16D_STK/v001/079082"),
                                                  replacement =            "/http/s2/mini/brick_1/S2_10_16D_STK/v001/079082"),
                  out_dir = purrr::map_lgl(dirname(out_file),
                                           dir.create,
                                           recursive = TRUE)) %>%
    dplyr::mutate(cropped_image = purrr::map2_chr(in_file,
                                                  out_file,
                                                  crop_images,
                                                  xmin = xmin,
                                                  xmax = xmax,
                                                  ymin = ymin,
                                                  ymax = ymax))

xmin <- 4161976
ymin <- 11597629
xmax <- xmin + 10000
ymax <- ymin + 10000
mini_images <- images_tb %>%
    dplyr::select(in_file = file_path) %>%
    dplyr::mutate(out_file = stringr::str_replace(in_file,
                                                  pattern = stringr::fixed("/http/s2/S2_10_16D_STK/v001/079082"),
                                                  replacement =            "/http/s2/mini/brick_2/S2_10_16D_STK/v001/079082"),
                  out_dir = purrr::map_lgl(dirname(out_file),
                                           dir.create,
                                           recursive = TRUE)) %>%
    dplyr::mutate(cropped_image = purrr::map2_chr(in_file,
                                                  out_file,
                                                  crop_images,
                                                  xmin = xmin,
                                                  xmax = xmax,
                                                  ymin = ymin,
                                                  ymax = ymax))

xmin <-  4258000
ymin <- 11536300
xmax <- xmin + 10000
ymax <- ymin + 10000
mini_images <- images_tb %>%
    dplyr::select(in_file = file_path) %>%
    dplyr::mutate(out_file = stringr::str_replace(in_file,
                                                  pattern = stringr::fixed("/http/s2/S2_10_16D_STK/v001/079082"),
                                                  replacement =            "/http/s2/mini/brick_3/S2_10_16D_STK/v001/079082"),
                  out_dir = purrr::map_lgl(dirname(out_file),
                                           dir.create,
                                           recursive = TRUE)) %>%
    dplyr::mutate(cropped_image = purrr::map2_chr(in_file,
                                                  out_file,
                                                  crop_images,
                                                  xmin = xmin,
                                                  xmax = xmax,
                                                  ymin = ymin,
                                                  ymax = ymax))
