stop("Adapt from Rondonia to Raoraima")
# Copy a BDC data cube to a local disc.

library(dplyr)
library(lubridate)

cube_dir <- "~/Documents/bdc_amazonia/roraima/data/raster/S2_10_16D_STK/v001/079082"
out_dir  <- "/http/s2"

time_interval <- lubridate::interval(lubridate::ymd("2018-08-01"),
                                     lubridate::ymd("2019-07-31"))

#---- Make a local copy of the BDC files ----

image_tb <- cube_dir %>%
    list.files(pattern = "*.tif$",
               recursive = TRUE,
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(file_path))) %>%
    tidyr::separate(file_name, into = c("satellite", "res_meters", "res_days",
                                        "type", "version", "tile", "start",
                                        "end", "band"),
                    sep = "_") %>%
    dplyr::mutate(start = lubridate::as_date(start),
                  end   = lubridate::as_date(end)) %>%
    dplyr::filter(start %within% time_interval,
                  band %in% c("band2", "band3", "band4", "band8",
                              "band8a", "band11", "band12", "Fmask4")) %>%
    dplyr::arrange(start)

image_tb %>%
    dplyr::pull(file_path) %>%
        file.copy(to = out_dir)
