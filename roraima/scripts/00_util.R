#' Add coordinates as columns to an SF object.
#'
#' @param point_sf A sf object.
#' @return         A sf object.
add_coords <- function(point_sf){
    xy <- point_sf %>%
        sf::st_coordinates() %>%
        magrittr::set_colnames(c("longitude", "latitude")) %>%
        tidyr::as_tibble()
    point_sf %>%
        dplyr::bind_cols(xy) %>%
        return()
}

#' Remove invalid samples of time series.
#'
#' @param  sits_tb A sits_tibble.
#' @report report  When TRUE, not cleaning is done, just marking the offending samples.
#' @return A sits_tibble.
clean_ts <- function(sits_tb, report = FALSE){
    sits_tb %>%
        tidyr::drop_na() %>%
        dplyr::mutate(has_na    = purrr::map_int(time_series, function(x){return(sum(is.na(x)))}),
                      has_null  = purrr::map_int(time_series, function(x){return(sum(is.null(x), na.rm = TRUE))}),
                      has_overflow  = purrr::map_int(time_series, function(x){return(sum(sum(as.matrix(x[,2:ncol(x)]) < -1, na.rm = TRUE), sum(as.matrix(x[,2:ncol(x)]) > 1, na.rm = TRUE)))}),
                      time_mean = purrr::map_dbl(time_series, function(x){return(mean(x[[1]]))}),
                      n_cols    = purrr::map_int(time_series, ncol),
                      n_rows    = purrr::map_int(time_series, nrow)) %>%
        (function(.data){
            if (report){
                return(.data)
            }else{
                .data <- .data %>%
                    tidyr::drop_na() %>%
                    dplyr::filter(!has_null,
                                  n_cols > 1,
                                  n_rows > 0) %>%
                    dplyr::mutate(time_series = purrr::map(time_series, function(x){
                        my_approx <- function(v) {
                            apply(v, 2,
                                  function(x) {
                                      i <- tryCatch({
                                          approx(x, n = length(x))
                                      }, error = function(e) list(y = rep(0, length(x))))
                                      return(i$y)
                                  })
                        }
                        data_mt <- as.matrix(x[,2:ncol(x)])
                        data_mt[data_mt <= -1] <- NA
                        data_mt[data_mt >= 1]  <- NA
                        interp_mt <- my_approx(data_mt)
                        x %>%
                            dplyr::select(Index) %>%
                            dplyr::bind_cols(tibble::as_tibble(interp_mt)) %>%
                            return()
                    })) %>%
                    dplyr::select(-has_na, -has_null, -time_mean,
                                  -overflow, -n_cols, -n_rows)
                n_removed <- nrow(sits_tb) - nrow(.data)
                if (n_removed > 0)
                    warning(sprintf("Removed %s invalid samples out of  %s",
                                    n_removed, nrow(sits_tb)))
                return(.data)
            }
        }) %>%
        return()
}

#' Compute the information entropy in nats.
#'
#' @param img_path A length-one character. Path to a sits probability file.
#' @param out_file A length-one character. Path to the file to store the results.
#' @return         out_file.
compute_entropy <- function(img_path, out_file){
    # 2021-01-06
    n_bands <- img_path %>%
        ensurer::ensure_that(file.exists(.)) %>%
        gdalUtils::gdalinfo() %>%
        stringr::str_extract(pattern = "Band [0-9]+") %>%
        .[!is.na(.)] %>%
        dplyr::last() %>%
        stringr::str_extract(pattern = "[0-9]+") %>%
        as.numeric() %>%
        ensurer::ensure_that(is.numeric(.), . > 1,
                             err_desc = "Invalid number of bands!")
    exp_bands <- paste(sprintf("-%s %s --%s_band=%s", LETTERS[1:n_bands],
                               img_path, LETTERS[1:n_bands], 1:n_bands),
                       collapse = " ")
    exp_gdal <- paste0( "'(",
                        paste(
                            sprintf("%s.astype(numpy.float64)/10000 * numpy.log(%s.astype(numpy.float64)/10000)",
                                    LETTERS[1:n_bands],
                                    LETTERS[1:n_bands]),
                            collapse = " + "
                        ),
                        ") * -1'")
    cmd <- sprintf("gdal_calc.py %s --outfile=%s --calc=%s --NoDataValue=-9999 --type='Float64' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'",
                   exp_bands, out_file, exp_gdal)
    res <- system(cmd)
    invisible(out_file)
}

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


#' Return the PRODES year of the given date
#'
#' @param x            A Date object.
#' @pararm start_month A length-one integer. The first month of the PRODES year.
#' @return             An integer, the PRODES year.
get_prodes_year <- function(x, start_month = 8){
    if(!lubridate::is.Date(x))
        return(NA)
    if(length(x) == 1){
        m <- lubridate::month(x)
        if(m < start_month)
            return(as.integer(lubridate::year(x)))
        return(as.integer(lubridate::year(x) + 1))
    }else if(length(x) > 1){
        vapply(x, get_prodes_year, integer(1))
    }
}

#' Add an ID column to the given tibble. The column id name from the variables
#' longitude and latitude.
#'
#' @param x     A tibble.
#' @param col   An object. The name of the new column.
#' @param n_dec A length-one integer. The number of decimals to use.
#' @return      A tibble with the additional column id_coords.
id_from_coords <- function(x, col, n_dec = 6){
    x %>%
        ensurer::ensure_that(all(c("longitude", "latitude") %in% colnames(.)),
                             err_desc = "Missing columns: longitude and latitude") %>%
        dplyr::mutate(x_str = as.character(round(longitude, digits = n_dec)),
                      y_str = as.character(round(latitude, digits = n_dec)),
                      {{col}} := stringr::str_c(x_str, y_str, sep = "_")) %>%
        dplyr::select(-x_str, -y_str) %>%
        return()
}


#' Test if the data in a sits_tibble is valid.
#'
#' @param x A sits tibble.
#' @return  The given sits_tibble or error.
is_sits_valid <- function(x){
    .has_names <- function(y){
        sapply(y, function(z){
            !is.null(names(z))
        })
    }
    res <- x %>%
        dplyr::mutate(n_rows = purrr::map_int(time_series, nrow),
                      n_cols = purrr::map_int(time_series, ncol),
                      n_na   = purrr::map_int(time_series, function(x){
                          return(sum(is.na(x)))
                      })) %>%
        ensurer::ensure_that(all(.$n_rows > 1),
                             err_desc = "Wrong number of steps in time series!") %>%
        ensurer::ensure_that(length(unique(.$n_rows)) == 1,
                             err_desc = "Number of steps in time series don't match!") %>%
        ensurer::ensure_that(all(.$n_cols > 1),
                             err_desc = "Wrong number of variables in time series!") %>%
        ensurer::ensure_that(length(unique(.$n_cols)) == 1,
                             err_desc = "Number of variables don't match!") %>%
        ensurer::ensure_that(all(.$n_na == 0),
                             err_desc = "NAs found in time series!") %>%
        ensurer::ensure_that(sum(.has_names(.)) == 0,
                             err_desc = "The columns must not have list names internally!") %>%
        ensurer::ensure_that(!("grouped_df" %in% class(.)),
                             err_desc = "Grouped tibbles are not supported in sits!") %>%
        ensurer::ensure_that("sits" %in% class(.),
                             err_desc = "The tibble is not a sits tibble")
    invisible(x)
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

# Return a cube
get_cube <- function(cube){
    cube_path <- tibble::tribble(
        ~name, ~dir_path,
        "cube",   "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/S2_10_16D_STK/v001/079082",
        "mini_1", "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/mini/brick_1/S2_10_16D_STK/v001/079082",
        "mini_2", "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/mini/brick_2/S2_10_16D_STK/v001/079082",
        "mini_3", "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/mini/brick_3/S2_10_16D_STK/v001/079082"
    ) %>%
        ensurer::ensure_that(all(dir.exists(.$dir_path)),
                             err_desc = "Invalid cube directories!")

    cube_dir <- cube_path %>%
        dplyr::filter(name == cube) %>%
        ensurer::ensure_that(nrow(.) == 1,
                             err_desc = sprintf("Cube not found. Available options are %s",
                                                paste(cube_path$name, collapse = ", "))) %>%
                                 dplyr::pull(dir_path)

    sits::sits_cube(
        type = "STACK",
        name = "S2_10_16D_STK_079082",
        satellite = "SENTINEL-2",
        sensor = "MSI",
        data_dir = cube_dir,
        delim = "_",
        # S2_10_16D_STK_v001_079082_2019-07-28_2019-08-12_band4.tif
        parse_info = c("mission", "sp_res", "tm_res", "type", "version", "tile", "date", "end_date", "band")
    ) %>%
        return()
}
