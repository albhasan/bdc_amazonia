library(dplyr)
library(ensurer)
library(ggplot2)
library(purrr)
library(sf)
library(stringr)
library(tidyr)


source("./scripts/00_util.R")


samples_file <- "./data/samples/samples_tb.rds"
stopifnot(file.exists(samples_file))

samples_tb <- samples_file %>%
    readRDS()

samples_tb %>%
    is_sits_valid()

samples_tb <- samples_tb %>%
    tidyr::unnest(time_series) %>%
    dplyr::select(-longitude, -latitude, -cube,
                  Label = label) %>%
    dplyr::select(order(colnames(.))) %>%
    dplyr::select(Label, start_date, end_date, Index, everything())

my_date <- samples_tb %>%
    dplyr::select(start_date, end_date) %>%
    unlist() %>%
    lubridate::as_date() %>%
    range() %>%
    paste(collapse = "_")



#---- Plot samples' time series ----



f_plot <- function(x){
    x %>%
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = Index,
                                           y = Value,
                                           group = interaction(Index, Band)),
                              outlier.size = 0.5) +
        ggplot2::geom_smooth(ggplot2::aes(x = Index,
                                          y = Value,
                                          group =  Band,
                                          color = Label)) +
        ggplot2::theme(axis.text.x = element_text(angle = 90),
                       strip.text.y = element_blank()) +
        ggplot2::facet_grid(rows = vars(Label),
                            cols = vars(Band)) %>%
        return()
}


plot_tb <- samples_tb %>%
    tidyr::pivot_longer(cols = !tidyselect::all_of(c("start_date",
                                                     "end_date",
                                                     "Label", "Index")),
                        names_to = "Band",
                        values_to = "Value")

plot_tb %>%
    dplyr::filter(Band %in% c("B02", "B03", "B04")) %>%
    f_plot() +
    ggplot2::ggtitle("L8 samples - Flat bands") +
    ggplot2::ggsave(filename = paste0("./data/samples/plot_samples_bands_flat_",
                                      my_date, ".png"),
                    width = 297,
                    height = 420,
                    units = "mm")

plot_tb %>%
    dplyr::filter(Band %in% c("B08", "B8A", "B11", "B12")) %>%
    f_plot() +
    ggplot2::ggtitle("L8 samples - Bands") +
    ggplot2::ggsave(filename = paste0("./data/samples/plot_samples_bands_",
                                      my_date, ".png"),
                    width = 297,
                    height = 420,
                    units = "mm")

plot_tb %>%
    dplyr::filter(Band %in% c("EVI", "NDVI")) %>%
    f_plot() +
    ggplot2::ggtitle("L8 samples - Vegetation Indexes") +
    ggplot2::ggsave(filename = paste0("./data/samples/plot_samples_indices_",
                                      my_date, ".png"),
                    width = 297,
                    height = 420,
                    units = "mm")
