# Split the samples into train, test, and validation roles.

library(magrittr)
library(dplyr)
library(sf)
library(sits)

source("./roraima/scripts/00_util.R")


#---- Configuration ----

grid_size <- 1024


#---- Util ----

# SOM wrapper.
run_som <- function(my_samples){
    return(
        sits::sits_som_map(my_samples,
                           grid_xdim = 10,
                           grid_ydim = 10,
                           alpha = 1.0,
                           rlen = 100,
                           distance = "euclidean",
                           iterations = 1)
    )
}


#---- Get the samples ----


I cannot select a subset of bands to run the SOM. I need to use them all.

raw_samples_tb <- "./roraima/data/samples/raw_samples_ts.rds" %>%
    readRDS() %>%
    is_sits_valid() %>%
    sits::sits_mutate_bands(NDVI = (B08 - B04)/(B08 + B04)) %>%
    id_from_coords()


#---- Clean the samples ----

raw_samples_tb %>%
    dplyr::count(label)

set.seed(123)
som_cluster <- run_som(raw_samples_tb)

# Print to files.
for (my_type in c("codes", "mapping")) {
    for (b in sits::sits_bands(raw_samples_tb)) {
         png(file = paste0("./roraima/data/samples/som/raw/samples_ts_som_",
                           my_type, "_", b, ".png"))
         plot(som_cluster,
              type = my_type,
              whatmap = match(b, sits::sits_bands(raw_samples_tb)))
         dev.off()
    }
}
cluster_overall <- sits::sits_som_evaluate_cluster(som_cluster)
cluster_overall$confusion_matrix

clean_samples_tb <- som_cluster %>%
    sits::sits_som_clean_samples() %>%
    magrittr::extract2("clean_samples.tb")

clean_samples_tb %>%
    dplyr::count(label)





#----------------------------------
sits_plot_som_map2 <- function(koh, type = "codes", whatmap = 1 , class = NULL)
{
    if (type == "mapping") {
        graphics::plot(koh$som_properties,  bgcol = koh$som_properties$paint_map , "mapping", whatmap = whatmap)
    } else if (type == "codes" ){
        graphics::plot(koh$som_properties,  bgcol = koh$som_properties$paint_map , "codes", whatmap = whatmap, codeRendering = "lines")
    }else if (type == "by_year"){

        data.tb <- dplyr::select(koh$samples_output.tb,id_sample,latitude,longitude,start_date, end_date,label)
        samples_information <- koh$statistics_samples$samples_t
        it <- unique(max(samples_information$iteration))
        samples_information <- dplyr::filter(samples_information, samples_information$iteration == it)
        samples_st_id <- samples_information %>% dplyr::inner_join(data.tb, by = "id_sample")


        if (!is.null(class))
        {
            samples_st_id <- dplyr::filter(samples_st_id, samples_st_id$original_label == class)
        }

        id_all_year <- samples_st_id %>% dplyr::pull(id_neuron)
        id_all_year <- as.numeric(id_all_year)
        graphics::plot(koh$som_properties,  "mapping", classif = id_all_year , bgcol= koh$som_properties$paint, main = "All years" )

        year <- sort(unique(samples_st_id$start_date))
        year <- sort(unique(lubridate::year(samples_st_id$start_date)))
        n_year <- length(year)

        for (i in 1:length(year))
        {
            #samples_by_year <- dplyr::filter(samples_st_id, samples_st_id$start_date == year[i])

            samples_by_year<- dplyr::filter(samples_st_id, lubridate::year(samples_st_id$start_date) == year[i] )
            text_year <- substr(year[i], 1, 4)
            #get the neuron
            id_samples_year <- samples_by_year %>% dplyr::pull(4)
            id_samples_year <- as.numeric(id_samples_year)
            graphics::plot(koh$som_properties,  "mapping", classif =id_samples_year , bgcol= koh$som_properties$paint, main = text_year)
        }

    }

    #create a legend (fix it)
    leg <- cbind(koh$som_properties$neuron_label, koh$som_properties$paint_map)
    graphics::legend(
        "bottomright",
        legend = unique(leg[, 1]),
        col = unique(leg[, 2]),
        pch = 15,
        pt.cex = 2,
        cex = 1,
        text.col = "black",
        #horiz = T ,
        inset = c(0.0095, 0.05),
        xpd = TRUE,
        ncol = 1
    )
}
#----------------------------------


sits_plot_som_map2(som_cluster, type = "by_year", class = "Deforestation")
sits_plot_som_map2(som_cluster, type = "by_year", class = "Forest")
sits_plot_som_map2(som_cluster, type = "by_year", class = "Other")












som_clean <- sits::sits_som_clean_samples(som_cluster)

set.seed(234)
clean_samples_tb <- som_clean %>%
    magrittr::extract2("clean_samples.tb")
clean_samples_tb %>%
    dplyr::count(label)

som_cluster_clean <- clean_samples_tb %>%
    run_som()

# Print clean samples to files.
for (my_type in c("codes", "mapping")) {
    for (b in sits::sits_bands(clean_samples_tb)) {
         png(file = paste0("./roraima/data/samples/som/clean/samples_ts_som_",
                           my_type, "_", b, ".png"))
         plot(som_cluster_clean,
              type = my_type,
              whatmap = match(b, sits::sits_bands(clean_samples_tb)))
         dev.off()
    }
}


stop()

clean_samples_tb %>%
   saveRDS(file = "./roraima/data/samples/clean_samples_ts.rds")


stop()









#---- Split the samples spatially ----
samples_sf <- samples_tb %>%
    dplyr::select(id_coords, longitude, latitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326)
#set.seed(123) # all bands
#set.seed(234) # 7 bands
set.seed(345)  # samples including the label "other".
samples_grid <- samples_sf %>%
    sf::st_make_grid(n = rep(sqrt(grid_size), 2)) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(bin_id = sample(1:grid_size, size = grid_size))
# Give each polygon a bin
bin_break <- floor(seq(from = 1,
                 to = grid_size,
                 by = grid_size / 4))
samples_sf <- samples_sf %>%
    sf::st_join(samples_grid) %>%
    ensurer::ensure_that(sum(is.na(.$bin_id)) == 0,
                         err_desc = "Some samples are missing a bin_id") %>%
    dplyr::mutate(role = dplyr::case_when(bin_id %in% 1:bin_break[2]            ~ "train",
                                          bin_id %in% bin_break[2]:bin_break[3] ~ "train",
                                          bin_id %in% bin_break[3]:bin_break[4] ~ "test",
                                          TRUE                                  ~ "validation"))
samples_tb <- samples_tb %>%
    dplyr::left_join(samples_sf %>%
                         sf::st_drop_geometry() %>%
                         dplyr::select(id_coords, bin_id, role),
                     by = "id_coords") %>%
    dplyr::select(-id_coords)
samples_tb  %T>%
    (function(x){
        x %>%
            sf::st_as_sf(coords = c("longitude", "latitude"),
                         crs = 4326) %>%
            dplyr::select(label, role, bin_id) %>%
            plot()
        return(x)
    }) %>%
    dplyr::select(-time_series) %>%
    dplyr::group_by(label, role) %>%
    dplyr::summarise(samples = n())

#---- Initial split ----

train_tb <- samples_tb %>%
    dplyr::filter(role == "train") %>%
    dplyr::select(-bin_id, -role) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Missing training samples!") %>%
    (function(x){
        x %>%
            saveRDS(file = "./roraima/data/samples/samples_train.rds")
        invisible(x)
    })
test_tb <- samples_tb %>%
    dplyr::filter(role == "train") %>%
    dplyr::select(-bin_id, -role) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Missing testing samples!") %>%
    (function(x){
        x %>%
            saveRDS(file = "./roraima/data/samples/samples_test.rds")
        invisible(x)
    })
samples_tb %>%
    dplyr::filter(role == "validation") %>%
    dplyr::select(-bin_id, -role) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Missing validation samples!") %>%
    saveRDS(file = "./roraima/data/samples/samples_validation.rds")
