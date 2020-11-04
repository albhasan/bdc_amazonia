library(dplyr)

#---- Configuration ----

num_of_trees <- 1000
my_model <- paste("rf", num_of_trees, sep = "-")
#my_cube <- "S2_10_16D_STK"
#my_tiles <- "079082"
#my_version <- "v001"

train_tb <- readRDS("./roraima/data/samples/samples_train.rds")

#---- Choose the best band combination using kfolds ----

# Helper for running k-folds
run_kfolds <- function(bands, samples_tb, n_samples){
    #lapply(1:100, function(i){
    lapply(1:10, function(i){
        samples_tb %>%
            sits::sits_sample(n = n_samples) %>%
            sits::sits_select(bands) %>%
            sits::sits_kfold_validate(folds = 10,
                  ml_method = sits::sits_rfor(num_trees = num_of_trees)) %>%
            sits::sits_conf_matrix() %>%
            return()
    })
}

# Get the accuracy from the R objetc
get_accuracy <- function(x){
    c_mat <- x %>%
        magrittr::extract2("table")
    producer_acc <- diag(c_mat) / colSums(c_mat, na.rm = TRUE)
    user_acc     <- diag(c_mat) / rowSums(c_mat, na.rm = TRUE)
    pa <- tibble::as_tibble(t(producer_acc))
    ua <- tibble::as_tibble(t(user_acc))
    colnames(pa) <- paste0(names(producer_acc), "_pa")
    colnames(ua) <- paste0(names(user_acc),     "_ua")
    return(dplyr::bind_cols(pa, ua))
}

experiments <- list(all_bands = c("B01", "B02", "B03", "B04", "B05", "B06",
                                  "B08", "B8A", "B07", "B11", "B12"),
                    seven_bands = c("B02", "B03", "B04", "B08", "B8A",  "B11",
                                    "B12"),
                    two_bands = c("B04", "B08"))

kfold_ls <- lapply(experiments, run_kfolds, samples_tb = train_tb,
                   n_samples = sum(train_tb$label == "Deforestation"))

experiments_tb <- tibble::tibble(experiment = names(experiments)) %>%
    dplyr::mutate(kfold = kfold_ls) %>%
    tidyr::unnest(cols = c(kfold)) %>%
    dplyr::mutate(pu_accuracy = purrr::map(kfold, get_accuracy)) %>%
    tidyr::unnest(pu_accuracy)

experiments_tb %>%
    dplyr::group_by(experiment) %>%
    dplyr::summarise(mean_def_pa = mean(Deforestation_pa),
                     sd_def_pa   = sd(Deforestation_pa),
                     mean_def_ua = mean(Deforestation_ua),
                     sd_def_ua   = sd(Deforestation_ua),
                     mean_for_pa = mean(Forest_pa),
                     sd_for_pa   = mean(Forest_pa),
                     mean_for_ua = mean(Forest_ua),
                     sd_for_ua   = mean(Forest_ua))

# NOTE: Results using 1/3 of the samples for training
# # A tibble: 2 x 9
# experiment    mean_def_pa sd_def_pa mean_def_ua sd_def_ua mean_for_pa sd_for_pa mean_for_ua sd_for_ua
# <chr>               <dbl>     <dbl>       <dbl>     <dbl>       <dbl>     <dbl>       <dbl>     <dbl>
# 1 all_bands         0.953   0.00680       0.954   0.00796       0.954     0.954       0.953     0.953
# 2 seven_bands       0.947   0.00789       0.952   0.00780       0.952     0.952       0.947     0.947

#---- Test different models -----
