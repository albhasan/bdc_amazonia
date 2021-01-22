# Exploratory data analysis.

stop("DEPRECATED: I'm using adversarial learning!")



library(dplyr)
library(ggplot2)
library(sits)
library(stringr)

train_tb <- "./roraima/data/samples/samples_train.rds" %>%
    readRDS()
test_tb <- "./roraima/data/samples/samples_test.rds" %>%
    readRDS()
validation_tb <- "./roraima/data/samples/samples_validation.rds" %>%
    readRDS()
samples_tb <- train_tb %>%
    dplyr::bind_rows(test_tb) %>%
    dplyr::bind_rows(validation_tb)
rm(train_tb, test_tb, validation_tb)


#---- Plot sample time series ----

samples_plot <- samples_tb %>%
    tidyr::unnest(time_series) %>%
    dplyr::select(-longitude, -latitude, -cube,
                  Label = label) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("B"),
                        names_to = "Band",
                        values_to = "Value") %>%
    dplyr::filter(Band %in% c("B02", "B03", "B04", "B08", "B8A")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Index,
                                       y = Value,
                                       group = interaction(Index, Band))) +
    ggplot2::geom_smooth(ggplot2::aes(x = Index,
                                      y = Value,
                                      group =  Band,
                                      color = Label)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90)) +
    ggplot2::facet_grid(rows = vars(Label),
                        cols = vars(Band))
print(samples_plot)
ggplot2::ggsave(samples_plot,
                filename = "./roraima/data/samples/samples_time_series.png",
                width = 297, height = 210, units = "mm")


#---- PCA ----

samples_pca <- samples_tb %>%
    tidyr::unnest(cols = time_series) %>%
    dplyr::select(label, B01:B8A) %>%
    tidyr::nest(data = tidyselect::everything()) %>%
    dplyr::mutate(pca = purrr::map(data,
                                   ~ stats::prcomp(.x %>% dplyr::select(-label),
                                                   center = TRUE,
                                                   scale = TRUE)),
                  pca_aug = purrr::map2(pca, data,
                                        ~broom::augment(.x, data = .y)))

var_explained <- samples_pca %>%
    tidyr::unnest(pca_aug) %>%
    dplyr::select(tidyselect::contains(".fittedPC")) %>%
    dplyr::summarize_at(.vars = vars(contains("PC")),
                        .funs = funs(var)) %>%
    tidyr::gather(key = pc, value = variance) %>%
    dplyr::mutate(var_exp = variance/sum(variance),
                  cum_var_exp = cumsum(var_exp),
                  pc = str_replace(pc, ".fitted", ""))

# NOTE: The top 3 components explain 96% of the variance.
var_explained

# NOTE: This plot shows uses hexagons to count each observation on each sample
# time series. The labels overlap, specially Forest & Other.
samples_pca %>%
    tidyr::unnest(cols = pca_aug) %>%
    dplyr::select(Label = label,
                  PC1 = .fittedPC1,
                  PC2 = .fittedPC2,
                  PC3 = .fittedPC3) %>%
    ggplot2::ggplot() +
    ggplot2::geom_hex(ggplot2::aes(x = PC1, y = PC2)) +
    coord_fixed(ratio = 1) +
    ggplot2::facet_wrap(~ Label) +
    ggplot2::ggtitle(paste("Principal Components Analysis (",
                           paste(format(var_explained$cum_var_exp[2],
                                        digits = 2),
                                 "%",
                                 collapse = "",
                                 sep = ""),
                           "var)"))























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
