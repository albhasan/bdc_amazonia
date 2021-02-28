#---- set up level classification ----

## Level model (this is for the BIOME)
classification_name <- "active_learning"
satellite           <- "SENTINEL-2"
sensor              <- "MSI"
my_bands            <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")

## Level data (for list of tiles in the BIOME)
project_dir   <- "/home/alber.ipia/Documents/bdc_amazonia/roraima"
parse_info    <- c("mission", "sp_resolution",
                   "time_resolution", "type",
                   "version", "tile", "date",
                   "end_date", "band")
merge_out_dir <- paste0(project_dir, "/results/", classification_name)

# NOTE: These variables must match the number of tiles for classification.
split_dirs     <- paste0(project_dir,
                         c("/data/raster/S2_10_16D_STK/v001/079082_split"))

# split_out_dirs <- paste0(project_dir, "/results/", classification_name,
#                          c("/079082_split"))
# NOTE: Use this for Active Learning!
split_out_dirs <- paste0(project_dir, "/results/", classification_name,
                         c("/079082_split_al"))


tile_names     <- "S2_10_16D_STK_079082"

stopifnot(length(split_dirs) == length(split_out_dirs))
stopifnot(length(split_dirs) == length(tile_names))


#---- set up level tile ----

split_dir     <- split_dirs[[1]]
split_out_dir <- split_out_dirs[[1]]
cube_name     <- tile_names[[1]]


#model_file    <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/results/active_learning/ml_model.rds"
# NOTE: Use this for active learning!!!
model_file    <- "/home/alber.ipia/Documents/bdc_amazonia/roraima/results/active_learning/ml_model_active_learning.rds"

stopifnot(dir.exists(merge_out_dir))
if (!dir.exists(split_dir))
    dir.create(split_dir)
if (!dir.exists(split_out_dir))
    dir.create(split_out_dir)
stopifnot(dir.exists(split_dir))
stopifnot(dir.exists(split_out_dir))
stopifnot(file.exists(model_file))



#---- run level classification ----


if (file.exists(model_file)) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    ml_model <- readRDS(model_file)
} else {
    stop("Model file not found!")
}


#---- run level tile ----

stopifnot(file.exists("/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/S2_10_16D_STK/v001/079082_split/grid.rds"))

vrt_dirs <- list.dirs(split_dir,
                      full.names = TRUE,
                      recursive = FALSE)

class_cube <- sits::sits_cube(type = "STACK",
                              name = cube_name,
                              satellite = satellite,
                              sensor = sensor,
                              data_dir = "/home/alber.ipia/Documents/bdc_amazonia/roraima/data/raster/S2_10_16D_STK/v001/079082",
                              delim = "_",
                              parse_info = parse_info)

probs_cube <- sits::sits_cube(type = "PROBS",
                              names = classification_name,
                              satellite = satellite,
                              sensor = sensor,
                              timeline = sits::sits_timeline(class_cube),
                              labels = sort(unique(environment(ml_model)$data$label)),
                              files = "/home/alber.ipia/Documents/bdc_amazonia/roraima/results/active_learning/S2_10_16D_STK_079082_probs_2018_7.tif")

bayesian <- sits::sits_smooth(probs_cube,
                              type = "bayes",
                              window_size = 5,
                              multicores = 10,
                              memsize = 2,
                              output_dir = merge_out_dir)

?sits::sits_label_classification(probs_cube,
                                 multicores = 10,
                                 memsize = 2,
                                 output_dir = merge_out_dir)
