#' --------------------------------------------------------------------------------
#' Take presence/absence .Rdata files, and turn them into tifs and assign them
#' to training, testing, or validation making sure to stratify by yes/no,
#' sensor, and anything else we want
#' --------------------------------------------------------------------------------

# libraries
require(raster)
require(sf)
require(rgdal)
library(dplyr)
library(glue)

# setup environment
setwd(here::here())
set.seed(20210517) # first time written

# What is our yes/no threshold
yes_no_threshold <- 0.5
train_test_valid_split <- c(0.6, 0.2, 0.2)

# load the subject info
# What do we need to know about these scenes viz a viz floating forests
# get the subject and classification info and merge
subjects <- readRDS("data/floating_forests_data/subjects_presabs.RDS")
pa <- readRDS("data/floating_forests_data/pa.RDS")

# make a merged file
pa_merged <- merge(pa, subjects, by = "subject_id")

# create directories in case they are not here
dir.create("data/presence_absence")
dir.create("data/presence_absence/landsat_tiles")
dir.create("data/presence_absence/landsat_tiles/train")
dir.create("data/presence_absence/landsat_tiles/train/yes")
dir.create("data/presence_absence/landsat_tiles/train/no")
dir.create("data/presence_absence/landsat_tiles/valid")
dir.create("data/presence_absence/landsat_tiles/valid/yes")
dir.create("data/presence_absence/landsat_tiles/valid/no")
dir.create("data/presence_absence/landsat_tiles/test")
dir.create("data/presence_absence/landsat_tiles/test/yes")
dir.create("data/presence_absence/landsat_tiles/test/no")


## Make sure tile directories are empty...
f <- list.files("data/presence_absence/landsat_tiles", include.dirs = F, full.names = T, recursive = T)
dummy <- file.remove(f)
remove(f, dummy)

# what tiles are we dealing with
all_tiles <- list.files("data/temp/presence_absence/landsat_data",
  full.names = TRUE
)

# merge the filtered data in and calculate whether things are yes or no
pa_filtered <- pa_merged %>%
  dplyr::select(subject_id, yes, no, sensor_id) %>%
  filter(subject_id %in% gsub(
    "(data/temp/presence_absence/landsat_data/)(.*)(.RData)",
    "\\2", all_tiles
  )) %>%
  mutate(
    yes_no = yes / (yes + no) > yes_no_threshold,
    yes_no = c("no", "yes")[yes_no + 1]
  ) %>%
  dplyr::select(-yes, -no) %>%
  # our strata
  group_by(sensor_id, yes_no) %>%
  # use sample to assign tiles to train/test/valid
  mutate(
    train_test_validation =
      sample(c("train", "test", "valid"),
        replace = TRUE,
        n(),
        prob = train_test_valid_split
      )
  ) %>%
  ungroup()


# now use this data frame to push .Rdata files into the right directory
write_one_tile <- function(arow) {
  subj_file <- glue("data/temp/presence_absence/landsat_data/{arow$subject_id}.RData")
  load(subj_file) # creates NRG_data

  print(glue("writing {arow$subject_id} to {arow$train_test_validation}/{arow$yes_no}"))

  ls_out <- glue("data/presence_absence/landsat_tiles/{arow$train_test_validation}/{arow$yes_no}/{arow$subject_id}.tif")
  mls <- writeRaster(NRG_data, filename = ls_out, format = "GTiff", overwrite = TRUE)

  rm(NRG_data)
}


# the loop to write out the train/test/valid set to tifs
split(pa_filtered, 1:nrow(pa_filtered)) %>%
  walk(write_one_tile)
