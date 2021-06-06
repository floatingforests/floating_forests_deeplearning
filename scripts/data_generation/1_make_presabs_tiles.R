library(readr)
library(purrr)
library(dplyr)

# Load a function to process one scene into tiles
source("process_landsat_scene_to_tiles.R")

setwd(here::here())

# Cleanup if we are starting fresh
f <- list.files("data/temp/presence_absence/landsat_data/", include.dirs = F, full.names = T, recursive = T)
file.remove(f)

# What scenes are we processing today
LSfiles <- list.files("data/landsat_scenes",
                      recursive = TRUE,
                      full.names = TRUE) %>%
  keep(~ grepl("\\.tar\\.gz", .x)) #just the gzips!

#debug
LSfiles <- LSfiles[1:10]

# coastline
coastline <- st_read("data/masking_layers/falklands_coastline")

# What do we need to know about these scenes viz a viz floating forests
# get the subject and classification info and merge
subjects <- readRDS("data/floating_forests_data/subjects_presabs.RDS")
pa <- readRDS("data/floating_forests_data/pa.RDS")

# make a merged file
pa_merged <- merge(pa,subjects,by = "subject_id")
presence_absence_scenes <- gsub('-.*','',pa_merged$'!scene')

# A little cleanup of objects we do not need
rm(subjects)
rm(pa)

#Loop over all files to make tiff tiles
walk(LSfiles[1:2],
     ~make_LS_tiles(one_ls_file = .x,
                    scenes_to_be_used = presence_absence_scenes,
                    subject_info = pa_merged,
                    coastline = coastline,
                    out_dir = "data/temp/presence_absence/landsat_data/"))

