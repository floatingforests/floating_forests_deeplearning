#'----------------------------------------------------------
#' Download and/or Parse relevant raw data sources for tile generation
#' for deep learning pipeline
#'----------------------------------------------------------

library(gitignore)
library(glue)
library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)
library(purrr)
source("scripts/data_generation/helpers.r")

setwd(here::here())
masking_layers <- "data/masking_layers"

#'-----------
# Download Data Layers for Masking ####
#'-----------

# Falklands Coastline
SHP_url <- "https://stacks.stanford.edu/file/druid:kz201wg0124/data.zip"
download.file(SHP_url,
              destfile= glue("{masking_layers}/FLK_shape.zip"),
              method="libcurl",quiet=TRUE) #wget

unzip(glue("{masking_layers}/FLK_shape.zip"),
      exdir=glue("{masking_layers}/falklands_coastline"))

file.remove(glue("{masking_layers}/FLK_shape.zip"))

#'-----------
# Process FF Data ####
#'-----------

subjects <- read_csv("data/floating_forests_data/floating-forests-subjects.csv")

subject_pres_abs <- subjects %>%
  filter(workflow_id %in% c(3246)) %>%
  mutate(
    metadata = map(metadata, ~fromJSON(.x) %>%
                     as_tibble(.name_repair = "minimal")),
    locations = map(locations, fromJSON)) %>%
  unnest(locations) %>%
  mutate(locations = unlist(locations)) %>%
  unnest(metadata) %>%
  dplyr::select(subject_id, `#row`:locations)

saveRDS(subject_pres_abs, file = "data/floating_forests_data/subjects_presabs.RDS")

#pres/abs data
pa <- read_csv("data/floating_forests_data/kelp-presence-absence-classifications.csv")%>%
  deparse_swipe_annotation

##save rds file
saveRDS(pa, file = "data/floating_forests_data/pa.RDS")

# preamble
#pa_merged <- merge(pa,subjects,by = "subject_id")
#presence_absence_scenes <- gsub('-.*','',pa_merged$'!scene')
