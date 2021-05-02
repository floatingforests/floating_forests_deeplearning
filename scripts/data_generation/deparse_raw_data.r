#'------------------------------------------
#' Process raw classifications and save
#' as rdata files to save space and deal with JSON
#'------------------------------------------
 
library(vroom)
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)
source("scripts/helpers.R")

options(stringsAsFactors = FALSE)

#'--------------------
# Subjects ####
#'--------------------

subjects <- vroom("raw_data/floating-forests-subjects.csv") 

subjects <- subjects %>% 
  filter(workflow_id %in% c(3246, 14705, 11268))

# parse json
subjects <-  subjects %>% 
  mutate(
    metadata = map(metadata, ~fromJSON(.x) %>% 
                     as_tibble(.name_repair = "minimal")),
    locations = map(locations, fromJSON)) %>%  
  unnest(locations) %>%
  mutate(locations = unlist(locations)) %>%  
  unnest(metadata) %>%
  dplyr::select(subject_id, `#row`:locations)

# save rds file
saveRDS(subjects, file = "data/subjects.RDS")

# remove
rm(subjects)

#'-----------------------
# Falklands Pres_abs ####
#'-----------------------
falk <- vroom("raw_data/kelp-presence-absence-classifications.csv")

# deparse JSON
falk <- falk %>%
  deparse_swipe_annotation


#save rds file
saveRDS(falk, file = "data/falk.RDS")

#remove
rm(falk)

#'--------------------
# Kelp on the Edge ####
#'--------------------

#
koe <- vroom("raw_data/find-kelp-on-the-edge-classifications.csv")

# deparse JSON
koe <- koe %>%
  deparse_swipe_annotation

# save rds file
saveRDS(koe, file = "data/koe.RDS")

# remove
rm(koe)


#'----------------------
# Urban Kelp Finder ####
#'----------------------
urban <- vroom("raw_data/find-urban-kelp-classifications.csv")

urban <- urban %>%
  deparse_swipe_annotation

saveRDS(urban, file = "data/urban.Rds")

rm(urban)