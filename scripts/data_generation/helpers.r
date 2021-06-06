library(raster)
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)

rasterizeFFImage <- function(subject){
  subj_metadata <- ff_relaunch_subjects %>% filter(subject_id == subject)
  subj_url <- subj_metadata$locations

  crs <- str_c("+proj=utm +zone=",  subj_metadata$`#utm_zone`,
               " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

  download.file(subj_url, basename(subj_url), mode = 'wb')

  img <- raster::brick(basename(subj_url),
                       crs=crs)

  img@extent = raster::extent(as.numeric(subj_metadata$`#tile_UL_x`), as.numeric(subj_metadata$`#tile_LR_x`),
                              as.numeric(subj_metadata$`#tile_LR_y`), as.numeric(subj_metadata$`#tile_UL_y`))

  return(img)

}

deparse_swipe_annotation <- . %>%
  dplyr::select(classification_id, subject_ids,
         annotations) %>%
  mutate(annotations = map(annotations,
                           ~fromJSON(.x) %>%
                             as_tibble(.name_repair = "minimal")) %>%
           map(~as_tibble(.) %>%`[[`(3) %>% unlist)) %>% #dealing with weird nested JSON
  unnest(annotations) %>%
  group_by(subject_ids) %>%
  summarize(yes = sum(annotations == "Yes", na.rm=TRUE),
            no = sum(annotations == "No", na.rm=TRUE),
            bad_image = sum(annotations == "Bad image", na.rm=TRUE),
            clouds = sum(annotations == "Clouds", na.rm=TRUE)) %>%
  ungroup() %>%
  rename(subject_id = subject_ids)



make_poly <- function(arow){
  mat <- with(arow,
              matrix(
                c(`#tile_LL_x`, `#tile_LL_y`,
                  `#tile_UL_x`, `#tile_UL_y`,
                  `#tile_UR_x`, `#tile_UR_y`,
                  `#tile_LR_x`, `#tile_LR_y`,
                  `#tile_LL_x`, `#tile_LL_y`) %>% as.numeric(),
                ncol=2, byrow=TRUE)
  )
  #mat_crs <- st_crs("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")
  st_polygon(list(mat))
}

