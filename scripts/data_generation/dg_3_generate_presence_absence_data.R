# dg_3_generate_presence_absence_data.r
# Script to read in landsat data scene-by-scene,
# identify relevant FF subjects, and subsample the
# landsat scene to each tile for presence/absence subjects.
# Each subsampled image then masked for land and clouds, and
# the NIR/Red/Green image stacks are saved as .RData files
# with dimensions: (x,y,channel), with naming convention:
#   floating_forests_deeplearning/data/temp/
#       presence_absence/landsat_data/{tileID}.RData
#
# Henry Houskeeper; updated 1 May 2021

rm(list=ls())
#f <- list.files("data/temp/LS_scene/", include.dirs = F, full.names = T, recursive = T)
#file.remove(f)
#file.remove("data/temp/LS_scene.tar.gz")

LS_FILES_TO_RUN = c(1:15) #c(12,13,14,15)

#consensus <- 8

## Create empty vectors for metadata later:
ID_MAT <- vector()
SENSOR_MAT <- vector()
LSNAME_MAT <- vector()
PA_MAT <- vector()

require(raster)
require(gitignore)
require(rgdal)
require(sf)
require(vroom)
require(dplyr)
require(jsonlite)
require(purrr)
require(tidyr)
source("scripts/data_generation/helpers.R")
source("scripts/data_generation/deparse_raw_data.R")
options(stringsAsFactors = FALSE)

dir.create("data/temp")
dir.create("data/temp/landsat_data")
dir.create("data/temp/landsat_data/presence_absence_tiles")

gi_write_gitignore("**/temp/*")

###########################################################
#                 Load coastline shapefile                #
###########################################################
SHP_url <- "https://stacks.stanford.edu/file/druid:kz201wg0124/data.zip"
download.file(SHP_url,
              destfile="data/temp/FLK_shape.zip",
              method="libcurl",quiet=TRUE) #wget
unzip("data/temp/FLK_shape.zip",exdir="data/temp/")
paste("data/temp/",dir("data/temp/","F*.shp$"),sep="")
FALK_LAND <- st_read(paste("data/temp/",dir("data/temp/","F*.shp$"),sep=""))

###########################################################
#          Specify locations of data and metadata         #
###########################################################

#####################################
#      Presence/Absence data        #
#####################################
#subjects <- vroom("floating-forests-subjects.csv",
#                  delim=",")
subjects <- read.csv("data/floating-forests-subjects.csv",
                               header = TRUE)

#subjects <- subjects %>%
#  filter(workflow_id %in% c(3246, 14705, 11268))
subjects <- subjects %>%
  filter(workflow_id %in% c(3246))

## parse json
subjects <-  subjects %>%
  mutate(
    metadata = map(metadata, ~fromJSON(.x) %>%
                     as_tibble(.name_repair = "minimal")),
    locations = map(locations, fromJSON)) %>%
  unnest(locations) %>%
  mutate(locations = unlist(locations)) %>%
  unnest(metadata) %>%
  dplyr::select(subject_id, `#row`:locations)

## save rds file
#saveRDS(subjects, file = "data/temp/subjects.RDS")

#'-----------------------
# Falklands Pres_abs ####
#'-----------------------
#pa <- vroom("kelp-presence-absence-classifications.csv")
pa <- read.csv("data/kelp-presence-absence-classifications.csv",
                     header = TRUE)
## deparse JSON
pa <- pa %>%
  deparse_swipe_annotation

##save rds file
#saveRDS(pa, file = "data/temp/pa.RDS")

#pa_merged <- dplyr::left_join(pa,subjects)
pa_merged <- merge(pa,subjects,by = "subject_id")
presence_absence_scenes <- gsub('-.*','',pa_merged$'!scene')
## remove
rm(subjects)
rm(pa)

###########################################################
#   Read scene-by-scene through landsat practice scenes   #
###########################################################
## Landsat scenes obtained from:
#LSdir <- "https://www.dropbox.com/sh/3c3clh6em4iyd5u/AABWKGaNgIj0OISenVkVe8n0a/practice_scenes?dl=0&subfolder_nav_tracking=1"

# Instead of downloading full directory, loop scene-by-scene using manifest file...
LSurls <- read.delim(file="data/Practice_Manifest.txt", header=F, sep = "\n")

## To test by running just scene 1, can use:
#LSurl <- "https://www.dropbox.com/sh/3c3clh6em4iyd5u/AAASKt8Bxai373CkBdX7KJG9a/practice_scenes/LC082200962017012201T1-SC20171128154358.tar.gz?dl=0"
#LSname <- strsplit(LSurl,"[/,-]")[[1]][8]

for (LSurl in LSurls[LS_FILES_TO_RUN,1]){

  ###########################################################
  #          Part 1: Preprocess Segmentation Data           #
  ###########################################################

  dummy <- strsplit(LSurl,"/")[[1]][8]
  LSname <- strsplit(dummy,"-")[[1]][1]
  #print(LSname)
  rm(dummy)

  i <- grep(LSname,presence_absence_scenes)
  if (length(i) > 0) {

    ## Extract sensor name:
    sensor <- pa_merged$sensor_id[i[1]]

    ## Download LS scene and unzip:
    print(paste("Downloading",LSname,"...",sep=" "))
    download.file(LSurl,
      destfile="data/temp/LS_scene.tar.gz",
      method="wget",quiet=TRUE) #method="libcurl"
    untar("data/temp/LS_scene.tar.gz",
      exdir = "data/temp/LS_scene")

    ## Define filenames for landsat scene
    if (grepl("OLI_TIRS",sensor)) {
        NIRname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*_sr_band5.tif"),
                    sep = "")
        REDname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*_sr_band4.tif"),
                    sep = "")
        GREENname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*_sr_band3.tif"),
                    sep = "")
        CLOUDname <- paste("data/temp/LS_scene/",
                           dir("data/temp/LS_scene","L*_pixel_qa.tif"),
                           sep = "")
        LSID <- 8
    } else if (grepl("ETM",sensor)){
        NIRname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*_sr_band4.tif"),
                    sep = "")
        REDname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*_sr_band3.tif"),
                    sep = "")
        GREENname <- paste("data/temp/LS_scene/",
                      dir("data/temp/LS_scene","L*_sr_band2.tif"),
                      sep = "")
        CLOUDname <- paste("data/temp/LS_scene/",
                           dir("data/temp/LS_scene","L*_sr_cloud_qa.tif"),
                           sep = "")
        LSID <- 7
    } else if (grepl("TM",sensor)){
        NIRname <- paste("data/temp/LS_scene/",
                       dir("data/temp/LS_scene","L*_sr_band4.tif"),
                       sep = "")
        REDname <- paste("data/temp/LS_scene/",
                     dir("data/temp/LS_scene","L*_sr_band3.tif"),
                     sep = "")
        GREENname <- paste("data/temp/LS_scene/",
                       dir("data/temp/LS_scene","L*_sr_band2.tif"),
                       sep = "")
        CLOUDname <- paste("data/temp/LS_scene/",
                           dir("data/temp/LS_scene","L*_sr_cloud_qa.tif"),
                           sep = "")
        LSID <- 5
    }

    ## Extract information from tiff layers and form image stack:
    NIR <- raster(NIRname)
    RED <- raster(REDname)
    GREEN <- raster(GREENname)

    NRG <- brick(NIR,RED,GREEN,
                      xmn = as.numeric(pa_merged$'#scene_corner_UL_x'[i[1]]),
                      xmx = as.numeric(pa_merged$'#scene_corner_LR_x'[i[1]]),
                      ymn = as.numeric(pa_merged$'#scene_corner_LR_y'[i[1]]),
                      ymx = as.numeric(pa_merged$'#scene_corner_UL_y'[i[1]]),
                      crs=CRS(paste("+proj=utm +units=m +zone=",
                                    pa_merged$'#utm_zone'[i[1]],sep="")))
    ## plot(raster(NRG,layer=1))

    ## Reproject coastline shapefile onto landsat scene projection:
    FALK_LAND_UTM <- st_transform(FALK_LAND,crs(raster(NRG, layer=1)))

    ## Mask out land:
    NRG_masked_land <- mask(NRG, FALK_LAND_UTM, inverse=TRUE)
    ## plot(raster(NRG_masked_land,layer=1))

    ## Mask out clouds:
    QA <- raster(CLOUDname)
    CLOUDmask <- QA*0

    if (LSID == 8){
      ## bit interpretation for LS-8 (using pixel_qa):
      ##  Attribute           PixelValue
      ##   Fill                1
      ##   Clear               322, 386, 834, 898, 1346
      ##   Water               324, 388, 836, 900, 1348
      ##   Cloud Shadow        328, 392, 840, 904, 1350
      ##   Snow/Ice            336, 368, 400, 432, 848, 880, 912, 944, 1352
      ##   Cloud               352, 368, 416, 432, 480, 864, 880, 928, 944, 992
      MVs = c(352, 368, 416, 432, 480, 864, 880, 928, 944, 992)
    } else {
      ## bit interpretation for LS-7 and LS-5:
      ##  Attribute           PixelValue
      ##   DDV                 1, 9
      ##   Cloud               2, 34
      ##   Cloud Shadow        4, 12, 20, 36, 52
      ##   Adjacent to Cloud   8, 12, 24, 40, 56
      ##   Snow                16, 20, 24, 48, 52, 56
      ##   Water               32, 34, 36, 40, 48, 52, 56
      MVs = c(2, 34)
    }

    for (MV in MVs) {
      CLOUDmask[QA == MV] <- 1
    }
    ## plot(CLOUDmask)
    NRG_masked_land_cloud <- mask(NRG_masked_land, CLOUDmask,
                                  maskvalue=1, inverse=FALSE)
    ## plot(raster(NRG_masked_land_cloud,layer=1))

    ## loop through each FF tile:
    j <- 0

    for(k in i) {
      j <- j+1
      print(paste("Processing subject",pa_merged$subject_id[k],
                "-- Tile",j,"of",length(i),"in LS scene:",LSname,
                sep=" "))

        ## Extract Landsat brick subset:
        tile_extent <- extent(as.numeric(pa_merged$'#tile_LL_x'[k]),
                              as.numeric(pa_merged$'#tile_UR_x'[k]),
                              as.numeric(pa_merged$'#tile_LL_y'[k]),
                              as.numeric(pa_merged$'#tile_UR_y'[k]))
        NRG_data <- crop(NRG_masked_land_cloud,tile_extent)

        ## Save Landsat brick subset:
        save(NRG_data,file = paste(
          "data/temp/presence_absence/landsat_data/",pa_merged$subject_id[k],
          ".RData",sep=""))

        ## pa is a binary indicating presence (1) or absence (2)
        ##   based on average swipe outcome:
        pa <- round(
              (pa_merged$yes[k])/(pa_merged$yes[k] + pa_merged$no[k])
                    )

        ## record subject ID, sensor #, and LS filename:
        PA_MAT <- c(PA_MAT,pa)
        SENSOR_MAT <- c(SENSOR_MAT,LSID)
        LSNAME_MAT <- c(LSNAME_MAT,LSname)
        ID_MAT <- c(ID_MAT,pa_merged$subject_id[k])

        remove(pa,NRG_data,tile_extent)

    } ## end loop through tiles corresponding to scene

    ## Remove LS scene from temp directory:
    f <- list.files("data/temp/LS_scene/", include.dirs = F, full.names = T, recursive = T)
    file.remove(f)
    file.remove("data/temp/LS_scene.tar.gz")

    remove(QA,CLOUDmask,CLOUDname,MV,MVs,
           sensor,NRG,FALK_LAND_UTM,
           NRG_masked_land,NRG_masked_land_cloud,
           GREEN,GREENname,
           RED,REDname,
           NIR,NIRname)
  } ## end if statement checking that >0 FF tiles correspond to scene

  remove(LSname,i)

} ## end of loop through scenes in manifest file...

## Save metadata for the tiles and LS bricks saved in the data directories:
save(ID_MAT,SENSOR_MAT,LSNAME_MAT,PA_MAT,
     file = "data/temp/presence_absence/META.RData")

## Remove the raster tiles from the temp directory:
f <- list.files("data/temp/raster_tiles/", include.dirs = F, full.names = T, recursive = T)
dummy <- file.remove(f)
