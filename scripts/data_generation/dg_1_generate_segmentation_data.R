# dg_1_generate_segmentation_data.r
# Script to read in landsat data scene-by-scene,
# identify relevant FF tiles, and subsample the
# landsat scene to each tile in the scene.
# Each subsampled image then masked for land and clouds, and
# the NIR/Red/Green image stacks are saved as .RData files
# with dimensions: (x,y,channel), with naming convention:
#   floating_forests_deeplearning/data/temp/
#       segmentation/landsat_data/{tileID}.RData
# Floating forest tiles converted to binary using consensus
# and saved as .RData files with naming convention:
#   floating_forests_deeplearning/data/temp/
#       segmentation/floating_forests_data/Consensus_{tileID}.RData
#
# Henry Houskeeper; updated 1 May 2021

rm(list=ls())
f <- list.files("data/temp/LS_scene/", include.dirs = F, full.names = T, recursive = T)
file.remove(f)
file.remove("data/temp/LS_scene.tar.gz")

LS_FILES_TO_RUN = c(1:25)

consensus <- 8

## Create empty vectors for metadata later:
Missing_Tiles <- vector()
ID_MAT <- vector()
SENSOR_MAT <- vector()
LSNAME_MAT <- vector()
PA_MAT <- vector()

require(raster)
require(gitignore)
require(rgdal)

dir.create("data/temp")
dir.create("data/temp/segmentation")
dir.create("data/temp/segmentation/floating_forests_data")
dir.create("data/temp/segmentation/landsat_data")
dir.create("data/segmentation")

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

## Tile metadata obtained from:
csv_url <- "https://www.dropbox.com/s/9qjuvkqh5wwop1m/ff_relaunch_subjects.csv?dl=1#"
tile_metadata_full <- read.csv(csv_url,
                               header = TRUE)
# Keep the tiles that either had full (15) classifications, or had no kelp
tile_metadata <- subset(tile_metadata_full, retirement_reason != "consensus")
csv_scenes <- gsub('-.*','',tile_metadata$X.scene)

## Tile data obtained from:
tile_data <- "https://www.dropbox.com/s/yadishiz4k8008k/falklands_raster_tiles.tar.gz?dl=1"
## (To Do: update code to only pull the tiles relevant to an individual scene)
#tile_data <- paste("https://www.dropbox.com/s/yadishiz4k8008k/falklands_raster_tiles.tar.gz?dl=1",
#                "&file_subpath=%2Fraster_tiles%2F",tile_metadata$subject_id[k],".grd",
#                sep="")

print("Downloading Floating Forest Tiles...")
#download.file(tile_data,
#              destfile="data/temp/raster_tiles.tar.gz",
#              method="wget",quiet=TRUE)
untar("data/temp/raster_tiles.tar.gz",
      exdir = "data/temp")
tile_names <- dir("data/temp/raster_tiles")
tile_names <- unique(substr(tile_names,1, nchar(tile_names)-4))

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
  #for (LSurl in LSurls[,1]){
  #print(LSurl)

  if (strsplit(LSurl,"/")[[1]][3] == "www.dropbox.com") {
  dummy <- strsplit(LSurl,"/")[[1]][8]
  LSname <- strsplit(dummy,"-")[[1]][1]
  #print(LSname)
  rm("dummy")
  } else if (strsplit(LSurl,"/")[[1]][3] == "app.box.com") {

    ## Download LS scene and unzip:
    print(paste("Downloading a scene from Box...",sep=" "))
    download.file(LSurl,
                  destfile="data/temp/LS_scene.tar.gz",
                  method="wget",quiet=TRUE) #method="libcurl"
    untar("data/temp/LS_scene.tar.gz",
          exdir = "data/temp/LS_scene")

    dummy <- strsplit(dir("data/temp/LS_scene","L*.tif")[1],"_")
    LSname <- paste(dummy[[1]][1],dummy[[1]][3],dummy[[1]][4],"01T1",sep="")
  } # End determining LSname based on dropbox or box direct downloads.

  i <- grep(LSname,csv_scenes)
  if (length(i) > 0) {

    ## Extract sensor name:
    sensor <- tile_metadata$sensor_id[i[1]]

    ## If file from Box, already downloaded LS scene. If from dropbox, need to get it now:
    if (strsplit(LSurl,"/")[[1]][3] == "www.dropbox.com") {
    ## Download LS scene and unzip:
    print(paste("Downloading",LSname,"...",sep=" "))
    download.file(LSurl,
                  destfile="data/temp/LS_scene.tar.gz",
                  method="wget",quiet=TRUE) #method="libcurl"
    untar("data/temp/LS_scene.tar.gz",
          exdir = "data/temp/LS_scene")
    }

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
                 xmn = tile_metadata$X.scene_LL_x[i[1]],
                 xmx = tile_metadata$X.scene_UR_x[i[1]],
                 ymn = tile_metadata$X.scene_LL_y[i[1]],
                 ymx = tile_metadata$X.scene_UR_y[i[1]],
                 crs=CRS(paste("+proj=utm +units=m +zone=",
                               tile_metadata$X.utm_zone[i[1]],sep="")))
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
      print(paste("Processing subject",tile_metadata$subject_id[k],
                  "-- Tile",j,"of",length(i),"in LS scene:",LSname,
                  sep=" "))

      if (length(grep(tile_metadata$subject_id[k],tile_names))>0) {
        print(paste(
          "Tile",tile_metadata$subject_id[k],"exists.",sep=" "))

        ## Extract Landsat brick subset:
        tile_extent <- extent(tile_metadata$X.tile_LL_x[k],
                              tile_metadata$X.tile_UR_x[k],
                              tile_metadata$X.tile_LL_y[k],
                              tile_metadata$X.tile_UR_y[k])
        NRG_data <- crop(NRG_masked_land_cloud,tile_extent)

        ## Extract Floating Forests tile:
        if (tile_metadata$retirement_reason[k] == "nothing_here") {
          tile_consensus <- raster(NRG_data,layer=1)*0
        } else{
        tile_data_10m <- raster(paste(
          "data/temp/raster_tiles/",tile_metadata$subject_id[k],".gri",sep=""))
        ## Resample to 30m to match landsat scene:
        tile_data <- resample(tile_data_10m,NRG_data,method="ngb")
        ## Convert # annotations per pixel to consensus pixels:
        tile_consensus <- tile_data
        tile_consensus[is.na(tile_consensus)] <- 0
        tile_consensus[tile_consensus < consensus] <- 0
        tile_consensus[tile_consensus >= consensus] <- 1
        remove(tile_data,tile_data_10m)
        }

        ## Mask consensus data for land:
        ## (don't need to crop land since shapefile...)
        tile_consensus_masked_land <- mask(tile_consensus,
                                           FALK_LAND_UTM, inverse=TRUE)
        ## Mask consensus data for cloud:
        cloud_tile <- crop(CLOUDmask,tile_extent)
        tile_consensus_masked_land_cloud <- mask(tile_consensus_masked_land,
                                                 cloud_tile, maskvalue=1, inverse=FALSE)

        ### Save tile data:
        #save(tile_data,file = paste(
        #  "data/temp/floating_forests_data/",tile_metadata$subject_id[k],
        #  ".RData",sep=""))

        ## Save consensus tile data:
        save(tile_consensus_masked_land_cloud,file = paste(
          "data/temp/segmentation/floating_forests_data/Consensus_",tile_metadata$subject_id[k],
          ".RData",sep=""))
        ## Save Landsat brick subset:
        save(NRG_data,file = paste(
          "data/temp/segmentation/landsat_data/",tile_metadata$subject_id[k],
          ".RData",sep=""))

        ## record subject ID, sensor #, and LS filename:
        ID_MAT <- c(ID_MAT,tile_metadata$subject_id[k])
        SENSOR_MAT <- c(SENSOR_MAT,LSID)
        LSNAME_MAT <- c(LSNAME_MAT,LSname)
        PA_MAT <- c(PA_MAT,max(as.matrix(tile_consensus_masked_land_cloud),na.rm=TRUE))

        remove(NRG_data,cloud_tile,tile_extent,
               tile_consensus_masked_land,tile_consensus_masked_land_cloud)

      } else {
        print(paste(
          "Tile",tile_metadata$subject_id[k],"does not exist.",sep=" "))
        ## record that these tiles are missing:
        Missing_Tiles <- c(Missing_Tiles,tile_metadata$subject_id[k])
      } ## end if statement checking if tiles exist

    } ## end loop through tiles corresponding to scene

    ## Remove LS scene from temp directory:
    f <- list.files("data/temp/LS_scene/", include.dirs = F, full.names = T, recursive = T)
    file.remove(f)
    file.remove("data/temp/LS_scene.tar.gz")

    ## Clean up workspace:
    remove(QA,CLOUDmask,CLOUDname,MV,MVs,
           NRG_masked_land,NRG_masked_land_cloud,
           GREEN,GREENname,
           RED,REDname,
           NIR,NIRname)

  } else {
    print(paste("Scene ",LSname," had no segmentation tiles...",sep=""))
    } ## end if statement checking that >0 FF tiles correspond to scene

  remove(i,LSname)

} ## end of loop through scenes in manifest file...

## Save metadata for the tiles and LS bricks saved in the data directories:
save(ID_MAT,SENSOR_MAT,LSNAME_MAT,Missing_Tiles,PA_MAT,
     file = "data/segmentation/META.RData")

## Remove the raster tiles from the temp directory:
f <- list.files("data/temp/raster_tiles/", include.dirs = F, full.names = T, recursive = T)
dummy <- file.remove(f)
#file.remove("data/temp/raster_tiles.tar.gz")
