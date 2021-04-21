# dg_1.r
# Script to read in landsat data scene-by-scene,
# identify relevant FF tiles, and subsample the
# landsat scene to each tile in the scene.
# Each subsampled image then saved as a data stack .RData file
# with dimensions: (x,y,channel), with naming convention:
# floating_forests_deeplearning/data/landsat_scenes/landsat_data/{tileID}.RData

#rm(list=ls())
#LS_FILES_TO_RUN = c(1,2)

require(raster)
require(gitignore)

dir.create("data/temp")
gi_write_gitignore("**/temp/*")
gi_write_gitignore("**/floating_forests_data/*")
gi_write_gitignore("**/landsat_data/*")

###########################################################
#          Specify locations of data and metadata         #
###########################################################

## Tile metadata obtained from:
csv_url = "https://www.dropbox.com/s/9qjuvkqh5wwop1m/ff_relaunch_subjects.csv?dl=1#"
tile_metadata = read.csv(csv_url,
                     header = TRUE)
# Keep the tiles that either had full (15) classifications, or had no kelp
tile_metadata = tile_metadata[
    tile_metadata$retirement_reason == "classification_count" |
    tile_metadata$retirement_reason == "nothing_here",]
csv_scenes = gsub('-.*','',tile_metadata$X.scene)

## Tile data obtained from:
tile_data = "https://www.dropbox.com/s/yadishiz4k8008k/falklands_raster_tiles.tar.gz?dl=1"
## (To Do: update code to only pull the tiles relevant to an individual scene)
#tile_data = paste("https://www.dropbox.com/s/yadishiz4k8008k/falklands_raster_tiles.tar.gz?dl=1",
#                "&file_subpath=%2Fraster_tiles%2F",tile_metadata$subject_id[k],".grd",
#                sep="")

download.file(tile_data,
              destfile="data/temp/raster_tiles.tar.gz",
              method="wget")
untar("data/temp/raster_tiles.tar.gz",
      exdir = "data/temp")
tile_names <- dir("data/temp/raster_tiles")
tile_names <- unique(substr(tile_names,1, nchar(tile_names)-4))

## Create empty vectors for metadata later:
Missing_Tiles <- vector()
ID_MAT <- vector()
SENSOR_MAT <- vector()
LSNAME_MAT <- vector()

###########################################################
#   Read scene-by-scene through landsat practice scenes   #
###########################################################
## Landsat scenes obtained from:
#LSdir = "https://www.dropbox.com/sh/3c3clh6em4iyd5u/AABWKGaNgIj0OISenVkVe8n0a/practice_scenes?dl=0&subfolder_nav_tracking=1"

# Instead of downloading full directory, loop scene-by-scene using manifest file...
LSurls <- read.delim(file="data/Practice_Manifest.txt", header=F, sep = "\n")
nLSurls <- length(LS_FILES_TO_RUN)

## To tst by running just scene 1, can use:
#LSurl = "https://www.dropbox.com/sh/3c3clh6em4iyd5u/AAASKt8Bxai373CkBdX7KJG9a/practice_scenes/LC082200962017012201T1-SC20171128154358.tar.gz?dl=0"
#LSname = strsplit(LSurl,"[/,-]")[[1]][8]

#for (LSurl in LSurls[LS_FILES_TO_RUN,1]){
for (LSurl in LSurls[,1]){
  #print(LSurl)
  dummy = strsplit(LSurl,"/")[[1]][8]
  LSname = strsplit(dummy,"-")[[1]][1]
  #print(LSname)
  rm("dummy")

  i <- grep(LSname,csv_scenes)
  if (length(i) > 0) {

    ## Extract sensor name:
    sensor <- tile_metadata$sensor_id[i[1]]

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
                    dir("data/temp/LS_scene","L*band5.tif"),
                    sep = "")
        REDname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*band4.tif"),
                    sep = "")
        GREENname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*band3.tif"),
                    sep = "")
        LSID <- 8
    } else if (grepl("ETM",sensor)){
        NIRname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*band4.tif"),
                    sep = "")
        REDname <- paste("data/temp/LS_scene/",
                    dir("data/temp/LS_scene","L*band3.tif"),
                    sep = "")
        GREENname <- paste("data/temp/LS_scene/",
                      dir("data/temp/LS_scene","L*band2.tif"),
                      sep = "")
        LSID <- 7
    } else if (grepl("TM",sensor)){
        NIRname <- paste("data/temp/LS_scene/",
                       dir("data/temp/LS_scene","L*band4.tif"),
                       sep = "")
        REDname <- paste("data/temp/LS_scene/",
                     dir("data/temp/LS_scene","L*band3.tif"),
                     sep = "")
        GREENname <- paste("data/temp/LS_scene/",
                       dir("data/temp/LS_scene","L*band2.tif"),
                       sep = "")
        LSID <- 5
    }

    ## Extract information from tiff layers and form image stack:
    NIR = raster(NIRname)
    RED = raster(REDname)
    GREEN = raster(GREENname)
    #NRG = stack(NIR,RED,GREEN)

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
        tile_data <- raster(paste(
          "data/temp/raster_tiles/",tile_metadata$subject_id[k],".gri",sep=""))

        ## Extract brick subset:
        NRG_data <- brick(NIR,RED,GREEN,
          xmn = tile_metadata$X.tile_LL_x[k],xmx = tile_metadata$X.tile_LR_x[k],
          ymn = tile_metadata$X.tile_LL_y[k],ymx = tile_metadata$X.tile_LR_y[k],
          crs=CRS(paste("+proj=utm +units=m +zone=",tile_metadata$X.utm_zone[k],sep="")))
        ## Save tile data:
        save(tile_data,file = paste(
          "data/floating_forests_data/",tile_metadata$subject_id[k],
          ".RData",sep=""))
        ## Save Landsat brick subset:
        save(NRG_data,file = paste(
          "data/landsat_data/",tile_metadata$subject_id[k],
          ".RData",sep=""))
        remove("tile_data","NRG_data")
        ## record subject ID, sensor #, and LS filename:
        ID_MAT <- c(ID_MAT,tile_metadata$subject_id[k])
        SENSOR_MAT <- c(SENSOR_MAT,LSID)
        LSNAME_MAT <- c(LSNAME_MAT,LSname)
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

  } ## end if statement checking that >0 FF tiles correspond to scene

  ## Clean up workspace:
  rm(list = c('LSname','GREEN','GREENname','RED','REDname','NIR','NIRname','j','i','k'))
} ## end of loop through scenes in manifest file...

## Save metadata for the tiles and LS bricks saved in the data directories:
save(ID_MAT,SENSOR_MAT,LSNAME_MAT,Missing_Tiles,file = "data/META.RData")

## Remove the raseter tiles from the temp directory:
f <- list.files("data/temp/raster_tiles/", include.dirs = F, full.names = T, recursive = T)
file.remove(f)
file.remove("data/temp/raster_tiles.tar.gz")
