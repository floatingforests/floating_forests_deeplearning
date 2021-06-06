require(glue)
require(raster)
#for (LSurl in LSurls[LS_FILES_TO_RUN,1]){ #make me parallel with a function

#one file from LSnames
make_LS_tiles <- function(one_ls_file,
                          scenes_to_be_used, #yes, it's duplicated, but, it's length matches subject_info, which is important
                          subject_info,
                          coastline,
                          out_dir = "data/temp/presence_absence/landsat_data/"){
  scene_name <- gsub("(.*)\\/(.*)(.tar.gz)", "\\2", one_ls_file)
  scene_dir <- glue("data/temp/LS_scene_{scene_name}/")

  print(glue("Processing {one_ls_file}"))

  # empty a dir we are working on
  f <- list.files(scene_dir, include.dirs = F, full.names = T, recursive = T)
  file.remove(f)

  ###########################################################
  #         Part 1: Preprocess Presence/Absence Data        #
  ###########################################################

  #unpack the file into temp
  untar(one_ls_file,
        exdir = scene_dir)

  #get the scene name
  dummy <- strsplit(dir(scene_dir,"L*.tif")[1],"_")
  LSname <- paste(dummy[[1]][1],dummy[[1]][3],dummy[[1]][4],"01T1",sep="")

  #check to make sure we have not already done this one
  i <- grep(LSname,scenes_to_be_used)

  if (length(i) > 0) {

    ## Extract sensor name:
    sensor <- subject_info$sensor_id[i[1]]


    ## Define filenames for landsat scene
    if (grepl("OLI_TIRS",sensor)) {
      NIRname <- paste(scene_dir,
                       dir(scene_dir,"L*_sr_band5.tif"),
                       sep = "")
      REDname <- paste(scene_dir,
                       dir(scene_dir,"L*_sr_band4.tif"),
                       sep = "")
      GREENname <- paste(scene_dir,
                         dir(scene_dir,"L*_sr_band3.tif"),
                         sep = "")
      CLOUDname <- paste(scene_dir,
                         dir(scene_dir,"L*_pixel_qa.tif"),
                         sep = "")
      LSID <- 8
    } else if (grepl("ETM",sensor)){
      NIRname <- paste(scene_dir,
                       dir(scene_dir,"L*_sr_band4.tif"),
                       sep = "")
      REDname <- paste(scene_dir,
                       dir(scene_dir,"L*_sr_band3.tif"),
                       sep = "")
      GREENname <- paste(scene_dir,
                         dir(scene_dir,"L*_sr_band2.tif"),
                         sep = "")
      CLOUDname <- paste(scene_dir,
                         dir(scene_dir,"L*_sr_cloud_qa.tif"),
                         sep = "")
      LSID <- 7
    } else if (grepl("TM",sensor)){
      NIRname <- paste(scene_dir,
                       dir(scene_dir,"L*_sr_band4.tif"),
                       sep = "")
      REDname <- paste(scene_dir,
                       dir(scene_dir,"L*_sr_band3.tif"),
                       sep = "")
      GREENname <- paste(scene_dir,
                         dir(scene_dir,"L*_sr_band2.tif"),
                         sep = "")
      CLOUDname <- paste(scene_dir,
                         dir(scene_dir,"L*_sr_cloud_qa.tif"),
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
    coastline_utm <- st_transform(coastline,crs(raster(NRG, layer=1)))

    ## Mask out land:
    NRG_masked_land <- mask(NRG, coastline_utm, inverse=TRUE)
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


    #above here is processing the scene
    #below here is processing subject by subject tiles
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
        out_dir,pa_merged$subject_id[k],
        ".RData",sep=""))

      ## pa is a binary indicating presence (1) or absence (2)
      ##   based on average swipe outcome:
      # pa <- round(
      #   (pa_merged$yes[k])/(pa_merged$yes[k] + pa_merged$no[k])
      # )

      ## record subject ID, sensor #, and LS filename:
      # PA_MAT <- c(PA_MAT,pa)
      # SENSOR_MAT <- c(SENSOR_MAT,LSID)
      # LSNAME_MAT <- c(LSNAME_MAT,LSname)
      # ID_MAT <- c(ID_MAT,pa_merged$subject_id[k])

     # remove(pa,NRG_data,tile_extent)

    } ## end loop through k tiles corresponding to scene

    ## Remove LS scene from temp directory:
    f <- list.files(scene_dir, include.dirs = F, full.names = T, recursive = T)
    file.remove(f)
    file.remove(scene_dir)

    #remove masks and such from memory
    remove(QA,CLOUDmask,CLOUDname,MV,MVs,
           sensor,NRG,coastline_utm,LSID,
           NRG_masked_land,NRG_masked_land_cloud,
           GREEN,GREENname,
           RED,REDname,
           NIR,NIRname)
  } ## end if statement checking that >0 FF tiles correspond to scene

#  remove(LSname,i)

}
