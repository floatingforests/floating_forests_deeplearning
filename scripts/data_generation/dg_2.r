# dg_2.r
# Divides data into training, validation, and test sets,
# with ~equal representation from each sensor in each set.
# Standardizes data by the mean and standard deviation of
# the training dataset. Saves each data stack with naming convention:
# "floating_forests_deeplearning/data/
#     landsat_tiles/{TRAIN/VALID/TEST}/LS_Standardized_{tileID}.tif"
# "floating_forests_deeplearning/data/
#     floating_forests_tiles/{TRAIN/VALID/TEST}/Consensus_{tileID}.tif"

rm(list=ls())
set.seed(137)

Nm <- vector()
Rm <- vector()
Gm <- vector()
Ns <- vector()
Rs <- vector()
Gs <- vector()

require(raster)
require(gitignore)
require(sf)
require(rgdal)

gi_write_gitignore("**/temp/*")
gi_write_gitignore("**/floating_forests_tiles/*")
gi_write_gitignore("**/landsat_tiles/*")


#########################################
load("data/META.RData")

i5 <- sample(grep(5,SENSOR_MAT),replace=FALSE)
i7 <- sample(grep(7,SENSOR_MAT),replace=FALSE)
i8 <- sample(grep(8,SENSOR_MAT),replace=FALSE)

n5 <- length(i5)
n7 <- length(i7)
n8 <- length(i8)

break5 <- c(floor(n5*0.6),floor(n5*0.8))
break7 <- c(floor(n7*0.6),floor(n7*0.8))
break8 <- c(floor(n8*0.6),floor(n8*0.8))

itrain <- c(i5[1:break5[1]],
           i7[1:break7[1]],
           i8[1:break8[1]])
ivalid <- c(i5[(1+break5[1]):break5[2]],
           i7[(1+break7[1]):break7[2]],
           i8[(1+break8[1]):break8[2]])
itest <- c(i5[(1+break5[2]):n5],
          i7[(1+break7[2]):n7],
          i8[(1+break8[2]):n8])

j <- 0
for (k in itrain){
  j <- j+1
  load(paste("data/temp/landsat_data/",ID_MAT[k],".RData",sep=""))

  ### Commented out bc masking now occurs in dg_1.r:
  ## Reproject shapefile into tile space:
  #FALK_LAND_UTM <- st_transform(FALK_LAND,
  #                             crs(raster(NRG_data, layer=1)))
  ## Mask out land:
  #NRG_data_masked <- mask(NRG_data, FALK_LAND_UTM, inverse=TRUE)

  ## Extract data from each layer as matrices:
  N <- as.matrix(raster(NRG_data, layer=1))
  R <- as.matrix(raster(NRG_data, layer=2))
  G <- as.matrix(raster(NRG_data, layer=3))

  Nm = c(Nm,mean(N,na.rm=TRUE))
  Rm = c(Rm,mean(R,na.rm=TRUE))
  Gm = c(Gm,mean(G,na.rm=TRUE))

  Ns = c(Ns,sd(N,na.rm=TRUE))
  Rs = c(Rs,sd(R,na.rm=TRUE))
  Gs = c(Gs,sd(G,na.rm=TRUE))

  rm(NRG_data,NRG_data_masked,FALK_LAND_UTM)
  print(paste("Standardizing LS bands, tile ",j," of ",length(itrain)))
}

Nmean = mean(Nm,na.rm=TRUE)
Rmean = mean(Rm,na.rm=TRUE)
Gmean = mean(Gm,na.rm=TRUE)
Nstd = mean(Ns,na.rm=TRUE)
Rstd = mean(Rs,na.rm=TRUE)
Gstd = mean(Gs,na.rm=TRUE)

j <- 0
for (k in itrain){
  j <- j+1
  load(paste("data/temp/floating_forests_data/Consensus_",ID_MAT[k],".RData",sep=""))
  ff_out <- paste("data/floating_forests_tiles/train/",ID_MAT[k],".tif",sep="")
  load(paste("data/temp/landsat_data/",ID_MAT[k],".RData",sep=""))
  ls_out <- paste("data/landsat_tiles/train/",ID_MAT[k],".tif",sep="")

  NRG <- brick(
    (raster(NRG_data,layer=1)-Nmean)/Nstd,
    (raster(NRG_data,layer=1)-Nmean)/Nstd,
    (raster(NRG_data,layer=1)-Nmean)/Nstd)

  mff <- writeRaster(tile_consensus_masked_land_cloud,filename=ff_out,format="GTiff",overwrite=TRUE)
  mls <- writeRaster(NRG,filename=ls_out,format="GTiff",overwrite=TRUE)

  rm(NRG_data,tile_consensus_masked_land_cloud,ff_out,ls_out)

  print(paste("Writing training set, tile ",j," of ",length(itrain)))
}

j <- 0
for (k in ivalid){
  j <- j+1
  load(paste("data/temp/floating_forests_data/Consensus_",ID_MAT[k],".RData",sep=""))
  ff_out <- paste("data/floating_forests_tiles/valid/",ID_MAT[k],".tif",sep="")
  load(paste("data/temp/landsat_data/",ID_MAT[k],".RData",sep=""))
  ls_out <- paste("data/landsat_tiles/valid/",ID_MAT[k],".tif",sep="")

  NRG <- brick(
    (raster(NRG_data,layer=1)-Nmean)/Nstd,
    (raster(NRG_data,layer=1)-Nmean)/Nstd,
    (raster(NRG_data,layer=1)-Nmean)/Nstd)

  mff <- writeRaster(tile_consensus_masked_land_cloud,filename=ff_out,format="GTiff",overwrite=TRUE)
  mls <- writeRaster(NRG,filename=ls_out,format="GTiff",overwrite=TRUE)

  rm(NRG_data,tile_consensus_masked_land_cloud,ff_out,ls_out)

  print(paste("Writing validation set, tile ",j," of ",length(itrain)))
}

j <- 0
for (k in itest){
  j <- j+1
  load(paste("data/temp/floating_forests_data/Consensus_",ID_MAT[k],".RData",sep=""))
  ff_out <- paste("data/floating_forests_tiles/test/",ID_MAT[k],".tif",sep="")
  load(paste("data/temp/landsat_data/",ID_MAT[k],".RData",sep=""))
  ls_out <- paste("data/landsat_tiles/test/",ID_MAT[k],".tif",sep="")

  NRG <- brick(
    (raster(NRG_data,layer=1)-Nmean)/Nstd,
    (raster(NRG_data,layer=1)-Nmean)/Nstd,
    (raster(NRG_data,layer=1)-Nmean)/Nstd)

  mff <- writeRaster(tile_consensus_masked_land_cloud,filename=ff_out,format="GTiff",overwrite=TRUE)
  mls <- writeRaster(NRG,filename=ls_out,format="GTiff",overwrite=TRUE)

  rm(NRG_data,tile_consensus_masked_land_cloud,ff_out,ls_out)

  print(paste("Writing testing set, tile ",j," of ",length(itrain)))
}


