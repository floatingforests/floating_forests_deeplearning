# dg_2_preprocess_segmentation_data.r
# Script to generate segmentation data tiffs.
# Divides data into training, validation, and test sets,
# with ~equal representation from each sensor in each set,
# and no crossover of scenes between sets.
# Standardizes landsat data by the mean and standard deviation of
# the training dataset. Saves each data stack with naming conventions:
# "floating_forests_deeplearning/data/segmentation/
#     landsat_tiles/{train/valid/test}/{tileID}.tif"
# "floating_forests_deeplearning/data/segmentation/
#     floating_forests_tiles/{train/valid/test}/{tileID}.tif"
#
#  Henry Houskeeper; updated 1 May 2021

rm(list=ls())
set.seed(134)

require(raster)
require(gitignore)
require(sf)
require(rgdal)

dir.create("data/segmentation/floating_forests_tiles")
dir.create("data/segmentation/floating_forests_tiles/train")
dir.create("data/segmentation/floating_forests_tiles/valid")
dir.create("data/segmentation/floating_forests_tiles/test")
dir.create("data/segmentation/landsat_tiles")
dir.create("data/segmentation/landsat_tiles/train")
dir.create("data/segmentation/landsat_tiles/valid")
dir.create("data/segmentation/landsat_tiles/test")

## Make sure tile directories are empty...
f <- list.files("data/segmentation/floating_forests_tiles/", include.dirs = F, full.names = T, recursive = T)
dummy <- file.remove(f)
f <- list.files("data/segmentation/landsat_tiles/", include.dirs = F, full.names = T, recursive = T)
dummy <- file.remove(f)

gi_write_gitignore("**/temp/*")
gi_write_gitignore("**/segmentation/*")

Nm <- vector()
Rm <- vector()
Gm <- vector()
Ns <- vector()
Rs <- vector()
Gs <- vector()

#########################################
load("data/segmentation/META.RData")

LSNAMES <- unique(LSNAME_MAT)
LSIDS <- vector()
N <- vector()
for (k in 1:length(LSNAMES)) {
  i <- grep(LSNAMES[k],LSNAME_MAT)
  LSIDS[k] <- SENSOR_MAT[i[1]]
  N[k] <- length(i)
  remove(i)
}

i5 <- sample(grep(5,LSIDS),replace=FALSE)
i7 <- sample(grep(7,LSIDS),replace=FALSE)
i8 <- sample(grep(8,LSIDS),replace=FALSE)

#N[i5]

f5 <- head(cumsum(c(0,N[i5])) / sum(N[i5]),-1)
f7 <- head(cumsum(c(0,N[i7])) / sum(N[i7]),-1)
f8 <- head(cumsum(c(0,N[i8])) / sum(N[i8]),-1)

itrain_scenes <- c(i5[f5<=0.6],i7[f7<=0.6],i8[f8<=0.6])
ivalid_scenes <- c(i5[f5>0.6 & f5<=0.8],i7[f7>0.6 & f7<=0.8],i8[f8>0.6&f8<=0.8])
itest_scenes <- c(i5[f5>0.8],i7[f7>0.8],i8[f8>0.8])

#i5 <- sample(grep(5,SENSOR_MAT),replace=FALSE)
#i7 <- sample(grep(7,SENSOR_MAT),replace=FALSE)
#i8 <- sample(grep(8,SENSOR_MAT),replace=FALSE)

#n5 <- length(i5)
#n7 <- length(i7)
#n8 <- length(i8)

#break5 <- c(floor(n5*0.6),floor(n5*0.8))
#break7 <- c(floor(n7*0.6),floor(n7*0.8))
#break8 <- c(floor(n8*0.6),floor(n8*0.8))

#itrain <- c(i5[1:break5[1]],
#           i7[1:break7[1]],
#           i8[1:break8[1]])
#ivalid <- c(i5[(1+break5[1]):break5[2]],
#           i7[(1+break7[1]):break7[2]],
#           i8[(1+break8[1]):break8[2]])
#itest <- c(i5[(1+break5[2]):n5],
#          i7[(1+break7[2]):n7],
#          i8[(1+break8[2]):n8])

print(paste("Standardizing LS bands using training dataset"))
for (k in itrain_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1
    load(paste("data/temp/segmentation/landsat_data/",
               ID_MAT[l],".RData",sep=""))
    #load(paste("data/temp/segmentation/landsat_data/",ID_MAT[k],".RData",sep=""))

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

    rm(NRG_data,N,R,G)

  } ## end looping through tiles in scene
} ## end looping through scenes in training set
rm(i_tiles)

Nmean = mean(Nm,na.rm=TRUE)
Rmean = mean(Rm,na.rm=TRUE)
Gmean = mean(Gm,na.rm=TRUE)
Nstd = mean(Ns,na.rm=TRUE)
Rstd = mean(Rs,na.rm=TRUE)
Gstd = mean(Gs,na.rm=TRUE)

############################################
#      Write tiffs for training set        #
############################################

for (k in itrain_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1

    load(paste("data/temp/segmentation/floating_forests_data/Consensus_",
               ID_MAT[l],".RData",sep=""))
    ff_out <- paste("data/segmentation/floating_forests_tiles/train/",
                    ID_MAT[l],".tif",sep="")
    load(paste("data/temp/segmentation/landsat_data/",
               ID_MAT[l],".RData",sep=""))
    ls_out <- paste("data/segmentation/landsat_tiles/train/",
                    ID_MAT[l],".tif",sep="")

    NRG <- brick(
      (raster(NRG_data,layer=1)-Nmean)/Nstd,
      (raster(NRG_data,layer=2)-Rmean)/Rstd,
      (raster(NRG_data,layer=3)-Gmean)/Gstd)

    mff <- writeRaster(tile_consensus_masked_land_cloud,
                       filename=ff_out,format="GTiff",overwrite=TRUE)
    mls <- writeRaster(NRG,
                       filename=ls_out,format="GTiff",overwrite=TRUE)

    rm(NRG_data,NRG,tile_consensus_masked_land_cloud,ff_out,ls_out)

    print(paste("Writing training set, tile ",j," of ",length(i_tiles),
                " in scene ",LSNAMES[k]))
  }
  remove(i_tiles)
}

############################################
#      Write tiffs for validation set        #
############################################

for (k in ivalid_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1

    load(paste("data/temp/segmentation/floating_forests_data/Consensus_",
               ID_MAT[l],".RData",sep=""))
    ff_out <- paste("data/segmentation/floating_forests_tiles/valid/",
                    ID_MAT[l],".tif",sep="")
    load(paste("data/temp/segmentation/landsat_data/",
               ID_MAT[l],".RData",sep=""))
    ls_out <- paste("data/segmentation/landsat_tiles/valid/",
                    ID_MAT[l],".tif",sep="")

    NRG <- brick(
      (raster(NRG_data,layer=1)-Nmean)/Nstd,
      (raster(NRG_data,layer=2)-Rmean)/Rstd,
      (raster(NRG_data,layer=3)-Gmean)/Gstd)

    mff <- writeRaster(tile_consensus_masked_land_cloud,
                       filename=ff_out,format="GTiff",overwrite=TRUE)
    mls <- writeRaster(NRG,
                       filename=ls_out,format="GTiff",overwrite=TRUE)

    rm(NRG_data,NRG,tile_consensus_masked_land_cloud,ff_out,ls_out)

    print(paste("Writing validation set, tile ",j," of ",length(i_tiles),
                " in scene ",LSNAMES[k]))
  }
  remove(i_tiles)
}

############################################
#      Write tiffs for test set        #
############################################

for (k in itest_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1

    load(paste("data/temp/segmentation/floating_forests_data/Consensus_",
               ID_MAT[l],".RData",sep=""))
    ff_out <- paste("data/segmentation/floating_forests_tiles/test/",
                    ID_MAT[l],".tif",sep="")
    load(paste("data/temp/segmentation/landsat_data/",
               ID_MAT[l],".RData",sep=""))
    ls_out <- paste("data/segmentation/landsat_tiles/test/",
                    ID_MAT[l],".tif",sep="")

    NRG <- brick(
      (raster(NRG_data,layer=1)-Nmean)/Nstd,
      (raster(NRG_data,layer=2)-Rmean)/Rstd,
      (raster(NRG_data,layer=3)-Gmean)/Gstd)

    mff <- writeRaster(tile_consensus_masked_land_cloud,
                       filename=ff_out,format="GTiff",overwrite=TRUE)
    mls <- writeRaster(NRG,
                       filename=ls_out,format="GTiff",overwrite=TRUE)

    rm(NRG_data,NRG,tile_consensus_masked_land_cloud,ff_out,ls_out)

    print(paste("Writing testing set, tile ",j," of ",length(i_tiles),
                " in scene ",LSNAMES[k]))
  }
  remove(i_tiles)
}
