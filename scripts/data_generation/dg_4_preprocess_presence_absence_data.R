# dg_4_proprocess_presence_absence_data.r
# Script to generate presence/absence datasets.
# Divides data into training, validation, and test sets,
# with ~equal representation from each sensor in each set,
# and no crossover between scenes for each set.
# Standardizes landsat data by the mean and standard deviation of
# the training dataset. Saves each data stack with naming convention:
# "floating_forests_deeplearning/data/presence_absence/
#     landsat_tiles/{train/valid/test}/{yes/no}/{tileID}.tif"
#
#  Henry Houskeeper; updated 1 May 2021

rm(list=ls())
set.seed(137)

require(raster)
require(gitignore)
require(sf)
require(rgdal)

dir.create("data/presence_absence")
dir.create("data/presence_absence/landsat_tiles")
dir.create("data/presence_absence/landsat_tiles/train")
dir.create("data/presence_absence/landsat_tiles/train/yes")
dir.create("data/presence_absence/landsat_tiles/train/no")
dir.create("data/presence_absence/landsat_tiles/valid")
dir.create("data/presence_absence/landsat_tiles/valid/yes")
dir.create("data/presence_absence/landsat_tiles/valid/no")
dir.create("data/presence_absence/landsat_tiles/test")
dir.create("data/presence_absence/landsat_tiles/test/yes")
dir.create("data/presence_absence/landsat_tiles/test/no")

## Make sure tile directories are empty...
f <- list.files("data/presence_absence/landsat_tiles/", include.dirs = F, full.names = T, recursive = T)
dummy <- file.remove(f)
remove(f,dummy)

gi_write_gitignore("**/temp/*")
gi_write_gitignore("**/presence_absence/*")

#########################################
load("data/presence_absence/META.RData")

LSNAMES <- unique(LSNAME_MAT)
LSIDS <- vector()
N <- vector()

for (k in 1:length(LSNAMES)) {
  i <- grep(LSNAMES[k],LSNAME_MAT)
  LSIDS[k] <- SENSOR_MAT[i[1]]
  N[k] <- length(i)
  remove(i)
}

## I added an if statement to only separate sensors IF
##   there are more than 30 scenes with data:
if (length(LSNAMES) > 30) {
  i5 <- sample(grep(5,LSIDS),replace=FALSE)
  i7 <- sample(grep(7,LSIDS),replace=FALSE)
  i8 <- sample(grep(8,LSIDS),replace=FALSE)

  f5 <- head(cumsum(c(0,N[i5])) / sum(N[i5]),-1)
  f7 <- head(cumsum(c(0,N[i7])) / sum(N[i7]),-1)
  f8 <- head(cumsum(c(0,N[i8])) / sum(N[i8]),-1)

  itrain_scenes <- c(i5[f5<=0.6],i7[f7<=0.6],i8[f8<=0.6])
  ivalid_scenes <- c(i5[f5>0.6 & f5<=0.8],i7[f7>0.6 & f7<=0.8],i8[f8>0.6&f8<=0.8])
  itest_scenes <- c(i5[f5>0.8],i7[f7>0.8],i8[f8>0.8])

  remove(f5,f7,f8,i5,i7,i8,N)

} else {

  ## If less than or e1ual to 20 scenes, ignore sensor component:
  i <- sample(c(1:length(LSNAMES)),replace=FALSE)
  f <- head(cumsum(c(0,N[i])) / sum(N[i]),-1)
  itrain_scenes <- i[f<=0.6]
  ivalid_scenes <- i[f>0.6 & f<=0.8]
  itest_scenes <- i[f>0.8]

  remove(f,i,N)

}

remove(LSIDS)

############################################
#      Normalize using training set        #
############################################
#
#print(paste("Standardizing LS bands using training dataset"))
#for (k in itrain_scenes) {
#  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
#  j <- 0
#  for (l in i_tiles){
#    j <- j+1
#    load(paste("data/temp/presence_absence/landsat_data/",
#               ID_MAT[l],".RData",sep=""))
#
#    ## Extract data from each layer as matrices:
#    N <- as.matrix(raster(NRG_data, layer=1))
#    R <- as.matrix(raster(NRG_data, layer=2))
#    G <- as.matrix(raster(NRG_data, layer=3))
#
#    Nm = c(Nm,mean(N,na.rm=TRUE))
#    Rm = c(Rm,mean(R,na.rm=TRUE))
#    Gm = c(Gm,mean(G,na.rm=TRUE))
#
#    Ns = c(Ns,sd(N,na.rm=TRUE))
#    Rs = c(Rs,sd(R,na.rm=TRUE))
#    Gs = c(Gs,sd(G,na.rm=TRUE))
#
#    rm(NRG_data,N,R,G)
#
#  } ## end looping through tiles in scene
#} ## end looping through scenes in training set
#rm(i_tiles)
#
#Nmean = mean(Nm,na.rm=TRUE)
#Rmean = mean(Rm,na.rm=TRUE)
#Gmean = mean(Gm,na.rm=TRUE)
#Nstd = mean(Ns,na.rm=TRUE)
#Rstd = mean(Rs,na.rm=TRUE)
#Gstd = mean(Gs,na.rm=TRUE)


############################################
#      Write tiffs for training set        #
############################################

for (k in itrain_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1

    ## Load in NRG_data:
    load(paste(
      "data/temp/presence_absence/landsat_data/",ID_MAT[l],
      ".RData",sep=""))
    if (PA_MAT[l] == 1) {
      ls_out <- paste(
        "data/presence_absence/landsat_tiles/train/yes/",
        ID_MAT[l],".tif",sep="")
      } else {
      ls_out <- paste(
        "data/presence_absence/landsat_tiles/train/no/",
        ID_MAT[l],".tif",sep="")
    }

    ## Skipping normalization in this version...
    #NRG <- brick(
    #  (raster(NRG_data,layer=1)-Nmean)/Nstd,
    #  (raster(NRG_data,layer=2)-Rmean)/Rstd,
    #  (raster(NRG_data,layer=3)-Gmean)/Gstd)

    #mls <- writeRaster(NRG,filename=ls_out,format="GTiff",overwrite=TRUE)
    mls <- writeRaster(NRG_data,filename=ls_out,format="GTiff",overwrite=TRUE)

    #rm(NRG_data,NRG,ls_out)
    rm(NRG_data,ls_out)

    print(paste("Writing training set, tile ",j," of ",length(i_tiles),
              " in scene ",LSNAMES[k]))
  }
  remove(i_tiles,l,j)
}
remove(k)


############################################
#      Write tiffs for validation set      #
############################################

for (k in ivalid_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1

    load(paste(
      "data/temp/presence_absence/landsat_data/",ID_MAT[l],
      ".RData",sep=""))
    if (PA_MAT[l] == 1) {
      ls_out <- paste(
        "data/presence_absence/landsat_tiles/valid/yes/",
        ID_MAT[l],".tif",sep="")
    } else {
      ls_out <- paste(
        "data/presence_absence/landsat_tiles/valid/no/",
        ID_MAT[l],".tif",sep="")
    }

    ## Skipping normalization in this version...
    #NRG <- brick(
    #  (raster(NRG_data,layer=1)-Nmean)/Nstd,
    #  (raster(NRG_data,layer=2)-Rmean)/Rstd,
    #  (raster(NRG_data,layer=3)-Gmean)/Gstd)

    #mls <- writeRaster(NRG,filename=ls_out,format="GTiff",overwrite=TRUE)
    mls <- writeRaster(NRG_data,filename=ls_out,format="GTiff",overwrite=TRUE)

    #rm(NRG_data,NRG,ls_out)
    rm(NRG_data,ls_out)

    print(paste("Writing validation set, tile ",j," of ",length(i_tiles),
                " in scene ",LSNAMES[k]))
  }
  remove(i_tiles,l,j)
}
remove(k)

############################################
#         Write tiffs for test set         #
############################################
for (k in itest_scenes) {
  i_tiles <- grep(LSNAMES[k],LSNAME_MAT)
  j <- 0
  for (l in i_tiles){
    j <- j+1

    load(paste(
      "data/temp/presence_absence/landsat_data/",ID_MAT[l],
      ".RData",sep=""))
    if (PA_MAT[l] == 1) {
      ls_out <- paste(
        "data/presence_absence/landsat_tiles/test/yes/",
        ID_MAT[l],".tif",sep="")
    } else {
      ls_out <- paste(
        "data/presence_absence/landsat_tiles/test/no/",
        ID_MAT[l],".tif",sep="")
    }

    ## Skipping normalization in this version...
    #NRG <- brick(
    #  (raster(NRG_data,layer=1)-Nmean)/Nstd,
    #  (raster(NRG_data,layer=2)-Rmean)/Rstd,
    #  (raster(NRG_data,layer=3)-Gmean)/Gstd)

    #mls <- writeRaster(NRG,filename=ls_out,format="GTiff",overwrite=TRUE)
    mls <- writeRaster(NRG_data,filename=ls_out,format="GTiff",overwrite=TRUE)

    #rm(NRG_data,NRG,ls_out)
    rm(NRG_data,ls_out)

    print(paste("Writing testing set, tile ",j," of ",length(i_tiles),
                " in scene ",LSNAMES[k]))
  }
  remove(i_tiles,l,j)
}
remove(k)

