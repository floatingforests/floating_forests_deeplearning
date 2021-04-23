Scripts to generate training, validation, test datasets for presence-absence model. Created by HH on 2021 April 16
Updated by HH on 2021 April 23

dg_1.r
  Script to read in landsat data scene-by-scene,
    identify relevant FF tiles, and subsample the
    landsat scene to each tile in the scene.
  Each subsampled image then masked for land and clouds, and
    the NIR/Red/Green image stack is saved as a .RData file
    with dimensions: (x,y,channel), with naming convention:
      "floating_forests_deeplearning/data/landsat_scenes/
        landsat_data/{tileID}.RData"
  
dg_2.r
  Divides data into training, validation, and test sets,
with ~equal representation from each sensor in each set.
  Standardizes data by the mean and standard deviation of
the training dataset. Saves each data stack with naming convention:
    "floating_forests_deeplearning/data/
      landsat_tiles/{train/valid/test}/LS_Standardized_{tileID}.tif"
    "floating_forests_deeplearning/data/
      floating_forests_tiles/{train/valid/test}/Consensus_{tileID}.tif"
  
