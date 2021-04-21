Script to generate training, validation, test datasets for presence-absence model. Created by HH on 2021 April 16

dg_1.r
  Read in landsat scene by scene, identify relevant tiles, and subsample landsat scene to each tile in scene. Save each subsampled image as a data stack tiff (DIM: x,y,channel), with naming convention:
  "floating_forests_deeplearning/data/landsat_scenes/temp/LS_Data_{tileID}.tif"
  
dg_2.r
  Divide data into training, validation, and test sets, with ~equal representation from each sensor in each set. Standardize data by the mean and standard deviation of the training dataset. Save each data stack with naming convention:
  "floating_forests_deeplearning/data/landsat_tiles/{TRAIN/VALID/TEST}/LS_Standardized_{tileID}.tif"
  
dg_3.r
  Convert floating forest data to binary raster using consensus threshold of 8, with naming convention:
  "floating_forests_deeplearning/data/floating_forests_data/{TRAIN/VALID/TEST}/FF_Rasterized_{tileID}.tif"
  
