Scripts to generate training, validation, test datasets for presence-absence and segmentation models.
  Created by HH on 16 April 2021
  Updated by HH on 1 May 2021

dg_1_generate_segmentation_data.r
  Script to read in landsat data scene-by-scene,
    identify relevant FF tiles, and subsample the
    landsat scene to each tile in the scene.
  Each subsampled image then masked for land and clouds, and
    the NIR/Red/Green image stacks are saved as .RData files
    with dimensions: (x,y,channel), with naming convention:
      floating_forests_deeplearning/data/temp/
        segmentation/landsat_data/{tileID}.RData
  Floating forest tiles converted to binary using consensus
    and saved as .RData files with naming convention:
      "floating_forests_deeplearning/data/temp/
        segmentation/floating_forests_data/Consensus_{tileID}.RData"
  (Henry Houskeeper; updated 1 May 2021)

dg_2_preprocess_segmentation_data.r
  Script to generate segmentation data tiffs.
  Divides data into training, validation, and test sets,
    with ~equal representation from each sensor in each set,
    and no crossover of scenes between sets.
  Standardizes landsat data by the mean and standard deviation of
    the training dataset. Saves each data stack with naming conventions:
      "floating_forests_deeplearning/data/segmentation/
        landsat_tiles/{train/valid/test}/{tileID}.tif"
      "floating_forests_deeplearning/data/segmentation/
        floating_forests_tiles/{train/valid/test}/{tileID}.tif"
  (Henry Houskeeper; updated 1 May 2021)

dg_3_generate_presence_absence_data.r
  Script to read in landsat data scene-by-scene,
    identify relevant FF subjects, and subsample the
    landsat scene to each tile for presence/absence subjects.
  Each subsampled image then masked for land and clouds, and
    the NIR/Red/Green image stacks are saved as .RData files
    with dimensions: (x,y,channel), with naming convention:
      "floating_forests_deeplearning/data/temp/
        presence_absence/landsat_data/{tileID}.RData"
  (Henry Houskeeper; updated 1 May 2021)

dg_4_proprocess_presence_absence_data.r
  Script to generate presence/absence datasets.
  Divides data into training, validation, and test sets,
    with ~equal representation from each sensor in each set,
    and no crossover between scenes for each set.
  Standardizes landsat data by the mean and standard deviation of
    the training dataset. Saves each data stack with naming convention:
      "floating_forests_deeplearning/data/presence_absence/
        landsat_tiles/{train/valid/test}/{yes/no}/{tileID}.tif"
  (Henry Houskeeper; updated 1 May 2021)
