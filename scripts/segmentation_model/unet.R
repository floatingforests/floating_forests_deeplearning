library(tidyverse)
library(raster)
library(keras)
library(unet)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)

setwd(here::here())
#Code to take a directory of floating forest subject tiffs and feed them into resnet 50

#This script assumes data is organized given the following directory structure:




#####Helper functions

#1. make input tensor (x)
#input:path to directory of tiffs
#output: 350x350x3 array

tiff_to_array <- function(input_path){
  r <- brick(input_path)
  image_array <- array(0, c(352,352,3))
  image_array[1:nrow(r),1:ncol(r),1:3] <- as.array(r)
  return(image_array)
}
####2. make labels tensor (y)
#input: path to directory of tiffs
#output: yes/no label

filepath_to_label<- function(input_path){
  label <- as.character(str_extract_all(string = input_path, pattern = "yes|no"))
  #encode yes as 1 and no as 0
  label <- as.numeric(ifelse(label =="yes",1,0))
}

#set some params that will be used for creating generators
img_width <- 352
img_height <- 352
batch_size <- 32
n_bands <- 3

#set up input and test data

#set input directories
landsat_train_dir <- "./data/segmentation/landsat_tiles/train" #image, training data
landsat_test_dir <- './data/segmentation/landsat_tiles/test'
landsat_val_dir <- './data/segmentation/landsat_tiles/valid'
float_train_dir <- "./data/segmentation/floating_forests_tiles/train" #mask
float_test_dir <- './data/segmentation/floating_forests_tiles/test'
float_val_dir <- './data/segmentation/floating_forests_tiles/valid'


#create vectors of tiff filepaths directories, adding head to all to test the model
landsat_train_raw_names <- list.files(landsat_train_dir,
                                      recursive = T,
                                      full.names = T)
landsat_test_raw_names <- list.files(landsat_test_dir,
                                     recursive = T,
                                     full.names = T)
landsat_val_raw_names <- list.files(landsat_val_dir,
                                    recursive = T,
                                    full.names = T)

float_train_raw_names <- list.files(float_train_dir,
                                    recursive = T,
                                    full.names = T)
float_test_raw_names <- list.files(float_test_dir,
                                   recursive = T,
                                   full.names = T)
float_val_raw_names <- list.files(float_val_dir,
                                  recursive = T,
                                  full.names = T)


#generator to take batches of tiffs and create properly structured tensors
#input: vector of tiff paths, batch size
#output: a list with 2 elements.
#element 1: a tensor of inputs (aka tiffs converted to arrays) of dim(batch_size, 350, 350, 3)
#element 2: a tensor of labels of dim(batch_size, 1)


tiff_generator <- function(data_image, data_mask, batch_size, img_height, img_width, n_bands) {

  # start iterator
  i <- 1

  # return an iterator function
  function() {

    # reset iterator if already seen all data
    #6/3 update: removing batch size here - as written, it's checking the next next i because it already updates for the next batch in line 92
    if (i  >= length(data_image)) {
      i <<- 1
      data_image <<- sample(data_image, size = length(data_image), replace = FALSE)
    }

    # grab a number of rows equal to batch size
    #use i to determine the starting row #
    #when i is 1, and batch size is 5 it will take rows 1:5.
    #when i is 5, it will take rows 5:10
    #if batch size is > than remaining number of rows, just use i:nrow(data) instead
    #to confirm this behavior, set i to 79 and batch size to 5. rows will be just 79 and 80
    rows <- c(i:min(i + batch_size - 1, length(data_image)))

    # update to next iteration
    i <<- i + batch_size

    # create subset of tiffs to read in
    data_subset <- data_image[rows]

    #use tiff_to_array to read in each image in the batch
    tiffs <- map(data_image, tiff_to_array)
    #unlist and reorder bands to be: batch size X height x width x bands
    x_array <- aperm(array(unlist(tiffs),
                           dim=c(img_width, img_height, n_bands, length(tiffs))),
                     c(4, 1, 2, 3))

    #use masks
    #use tiff_to_array to read in each image in the batch
    tiffs_masks <- map(data_mask, tiff_to_array)
    #unlist and reorder bands to be: batch size X height x width x bands
    y_array <- aperm(array(unlist(tiffs_masks),
                           dim=c(img_width, img_height, 1, length(tiffs_masks))),
                     c(4, 1, 2, 3))

    list(x_array, y_array)
  }

}

#set up generators
train_gen <- tiff_generator(
  data_image = landsat_train_raw_names,
  data_mask = float_train_raw_names,
  batch_size = batch_size,
  img_width <- img_width,
  img_height <- img_height,
  n_bands <- n_bands
)

validation_gen <- tiff_generator(
  data_image = landsat_val_raw_names,
  data_mask = float_val_raw_names,
  batch_size = batch_size,
  img_width <- img_width,
  img_height <- img_height,
  n_bands <- n_bands
)

test_gen <- tiff_generator(
  data_image = landsat_test_raw_names,
  data_mask = float_test_raw_names,
  batch_size = batch_size,
  img_width <- img_width,
  img_height <- img_height,
  n_bands <- n_bands
)


# add gen to unet and hope it works

model <- unet(input_shape = c(img_width, img_height, n_bands)) #can't be 350x350, but be divisible by 32
#need images to be a multiple of 32, add NA to make it 352x352?

summary(model)

dice <- custom_metric("dice", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-5),
  loss = "binary_crossentropy",
  metrics = list(dice, metric_binary_accuracy)
)

model %>% fit(
  train_gen,
  steps_per_epoch = as.integer(length(landsat_val_raw_names)/batch_size),
  epochs = 10,
  validation_data = validation_gen,
  validation_steps = as.integer(length(float_val_raw_names)/batch_size)
)

# Save Prelim Model
#assumes your working directory is the top level project directory
#will overwrite by default, add overwrite = FALSE if you don't want this behavior
model %>% save_model_hdf5('./models/model_weights/ff_unet_seg.h5')
