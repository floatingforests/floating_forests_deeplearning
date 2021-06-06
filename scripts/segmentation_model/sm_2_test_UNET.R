#sm_2_test_UNET.r
#Test file from Garrett and Henry; 2021 June 4

library(tidyverse)
library(raster)
library(keras)
library(unet)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(reticulate)

## Point to file with model weights:
MODfile <- "/users/floatingforests/GitHub/floating_forests_deeplearning/models/fit_models/ff_unet_seg.h5"

#set input directories
landsat_test_dir <- "/users/floatingforests/GitHub/floating_forests_deeplearning/data/segmentation/landsat_tiles/test"
float_test_dir <- "/users/floatingforests/GitHub/floating_forests_deeplearning/data/segmentation/floating_forests_tiles/test"

#create vectors of tiff filepaths directories, adding head to all to test the model
landsat_test_raw_names <- list.files(landsat_test_dir,
                                     recursive = T,
                                     full.names = T)

float_test_raw_names <- list.files(float_test_dir,
                                   recursive = T,
                                   full.names = T)

# Define Tiff dimensions:
## (Now batch size is just all files)
img_width <- 352 
img_height <- 352
n_bands <- 3
batch_size <- length(float_test_raw_names)

#####Helper functions
tiff_to_array <- function(input_path){
  r <- brick(input_path)
  image_array <- array(0, c(352,352,3))
  image_array[1:nrow(r),1:ncol(r),1:3] <- as.array(r)
  return(image_array)
}
filepath_to_label<- function(input_path){
  label <- as.character(str_extract_all(string = input_path, pattern = "yes|no"))
  label <- as.numeric(ifelse(label =="yes",1,0))
}
tiff_generator <- function(data_image, data_mask, batch_size, img_height, img_width, n_bands) {
  i <- 1
  function() {
    if (i  >= length(data_image)) {
      i <<- 1
      data_image <<- sample(data_image, size = length(data_image), replace = FALSE)
    }
    rows <- c(i:min(i + batch_size - 1, length(data_image)))
    i <<- i + batch_size
    data_subset <- data_image[rows]
    tiffs <- map(data_image, tiff_to_array) 
    x_array <- aperm(array(unlist(tiffs),
                           dim=c(img_width, img_height, n_bands, length(tiffs))),
                     c(4, 1, 2, 3))
    tiffs_masks <- map(data_mask, tiff_to_array)
    y_array <- aperm(array(unlist(tiffs_masks),
                           dim=c(img_width, img_height, 1, length(tiffs_masks))),
                     c(4, 1, 2, 3))
    list(x_array, y_array)
  }
}
test_gen <- tiff_generator(
  data_image = landsat_test_raw_names,
  data_mask = float_test_raw_names,
  batch_size = batch_size,
  img_width <- img_width,
  img_height <- img_height,
  n_bands <- n_bands
)
dice <- custom_metric("dice", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

## https://blogs.rstudio.com/ai/posts/2019-08-23-unet/
#model <- load_model_hdf5(MODfile, custom_objects = c(dice = dice))
#batch <- test_gen() %>% as_iterator() %>% iter_next()
#predictions <- predict(model,batch)

Yhat <- predict(model, test_gen()[[1]])
#Yhat <- predict_classes(object=model, x=test_gen()[[1]], na.value=NA) %>% as.vector()
Y <- test_gen()[[2]] %>% as.vector()

#score <- model %>% evaluate(test_gen()[[1]],test_gen()[[2]])

## Evaluate Yhat using MCC
TP <- length(intersect(grep(1,Y),grep(1,Yhat)))
TN <- length(intersect(grep(0,Y),grep(0,Yhat)))
FP <- length(intersect(grep(0,Y),grep(1,Yhat)))
FN <- length(intersect(grep(1,Y),grep(0,Yhat)))
  
MCC <- ((TP*TN) - (FP*FN))/(sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN)))