#code for model evaluation from FF presence/absence data
library(tidyverse)
library(raster)
library(keras)
library(tensorflow)
library(caret)

#set wd and source helper functions
setwd(here::here())
source("./scripts/data_generation/tiff_generator.R")


#1. load model and set params
ff_mod <- load_model_hdf5("./models/fit_models/ff_resnet_pres_abs_3_epochs.h5")

#set some params that will be used for creating our test generator
img_width <- 350
img_height <- 350
batch_size <- 32
n_bands <- 3

#2. create test generator
test_input_dir <- "./data/presence_absence/landsat_tiles/test"
test_raw_names <- list.files(test_input_dir,
                             recursive = T,
                             full.names = T)

test_gen <- tiff_generator(
  data = test_raw_names,
  batch_size = batch_size,
  img_width = img_width,
  img_height = img_height,
  n_bands = n_bands
)

#3. evaluate
#circle back to steps = how many?
evaluate(ff_mod, test_gen, steps = 2)

#4. confusion matrix
pred_ff <- ff_mod %>% predict(test_gen, batch_size = length(test_raw_names))

pred_classes <- apply(pred_ff, 1, function(x) x[1] > x[2]) %>%
  factor(levels = c("TRUE", "FALSE"), labels = c("no", "yes") )


#here there be dragons
gsub("(.*)/(no|yes)/(.*)", "\\2", test_raw_names)


confusionMatrix(data = factor(pred_classes, levels = 0:9),
                #use the labels from the batch used
                reference = factor(mnist$test$y, levels = 0:9))
#5. extract predictions
