library(tidyverse)
library(terra)
library(keras)
#Code to take a directory of floating forest subject tiffs and feed them into resnet 50

#This script assumes data is organized given the following directory structure:

#data
#├── test
#│   ├── yes
#│   └── no
#├── train
#│   ├── yes
#│   └── no



#####Helper functions

#1. make input tensor (x)
#input:path to directory of tiffs
#output: 350x350x3 array

tiff_to_array <- function(input_path){
  r <- brick(input_path)
  image_array <- as.array(r)
}

#test
#test_array <- tiff_to_array("./data/presence_absence/landsat_tiles/train/no/16190277.tif")

####2. make labels tensor (y)
#input: path to directory of tiffs
#output: yes/no label

filepath_to_label<- function(input_path){
  label <- as.character(str_extract_all(string = input_path, pattern = "yes|no"))
  #encode yes as 1 and no as 0
  label <- as.numeric(ifelse(label =="Yes",1,0))
}

#test
#test_label <- filepath_to_label(input_path)


#create lists of tiffs within train/test directories

train_input_dir <- "./data/presence_absence/landsat_tiles/train"

train_raw_names <- list.files(train_input_dir,
                              recursive = T,
                              full.names = T)


test_input_dir <- "./data/presence_absence/landsat_tiles/test"

test_raw_names <- list.files(test_input_dir,
                             recursive = T,
                             full.names = T)

#generator to take batches of tiffs and create properly structured tensors
#input: vector of tiff paths, batch size
#output: a list with 2 elements.
#element 1: a tensor of inputs (aka tiffs converted to arrays) of dim(batch_size, 350, 350, 3)
#element 2: a tensor of labels of dim(batch_size, 1)


tiff_generator <- function(data,batch_size) {

  # start iterator
  i <- 1

  # return an iterator function
  function() {

    # reset iterator if already seen all data
    #6/3 update: removing batch size here - as written, it's checking the next next i because it already updates for the next batch in line 92
    if (i  >= length(data)) {
      i <<- 1
      data <<- sample(data, size = length(data), replace = FALSE)
      }

    # grab a number of rows equal to batch size
    #use i to determine the starting row #
    #when i is 1, and batch size is 5 it will take rows 1:5.
    #when i is 5, it will take rows 5:10
    #if batch size is > than remaining number of rows, just use i:nrow(data) instead
    #to confirm this behavior, set i to 79 and batch size to 5. rows will be just 79 and 80
    rows <- c(i:min(i + batch_size - 1, length(data)))

    # update to next iteration
    i <<- i + batch_size

    # create subset of tiffs to read in
    data_subset <- data[rows]

    #use tiff_to_array to read in each image in the batch
    tiffs <- map(data_subset, tiff_to_array)
    #unlist and transpose
    #this should be batch size X height x width x bands
    #future proof - don't hardcode in the number of bands, have it check the input data
    #arguments for band number, height, width
    x_array <- aperm(array(unlist(tiffs),
                           dim=c(350, 350, 3, length(tiffs))),
                     c(4, 1, 2, 3))

    #use filepath_to_label to parse yes/no labels from file paths
    labels <- map(data_subset, filepath_to_label)
    #unlist and transpose
    #batch size x 1
    y_array <- aperm(array(unlist(labels),
                           dim=c(1, length(labels))),
                     c(2, 1))

    # return the batch as a list
    list(x_array, y_array)
  }

}

#set some params that will be used for creating generators and preparing resnet50

img_width <- 350
img_height <- 350
batch_size <- 32


#set up generators
train_gen <- tiff_generator(
  data = train_raw_names,
  batch_size = batch_size
)

validation_gen <- tiff_generator(
  data = test_raw_names,
  batch_size = batch_size
)

## define the pretrained model, here: resnet50
base_model <- application_resnet50(weights = 'imagenet',
                                   #drop the classification layers
                                   include_top = FALSE,
                                   #tell it about our data
                                   input_shape = c(img_width, img_height, 3))


#lock all weights so that we only use pre-trained weights
for (layer in base_model$layers)
  layer$trainable <- FALSE

#create new classifier
predictions <- base_model$output %>%
  layer_global_average_pooling_2d(trainable = T) %>%
  layer_dense(64, trainable = T) %>%
  layer_activation("relu", trainable = T) %>%
  layer_dropout(0.4, trainable = T) %>%
  layer_dense(2, trainable=T) %>%    ## important to adapt to fit the 2 classes in the dataset!
  layer_activation("softmax", trainable=T)


#################


# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

#check that most params are non-trainable
summary(model)

###################
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.003, decay = 1e-6),
  metrics = "accuracy"
)



hist <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = as.integer(length(train_raw_names)/batch_size),
  epochs = 1,
  validation_data = validation_gen,
  validation_steps = as.integer(length(test_raw_names)/batch_size),
  verbose=2
)

# Save Prelim Model
#assumes your working directory is the top level project directory
#will overwrite by default, add overwrite = FALSE if you don't want this behavior
model %>% save_model_hdf5(paste0('./data/model_output/ff_resnet_pres_abs_', 'dat_path','.h5'))

