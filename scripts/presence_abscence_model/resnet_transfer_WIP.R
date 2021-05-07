#I more or less followed the instructions here, but adapted it for resnet50 and fixed a few issues that were likely due to keras/tf updates
#http://flovv.github.io/Logo_detection_transfer_learning_part2/

#load keras
library(keras)

### resnet Transfer-learning example

#set image and batch sizes
#starting with native image size - consider bumping this up to 500x500
img_width <- 350
img_height <- 350
batch_size <- 3

#paths relative to this file, within scripts directory
train_directory <- "./data/presence_absence_tif/landsat_tiles/train/"
test_directory <- "./data/presence_absence_tif/landsat_tiles/test/"

#set up training data generator
#these args all seemed pretty logical
train_generator <- flow_images_from_directory(train_directory, 
                                              generator = image_data_generator(),
                                              target_size = c(img_width, img_height),
                                              color_mode = "rgb",
                                              class_mode = "categorical", 
                                              batch_size = batch_size, 
                                              shuffle = TRUE,
                                              seed = 123)

validation_generator <- flow_images_from_directory(test_directory, 
                                                   generator = image_data_generator(),
                                                   target_size = c(img_width, img_height),
                                                   color_mode = "rgb", 
                                                   classes = NULL, #why this?
                                                   class_mode = "categorical",
                                                   batch_size = batch_size, 
                                                   shuffle = TRUE, 
                                                   seed = 123)


#these are the number of samples in each set
train_samples <- 4
validation_samples <- 2

###########also try a generator with augmentation ################
#augment by rotating, shifting, and flipping
datagen <- image_data_generator(
  rotation_range = 20,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  horizontal_flip = TRUE
)

#these are the same as above, only we are using our non-default data generator
train_augmented_generator <-  flow_images_from_directory(train_directory, 
                                                         generator = datagen,
                                                         target_size = c(img_width, img_height), 
                                                         color_mode = "rgb", 
                                                         classes = NULL, 
                                                         class_mode = "categorical",
                                                         batch_size = batch_size,
                                                         shuffle = TRUE,  
                                                         seed = 123)

## define the pretrained model, here: resnet50 
base_model <- application_resnet50(weights = 'imagenet',
                                   #I believe this drops the classification layers
                                   include_top = FALSE, 
                                   input_shape = c(img_width, img_height, 3))


#lock all weights so that we only use pre-trained weights
for (layer in base_model$layers)
  layer$trainable <- FALSE

predictions <- base_model$output %>% 
  layer_global_average_pooling_2d(trainable = T) %>% 
  layer_dense(64, trainable = T) %>%
  layer_activation("relu", trainable = T) %>%
  layer_dropout(0.4, trainable = T) %>%
  layer_dense(2, trainable=T) %>%    ## important to adapt to fit the 27 classes in the dataset!
  layer_activation("softmax", trainable=T)


#################


# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)



summary(model)
###################
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.003, decay = 1e-6),
  metrics = "accuracy"
)

hist <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = as.integer(train_samples/batch_size), 
  epochs = 50, 
  validation_data = validation_generator,
  validation_steps = as.integer(validation_samples/batch_size),
  verbose=2
)
###################### Train on augmented: artificially altered data #######
hist_aug <- model %>% fit_generator(
  train_augmented_generator,
  steps_per_epoch = as.integer(train_samples/batch_size), 
  epochs = 50, 
  validation_data = validation_generator,
  validation_steps = as.integer(validation_samples/batch_size),
  verbose=2
)





