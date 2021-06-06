#helper functions & data generator for floating forests resnet50 transfer learning


#make input tensor
#input:path to directory of tiffs
#output: 350x350x3 array

tiff_to_array <- function(input_path, width = 350, height = 350){
 r <- brick(input_path)
 image_array <- array(0, c(width+2,
                           height+2,
                           dim(r)[3]))
 image_array[1:nrow(r), 1:ncol(r), 1:dim(r)[3]] <- as.array(r)

 return(image_array)
}

#make label tensor
#input: path to directory of tiffs
#output: yes/no label

filepath_to_label<- function(input_path){
  label <- as.character(str_extract_all(string = input_path, pattern = "yes|no"))
  #encode yes as 1 and no as 0
  label <- as.numeric(ifelse(label =="yes",1,0))
}

tiff_generator <- function(data, batch_size, img_height, img_width, n_bands) {

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
    #unlist and reorder bands to be: batch size X height x width x bands
    x_array <- aperm(array(unlist(tiffs),
                           dim=c(img_width, img_height, n_bands, length(tiffs))),
                     c(4, 1, 2, 3))

    #use filepath_to_label to parse yes/no labels from file paths
    labels <- map(data_subset, filepath_to_label)
    #unlist and reorder to be a 1 dimensional array of batch size x 1
    y_array <- aperm(array(unlist(labels),
                           dim=c(1, length(labels))),
                     c(2, 1))

    # return the batch as a list
    list(x_array, y_array)
  }

}





