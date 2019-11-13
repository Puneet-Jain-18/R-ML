library(keras)
?dataset_cifar10
cifar<-dataset_cifar10()

train_x <-cifar$train$x /255

train_y <- to_categorical(cifar$train$y , num_classes = 10)

test_x <- cifar$test$x/255

test_y <- to_categorical(cifar$test$y,num_classes = 10)

dim(train_x)

cat("No of training samples\t",dim(train_x)[[1]],"\tNo of test samples\t",dim(test_x)[[1]])

model<-keras_model_sequential()

model %>%
  #first 2-D convolution layer
  layer_conv_2d(filter=32,kernel_size = c(3,3),padding = "same",
  input_shape = c(32,32,3))%>%
  layer_activation("relu")%>%  
  #another 2-D convolution layer
  layer_conv_2d(filter=32,kernel_size = c(3,3))%>%
  layer_activation("relu") %>%
  
  #defining pooling layer to reduce dimentions
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  
  #dropout to avoid overfitting
  layer_dropout(0.25)%>%
  
  layer_conv_2d(filter=32, kernel_size = c(3,3),padding = "same")%>%
  layer_activation("relu")%>%
  layer_conv_2d(filter=32,kernel_size=c(3,3) ) %>%
  layer_activation("relu") %>%  
  layer_max_pooling_2d(pool_size=c(2,2)) %>%  
  layer_dropout(0.25) %>%
  
  #flatten the input
  layer_flatten()%>%
  
  layer_dense(512)%>%
  layer_activation("relu")%>%
  
  layer_dropout(0.5)%>%
  
  #output layer with 10 classes
  layer_dense(10)%>%
  
  #applying softmax activation at output layer to calculate cross entropy
  
  layer_activation("softmax")


opt <- optimizer_adam(lr=0.0001, decay = 1e-6)

model %>%compile(
  loss="categorical_crossentropy",
  optimizer = opt,
  metrics = "accuracy")

summary(model)
data_augmentation <- TRUE 

if(!data_augmentation) {  
  model %>% fit( train_x,train_y ,batch_size=32,
                 epochs=80,validation_data = list(test_x, test_y),
                 shuffle=TRUE)

  }else {  
  #Generating images
  
  gen_images <- image_data_generator(featurewise_center = TRUE,
                                     featurewise_std_normalization = TRUE,
                                     rotation_range = 20,
                                     width_shift_range = 0.30,
                                     height_shift_range = 0.30,
                                     horizontal_flip = TRUE  )
  #Fit image data generator internal statistics to some sample data
  gen_images %>% fit_image_data_generator(train_x)
  #Generates batches of augmented/normalized data from image data and #labels to visually see the generated images by the Model
  model %>% fit_generator(
    flow_images_from_data(train_x, train_y,gen_images,
                          batch_size=32,save_to_dir="/home/puneet/Work/R-ML/neuralnet/cnn_output"),
    steps_per_epoch=as.integer(50000/32),epochs = 80,
    validation_data = list(test_x, test_y) )
  }

