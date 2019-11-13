library(keras)
library(stringr)
library(pbapply)
library(EBImage)

# Set image size 
width <- 50
height <- 50

extract_feature <- function(dir_path, width, height, labelsExist = T) {
  img_size <- width * height
  
  ## List images in path
  images_names <- list.files(dir_path)
  
  if(labelsExist){
    ## Select only cats or dogs images
    catdog <- str_extract(images_names, "^(cat|dog)")
    # Set cat == 0 and dog == 1
    key <- c("cat" = 0, "dog" = 1)
    y <- key[catdog]
  }
  
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale (normalized to max)
    ## Coerce to a vector (row-wise)
    img_resized <-array(as.numeric(unlist(img_resized)), dim=c( 50, 50,3))
    print(dim(img_resized))
    #img_vector <- as.vector(t(img_resized))
    return(img_resized)
  })
  
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  print(dim(feature_matrix))
  ## Set names
  if(labelsExist){
    return(list(X = feature_matrix, y = y))
  }else{
    return(feature_matrix)
  }
}

trainData <- extract_feature("/home/puneet/Work/R-ML/neuralnet/OriginalData/dogs-vs-cats/train/", width, height)
# Takes slightly less
testData <- extract_feature("/home/puneet/Work/R-ML/neuralnet/OriginalData/dogs-vs-cats/test1/", width, height, labelsExist = F)

par(mar = rep(0, 4))
testCat <- t(matrix(as.numeric(trainData$X[2,]),
                    nrow = width, ncol = height, T))
image(t(apply(testCat, 2, rev)), col = gray.colors(12),
      axes = F)  

#saving data
save(trainData, testData, file = "/home/puneet/Work/R-ML/neuralnet/OriginalData/dogs-vs-cats/catdogData.RData")  

#model construction

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
  layer_dense(1)%>%
  
  #applying softmax activation at output layer to calculate cross entropy
  
  layer_activation("sigmoid")
summary(model)

opt <- optimizer_adam(lr=0.0001, decay = 1e-6)


model %>%compile(
  loss="binary_crossentropy",
  optimizer=opt,
  metrics="accuracy"
)


