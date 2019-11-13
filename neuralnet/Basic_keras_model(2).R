# loading multiple images
library(EBImage)
files <- list.files(path="/home/puneet/Work/R-ML/neuralnet/OriginalData/dogs-vs-cats/temp", pattern=".jpg",all.files=T, full.names=T, no.. = T)
list_of_images = lapply(files, function(image){
  print(image)
  x<- readImage(image)
  x<- EBImage::resize(x,w=20,h=10)
  return (x)
}) 
image_matrix = do.call('rbind', lapply(list_of_images, as.matrix))

dim(image_matrix)<-c(10,20,10,3)

ans <- c(0,0,0,0,0,1,1,1,1,1)
ans<-as.factor(ans)
ans<-to_categorical(ans)
#ans<- factor(ans)
library(keras)
model<-keras_model_sequential()

model %>% layer_conv_2d(filter=32 ,kernel_size = c(3,3),padding = "same",
                        input_shape = c(20,10,3))%>%
          layer_activation("relu")%>%
          layer_flatten()%>%
          layer_dense(200)%>%
          layer_activation('relu')%>%
          layer_dense(2)%>%
          layer_activation("softmax")
          
print(summary(model))

opt <- optimizer_adam(lr=0.0001, decay = 1e-6)


model %>%compile(
  loss="categorical_crossentropy",
  optimizer=opt,
  metrics="accuracy"
)

model %>% fit( x=image_matrix,y=ans ,epochs=80,verbose=2,shuffle=TRUE)

