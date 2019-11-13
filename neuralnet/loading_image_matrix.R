# loading multiple images
library(EBImage)
files <- list.files(path="/home/puneet/Work/R-ML/neuralnet/OriginalData/dogs-vs-cats/temp", pattern=".jpg",all.files=T, full.names=T, no.. = T)
list_of_images = lapply(files, function(image){
  print(image)
  x<- readImage(image)
  x<- EBImage::resize(x,w=200,h=100)
  return (x)
}) 
image_matrix = do.call('rbind', lapply(list_of_images, as.numeric))
mat= lapply(list_of_images,as.matrix)
mat= t(image_matrix)

dim(image_matrix)<-c(10,200,100,3)
