library(keras)
mnist <-read.csv("./datafiles/shot.csv")

# x is a 3D array(images,width,height)
# y data is output numerical number ranging between 0 to 9
library(caTools)
library(caret)
split<- sample.split(mnist,SplitRatio=0.8)
x_train<-subset(mnist[0:52],split==T)

y_train<-subset(mnist[53],split==T)

x_test <-subset(mnist[0:52],split==F)
y_test <- subset(mnist[53],split==F)

# WE reshape array into 28X28 size image


x_train<-data.matrix(x_train, rownames.force = NA)
x_test <- data.matrix(x_test,rownames.force = NA)
# rescale

# performing one hot encoding on categorical output

y_train<-data.matrix(y_train, rownames.force = NA)
y_train <- array(y_train)
y_train=y_train-1
y_train <-to_categorical(y_train, 6)


y_test<-data.matrix(y_test, rownames.force = NA)
y_test <- array(y_test)
y_test=y_test-1


temptest <- y_test
y_test <-to_categorical(y_test, 6)


# defining model
model <-keras_model_sequential()

model %>%
  layer_dense(units=25,activation = 'relu',input_shape=c(52)) %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units = 18,activation = 'relu') %>%
  layer_dropout(rate=0.2)%>%
  layer_dense(units = 15,activation = 'relu') %>%
  layer_dropout(rate=0.1)%>%
  layer_dense(units=6,activation = 'softmax')

print(summary(model))

# next we compile model with loss function,optimizer,metrices

model %>%compile(
  loss='categorical_crossentropy',
  optimizer=optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train,y_train,
  epochs=30,batch_size=128,
  validation_split=0.2
)
plot(history)

model%>% evaluate(x_test,y_test)
model%>%predict_classes(x_test)
library(caret)
confusionMatrix(table(actual=temptest,predicted=model%>%predict_classes(x_test)))


















