library(keras)
mnist <-dataset_mnist()
mnist <-dataset_mnist()
# x is a 3D array(images,width,height)
# y data is output numerical number ranging between 0 to 9
x_train<-mnist$train$x
y_train<-mnist$train$y

x_test <-mnist$test$x
y_test <- mnist$test$y

# WE reshape array into 28X28 size image
x_train <-array_reshape(x_train,c(nrow(x_train),784))
x_test <-array_reshape(x_test,c(nrow(x_test),784))

# rescale
x_train <-x_train/255
x_test <-x_test/255

# performing one hot encoding on categorical output
y_train <-to_categorical(y_train, 10)
temptest <- y_test
y_test <-to_categorical(y_test, 10)


# defining model
model <-keras_model_sequential()

model %>%
  layer_dense(units=256,activation = 'relu',input_shape=c(784)) %>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units = 128,activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units=10,activation = 'softmax')

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


















