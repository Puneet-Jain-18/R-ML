#Link of reference
#https://www.kaggle.com/rtatman/beginner-s-intro-to-rnn-s-in-r

library(keras)
library(tidyverse)
library(caret)
data<- read.csv("./seattleWeather.csv")

#setting some global parameteres
max_len <- 6  # no of previous examples to look while performing current prediction

batch_size <- 32 # no of sequence to look during training 

total_epochs <- 15 
set.seed(123)

rain <-data$RAIN
print(table(rain))

print (summary(data))

# chopping data into subsamples(overlapping) of max_len+1 size(+1 is the prediction)


start_indexes <- seq(1, length(rain) - (max_len + 1), by = 3)

# create an empty matrix to store our data in
weather_matrix <- matrix(nrow = length(start_indexes), ncol = max_len + 1)

# fill our matrix with the overlapping slices of our dataset
for (i in 1:length(start_indexes)){
  weather_matrix[i,] <- rain[start_indexes[i]:(start_indexes[i] + max_len)]
}

weather_matrix <- na.omit(weather_matrix)

# to make sure everything is numeric

weather_matrix <- weather_matrix*1

# now we need to split data such that first max_len days are input and last one is output
# input matrix

x <- weather_matrix[,-ncol(weather_matrix)]
# output matrix 
y <- weather_matrix[,ncol(weather_matrix)]

#Splitting the data
training_index <- createDataPartition(y, p = .9, 
                                      list = FALSE, 
                                      times = 1)

# training data
X_train <- array(x[training_index,], dim = c(length(training_index), max_len, 1))
y_train <- y[training_index]

# testing data
X_test <- array(x[-training_index,], dim = c(length(y) - length(training_index), max_len, 1))
y_test <- y[-training_index]


# model construction

model<- keras_model_sequential()

model %>%
  layer_dense(input_shape = dim(X_train)[2:3],units = max_len)
# adding rnn layer with 6 neurons
model %>%
  layer_simple_rnn(units = 6)
model %>%
  layer_dense(units = 1, activation = "sigmoid")
summary(model)

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = 'RMSprop', 
                  metrics = c('accuracy'))


trained_model <- model%>%fit(
                x= X_train,
                y= y_train,
                batch_size= batch_size,
                epochs= total_epochs,
                validation_split= 0.1
)


plot(trained_model)

pred <- model %>% predict_classes(X_test)
print(confusionMatrix(table(pred,y_test)))
