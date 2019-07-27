dibaet <- read.csv("./datafiles/diabetes.csv")
library(caTools)
split= sample.split(dibaet,SplitRatio=0.7)

training <- subset(dibaet, split==TRUE)

testing <- subset(dibaet,split==FALSE)

library(randomForest)
training$type <- as.factor(training$type)

bestMtr <-tuneRF(training,training$type,stepFactor = 1.12,improve = 0.01,trace =T,plot = T)

forest <- randomForest(type~.,data= training )
varImpPlot(forest)

pred <- predict(forest,newdata = testing,type = "class")


library(caret)

print(confusionMatrix(table(pred,testing$type)))
