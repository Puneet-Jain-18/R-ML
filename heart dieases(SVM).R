library(caret)

heart <- read.csv("./datafiles/Heart.csv",sep=",",header = F)

library(caTools)

heart <- na.omit(heart)

split <- sample.split(heart,SplitRatio = 0.8)

training <- subset(heart, split ==T)
training <- na.omit(training)
testing <- subset(heart ,split==F)
testing <- na.omit(testing)
anyNA(heart)
summary(heart)
trctrl= trainControl(method = "repeatedcv", number =10,repeats = 3 )
svmLinear <- train(V15 ~. , data =heart,method = "svmLinear",
                   trControl = trctrl,
                   preProcess= c("center","scale"),tuneLength=10)

pred <- predict(svmLinear,newdata = testing )
print(confusionMatrix(table(pred,testing$V15)))
