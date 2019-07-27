dibaet <- read.csv("./datafiles/diabetes.csv")
library(caTools)
split <- sample.split(dibaet , SplitRatio = 0.8);

training<- subset(dibaet,split ==TRUE);

testing <-subset(dibaet, split ==FALSE);

library(rpart)

model <- rpart(type~. , data= training )
print(model)

plot(model,margin=0.1)
text(model,use.n=TRUE,pretty=TRUE)


##Now we would make prediction

pred <- predict(model,newdata=testing,type="class")

## so we build a confusion matrix to verify with the actual data
library(caret)
print(confusionMatrix(table(actualValue= testing$type, predValue = pred)))
