data <- read.csv("./datafiles/diabetes.csv",head=TRUE,sep=",");
library(caTools)
split=sample.split(data,SplitRatio = 0.8)
training <- subset(data, split== TRUE)
testing <- subset(data ,split == FALSE)

model=glm(type~.,training,family ="binomial");

print(summary(model))

res <- predict(model,testing,type="response")
library(ROCR)
rocrPred= prediction(res,testing$type)

rocrPref=performance(rocrPred,"tpr","fpr")

plot(rocrPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
res <- predict(model,testing,type="response")
print(table(ActualValue=testing$type,PredictedValue=res>0.4));
# Accuracy = sum of right digonal / sum of all values.

