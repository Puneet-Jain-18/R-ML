dat <- read.csv("./datafiles/Salaries.csv")
dat <- dat[,-1 ]
dat <- dat[,-5]
library(caTools)
split <- sample.split(dat, SplitRatio=0.7)
sal <- ifelse(dat$salary>=100000,"high","low")
dat<- data.frame(dat,sal)
training <- subset(dat, split==T)

testing <- subset(dat, split ==F)

library(e1071)
library(caret)
naive <- naiveBayes(sal~. , data=training)
print(naive)

pred <- predict(naive,newdata = testing)

print(confusionMatrix(table(pred,testing$sal)))
