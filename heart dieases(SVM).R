library(caret)

heart <- read.csv("./datafiles/Heart.csv",sep=",",header = F)

library(caTools)

heart <- na.omit(heart)

split <- sample.split(heart,SplitRatio = 0.8)

training <- subset(heart, split ==T)
testing <- subset(heart ,split==F)

anyNA(heart)
summary(heart)
trctrl= trainControl(method = "repeatedcv", number =10,repeats = 3 )
svmLinear <- train(V15 ~. , data =training,method = "svmLinear",
                   trControl = trctrl,
                   preProcess= c("center","scale"),tuneLength=10)
