data("iris")

dataset <- iris

colnames(dataset)
library(caTools)

split <- sample.split(dataset,SplitRatio = 0.8)

training <- subset(dataset, split==T)
testing <- subset(dataset,split ==F)
dim(dataset)
print(sapply(dataset,class))
print(head(dataset))
print(levels(dataset$Sepal.Length))
print(prop.table(table(dataset$Species))*100)
print(summary(dataset))
dataset[1:4,4:5] 
x <- dataset[,1:4]
y <- dataset[,5]
library(caret)
boxplot(x[,1],main=" names(iris)[1]")
a=c(1,3,4,5,6,7,8,9)
plot(y)
plot(a,sin(a),type = "b",col="green")
lines(a,cos(a),col="red")
featurePlot(x=x,y=y, plot= "ellipse")
set.seed(5)
control<-trainControl(method = "cv",number = 10)
fit.lda<-train(Species~.,data = dataset,method="lda",metric = "Accuracy",trControl=control)
  