data(iris)
library(caTools)
iris$Species <- factor(iris$Species,levels = c('setosa','versicolor','virginica'),labels = c(1,2,3))

split <- sample.split(iris,SplitRatio = 0.8)
training <- subset(iris,split==T)
testing <- subset(iris,split ==F)
library(neuralnet)
nn<-neuralnet(Species ~. , data=training,act.fct="logistic",hidden=3,linear.output=F)
plot(nn)
predict <- compute(nn,testing)

print (predict$net.result)

pred <- ifelse(predict$net.result>0.5,1,0)
library(caret)
pred[,1]
b<-vector()
for(row in 1:nrow(pred)) {
  for(col in 1:ncol(pred)) {
    if(pred[row,col]==1)
    {
      b[row]=col
      break
    }
    b[row]=1
  }
}
print(b)
print(pred)
print(confusionMatrix(iris$Species,b))
