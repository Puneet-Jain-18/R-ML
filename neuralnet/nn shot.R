iris=read.csv("./datafiles/shot.csv")
library(caTools)

# factorization of output variables which is of categorical type
iris$shotType <- factor(iris$shotType,levels = c('straight','cover','pull','scoop','leg','cut'),labels = c(1,2,3,4,5,6))

split <- sample.split(iris,SplitRatio = 0.8)
training <- subset(iris,split==T)
testing <- subset(iris,split ==F)
library(neuralnet)
nn<-neuralnet(shotType ~. , data=training,act.fct=softmax(),hidden=7,linear.output=F)
plot(nn)
predict <- compute(nn,testing)

print (predict$net.result)
pred<-predict$net.result
library(caret)
b<-vector()
for(row in 1:nrow(pred)) {
  max=0
  ind=0
  for(col in 1:ncol(pred)) {
    if(pred[row,col]>max)
    {
      print(ind)
      max=pred[row,col]
      ind=col
    }
  }
  b[row]=ind
}
print(b)
print(testing$Species)
print(confusionMatrix(table(testing$shotType,b)))


