library(MASS)
library(caTools)
data("Boston")
split=sample.split(Boston$medv,SplitRatio =0.8 )

training <- subset(Boston,split==TRUE)

testing <- subset(Boston,split==FALSE)

library(corrplot)
cr=cor(Boston)
model <- glm(medv~.-age -indus, training);
control<- trainControl(method ="cv",number=10)
m<- train(medv ~. -age -indus,data=training,method ="glm",trControl=control)
pre<-predict(model,testing)
p <- predict(m,testing)
plot(testing$medv,type="l",lty=1.8,col="green");
lines(pre,type="l",col="blue")
lines(p,type ="l",col="red")

