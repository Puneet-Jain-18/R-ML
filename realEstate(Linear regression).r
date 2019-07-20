library(MASS)
library(caTools)
data("Boston")
split=sample.split(Boston$medv,SplitRatio =0.8 )

training <- subset(Boston,split==TRUE)

testing <- subset(Boston,split==FALSE)

library(corrplot)
cr=cor(Boston)
model <- lm(medv~.-age -indus, training);
pre<-predict(model,testing)
plot(testing$medv,type="l",lty=1.8,col="green");
lines(pre,type="l",col="blue")
