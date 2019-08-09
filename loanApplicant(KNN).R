loan <- read.csv("./datafiles/loans.csv")
loan <- loan[,-2]
loan <- na.omit(loan)
head (loan)
normalize <- function(x){
  return(x-min(x)/(max(x)-min(x)))
}

loan.n <- as.data.frame(lapply(loan[,2:13],normalize ))
head(loan.n)
loan.n <- loan.n[,-3]
loan.n <- loan.n[,-5]
loan.n <- loan.n[,-6]
loan.n <- loan.n[,-6]
loan.n <- loan.n[,-6]
loan.n <- loan.n[,-6]
library(caTools)

split <- sample.split(loan , SplitRatio = 0.7)

training <- subset(loan, split ==T )
testing <- subset (loan ,split == F)

 library(class)
NROW(testing)

knn.26 <- knn (train = training, test = testing,cl= training.n,k=54)
  


