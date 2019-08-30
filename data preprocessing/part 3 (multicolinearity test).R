loan <- read.csv("./../datafiles/loans.csv")
print(sum(is.na(loan)))
library(mice)
md.pattern(loan)
impute <-mice(loan,m=3,seed=122)
loan<-complete(impute,2)  
print(sum(is.na(loan)))

############
# data normalization
########

######################
# multicollinearity
#####################
pairs(loan[,3:9])
round(cor(loan[,3:9]),2)
mode <- lm(credit.policy ~ ., data=loan)
library(faraway)
print(vif(mode))

print("if VIF greater than 10 , remove that variable")

#if VIF greater than 10 , remove that variable.

