loan <- read.csv("./datafiles/loans.csv")
print(sum(is.na(loan)))
library(mice)
md.pattern(loan)
impute <-mice(loan,m=3,seed=122)
loan<-complete(impute,2)  
print(sum(is.na(loan)))

############
# data normalization
############

loan

library(BBmisc)
loan[,3:10]<-normalize(loan[,3:10],method = "standardize",range = c(0,1))

head(loan)

######################
# Factorization
######################
loan$purpose<- factor(loan$purpose,levels = c("all_other",
                                              "credit_card",
                                              "debt_consolidation", 
                                              "educational",
                                              "home_improvement",
                                              "major_purchase" ,
                                              "small_business"),order=T
                      ,labels=c(0,1,2,3,4,5,6))
print(head(loan))