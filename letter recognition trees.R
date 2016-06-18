letters<-read.csv("letters_ABPR.csv")

letters$isB = as.factor(letters$letter == "B")


set.seed(1000)
library(caTools)

spl = sample.split(letters$isB,SplitRatio=0.5)

test<-subset(letters,spl!=TRUE)

table(letters$isB)

2350/nrow(letters)

CARTb = rpart(isB ~ . - letter, data=train, method="class")

pred<-predict(CARTb,newdata=test,type="class")

table(test$isB,pred)
(1144+328)/nrow(test)

pred


set.seed(100)

library("randomForest")
set.seed(1000)
rf<-randomForest(isB ~ .-letter,data=train)

pred<-predict(rf,newdata=test)

table(test$isB,pred)

Predicting letters:
  
  CART

  letters$letter = as.factor( letters$letter )  
set.seed(2000)
split=sample.split(letters$letter,SplitRatio=0.5)
trainm<-subset(letters,split=TRUE)
testm<-subset(letters,split=FALSE)

table(testm$letter)
803/nrow(testm)



cartf<-rpart(letter ~.-isB,data=trainm,method="class")
predcartf<-predict(cartf,newdata=testm,type="class")

table(testm$letter,predcartf)

(703+596+732+701)/nrow(testm)


RANDOM FOREST:
  library(randomForest)
set.seed(1000)
random<-randomForest(letter~.-isB,data=trainm)
predrandom<-predict(random,newdata=testm)
table(testm$letter,predrandom)
