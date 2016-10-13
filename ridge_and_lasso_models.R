setwd("C:/Users/emg643/Downloads")
red.test = read.csv("red test.csv")
red.train = read.csv("red train.csv")
white.train = read.csv("white train.csv")
white.test = read.csv("white test.csv")

install.packages("MASS")
library(MASS)
install.packages("glmnet")
library(glmnet)
install.packages("pROC")
library(pROC)

####red####
x.red = model.matrix(isgood~.,red.train)[,-1]
y.red = red.train$isgood
x.red.test = model.matrix(isgood~.,red.test)[,-1]

##ridge model
red.ridge = glmnet(x.red,y.red,alpha = 0, family = 'binomial')
red.ridge.cv = cv.glmnet(x.red,y.red,alpha = 0, family = 'binomial')
red.ridge.test = predict(red.ridge,s=red.ridge.cv$lambda.min, newx = x.red)
roc(red.train$isgood, red.ridge.test)

#accuracy
pred.ridge.red = predict(red.ridge,s=red.ridge.cv$lambda.min,newdata = red.train,type="response", newx = x.red)

testAccuracy.ridge.red = 0
c.ridge.red = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(red.train$isgood==(pred.ridge.red>(i*.01)))/nrow(red.train)
  
  if (accuracy > testAccuracy.ridge.red){
    testAccuracy.ridge.red = accuracy
    c.ridge.red = i*.01
  }
}

yhat.ridge.red = ifelse(predict(red.ridge,s=red.ridge.cv$lambda.min, newdata = red.test,type="response", newx = x.red.test)>c.ridge.red,1,0)
accuracy.ridge.red = sum(red.test$isgood==(yhat.ridge.red))/nrow(red.test)
accuracy.ridge.red

##lasso model
red.lasso = glmnet(x.red,y.red,alpha = 1, family = 'binomial')
red.lasso.cv = cv.glmnet(x.red,y.red,alpha = 1, family = 'binomial')
red.lasso.test = predict(red.lasso,s=red.lasso.cv$lambda.min, newx = x.red)
roc(red.train$isgood, red.lasso.test)

#accuracy
pred.lasso.red = predict(red.lasso,s=red.lasso.cv$lambda.min,newdata = red.train,type="response", newx = x.red)

testAccuracy.lasso.red = 0
c.lasso.red = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(red.train$isgood==(pred.lasso.red>(i*.01)))/nrow(red.train)
  
  if (accuracy > testAccuracy.ridge.red){
    testAccuracy.lasso.red = accuracy
    c.lasso.red = i*.01
  }
}

yhat.lasso.red = ifelse(predict(red.lasso,s=red.lasso.cv$lambda.min, newdata = red.test,type="response", newx = x.red.test)>c.ridge.red,1,0)
accuracy.lasso.red = sum(red.test$isgood==(yhat.lasso.red))/nrow(red.test)
accuracy.lasso.red





####white####
x.white = model.matrix(isgood~.,white.train)[,-1]
y.white = white.train$isgood
x.white.test = model.matrix(isgood~.,white.test)[,-1]

##ridge model
white.ridge = glmnet(x.white,y.white,alpha = 0, family = 'binomial')
white.ridge.cv = cv.glmnet(x.white,y.white,alpha = 0, family = 'binomial')
white.ridge.test = predict(white.ridge,s=white.ridge.cv$lambda.min, newx = x.white)
roc(white.train$isgood, white.ridge.test)

#accuracy
pred.ridge.white = predict(white.ridge,s=white.ridge.cv$lambda.min,newdata = white.train,type="response", newx = x.white)

testAccuracy.ridge.white = 0
c.ridge.white = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(white.train$isgood==(pred.ridge.white>(i*.01)))/nrow(white.train)
  
  if (accuracy > testAccuracy.ridge.white){
    testAccuracy.ridge.white = accuracy
    c.ridge.white = i*.01
  }
}

yhat.ridge.white = ifelse(predict(white.ridge,s=white.ridge.cv$lambda.min, newdata = white.test,type="response", newx = x.white.test)>c.ridge.white,1,0)
accuracy.ridge.white = sum(white.test$isgood==(yhat.ridge.white))/nrow(white.test)
accuracy.ridge.white

##lasso model
white.lasso = glmnet(x.white,y.white,alpha = 1, family = 'binomial')
white.lasso.cv = cv.glmnet(x.white,y.white,alpha = 1, family = 'binomial')
white.lasso.test = predict(white.lasso,s=white.lasso.cv$lambda.min, newx = x.white)
roc(white.train$isgood, white.lasso.test)

#accuracy
pred.lasso.white = predict(white.lasso,s=white.lasso.cv$lambda.min,newdata = white.train,type="response", newx = x.white)

testAccuracy.lasso.white = 0
c.lasso.white = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(white.train$isgood==(pred.lasso.white>(i*.01)))/nrow(white.train)
  
  if (accuracy > testAccuracy.lasso.white){
    testAccuracy.lasso.white = accuracy
    c.lasso.white = i*.01
  }
}

yhat.lasso.white = ifelse(predict(white.lasso,s=white.lasso.cv$lambda.min, newdata = white.test,type="response", newx = x.white.test)>c.lasso.white,1,0)
accuracy.lasso.white = sum(white.test$isgood==(yhat.lasso.white))/nrow(white.test)
accuracy.lasso.white