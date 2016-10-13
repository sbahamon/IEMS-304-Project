setwd("C:/Users/emg643/Downloads")
red.test = read.csv("red test.csv")
red.train = read.csv("red train.csv")
white.train = read.csv("white train.csv")
white.test = read.csv("white test.csv")

####GAM####

install.packages("gam")
install.packages("pROC")
library(gam)
library(splines)
library(pROC)

###red##
##basic model
fit.GAM.red = glm(isgood~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+bs(fixed.acidity)+bs(volatile.acidity)+bs(citric.acid)+bs(residual.sugar)+bs(chlorides)+bs(free.sulfur.dioxide)+bs(total.sulfur.dioxide)+bs(density)+bs(pH)+bs(sulphates)+bs(alcohol),data=red.train,family = binomial)
summary(fit.GAM.red)
#roc
plot.roc(red.train$isgood,fit.GAM.red$fitted.values)

#accuracy

pred.GAM.red = predict(fit.GAM.red,newdata = red.train,type="response")

testAccuracy.GAM.red = 0
c.GAM.red = 0
accuracy = 0

for (i in 1:99){
  accuracy=sum(red.train$isgood==(pred.GAM.red>(i*.01)))/nrow(red.train)
  
  if (accuracy > testAccuracy.GAM.red){
    testAccuracy.GAM.red = accuracy
    c.GAM.red = i*.01
  }
}

yhat.GAM.red = ifelse(predict(fit.GAM.red,newdata = red.test,type="response")>=c.GAM.red,1,0)
accuracy.GAM.red = sum(red.test$isgood==(yhat.GAM.red))/nrow(red.test)
accuracy.GAM.red


##step model
fit.GAM.step.red = step(fit.GAM.red,scope=~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+bs(fixed.acidity)+bs(volatile.acidity)+bs(citric.acid)+bs(residual.sugar)+bs(chlorides)+bs(free.sulfur.dioxide)+bs(total.sulfur.dioxide)+bs(density)+bs(pH)+bs(sulphates)+bs(alcohol))
summary(fit.GAM.step.red)

#roc
plot.roc(red.train$isgood,fit.GAM.step.red$fitted.values)

#accuracy

pred.GAM.step.red = predict(fit.GAM.step.red,newdata = red.train,type="response")

testAccuracy.GAM.step.red = 0
c.GAM.step.red = 0
accuracy = 0

for (i in 1:99){
  accuracy=sum(red.train$isgood==(pred.GAM.step.red>(i*.01)))/nrow(red.train)
  
  if (accuracy > testAccuracy.GAM.step.red){
    testAccuracy.GAM.step.red = accuracy
    c.GAM.step.red = i*.01
  }
}

yhat.GAM.step.red = ifelse(predict(fit.GAM.step.red,newdata = red.test,type="response")>=c.GAM.step.red,1,0)
accuracy.GAM.step.red = sum(red.test$isgood==(yhat.GAM.step.red))/nrow(red.test)
accuracy.GAM.step.red

###white##

##basic model
fit.GAM.white = glm(isgood~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+bs(fixed.acidity)+bs(volatile.acidity)+bs(citric.acid)+bs(residual.sugar)+bs(chlorides)+bs(free.sulfur.dioxide)+bs(total.sulfur.dioxide)+bs(density)+bs(pH)+bs(sulphates)+bs(alcohol),data=white.train,family = binomial)
summary(fit.GAM.white)

#roc
plot.roc(white.train$isgood,fit.GAM.white$fitted.values)

#accuracy
pred.GAM.white = predict(fit.GAM.white,newdata = white.train,type="response")

testAccuracy.GAM.white = 0
c.GAM.white = 0
accuracy = 0

for (i in 1:99){
  accuracy=sum(white.train$isgood==(pred.GAM.white>(i*.01)))/nrow(white.train)
  
  if (accuracy > testAccuracy.GAM.white){
    testAccuracy.GAM.white = accuracy
    c.GAM.white = i*.01
  }
}

yhat.GAM.white = ifelse(predict(fit.GAM.white,newdata = white.test,type="response")>=c.GAM.white,1,0)
accuracy.GAM.white = sum(white.test$isgood==(yhat.GAM.white))/nrow(white.test)
accuracy.GAM.white

##step model
fit.GAM.step.white = step(fit.GAM.white,scope=~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+bs(fixed.acidity)+bs(volatile.acidity)+bs(citric.acid)+bs(residual.sugar)+bs(chlorides)+bs(free.sulfur.dioxide)+bs(total.sulfur.dioxide)+bs(density)+bs(pH)+bs(sulphates)+bs(alcohol))
summary(fit.GAM.step.white)

#roc
plot.roc(white.train$isgood,fit.GAM.step.white$fitted.values)

#accuracy
pred.GAM.step.white = predict(fit.GAM.step.white,newdata = white.train,type="response")

testAccuracy.GAM.step.white = 0
c.GAM.step.white = 0
accuracy = 0

for (i in 1:99){
  accuracy=sum(white.train$isgood==(pred.GAM.step.white>(i*.01)))/nrow(white.train)
  
  if (accuracy > testAccuracy.GAM.step.white){
    testAccuracy.GAM.step.white = accuracy
    c.GAM.step.white = i*.01
  }
}

yhat.GAM.step.white = ifelse(predict(fit.GAM.step.white,newdata = white.test,type="response")>=c.GAM.step.white,1,0)
accuracy.GAM.step.white = sum(white.test$isgood==(yhat.GAM.step.white))/nrow(white.test)
accuracy.GAM.step.white

####Tree####
install.packages("tree")
library(tree)

###Red###

##Fit models##
fit.tree.red.big = tree(factor(isgood)~.,red.train,mindev=1e-7)
fit.tree.red.prune = prune.tree(fit.tree.red.big,best=30)
plot(fit.tree.red.big,type="uniform")
plot(fit.tree.red.prune,type="uniform")
text(fit.tree.red.prune)

##Predict##
yhat.red.big = predict(fit.tree.red.big,newdata = red.test,type="class")
yhat.red.prune = predict(fit.tree.red.prune,newdata = red.test,type="class")

##accuracy
accuracy.red.big = sum(yhat.red.big==red.test$isgood)/nrow(red.test)
accuracy.red.prune = sum(yhat.red.prune==red.test$isgood)/nrow(red.test)
accuracy.red.big
accuracy.red.prune


###White###

##Fit models##
fit.tree.white.big = tree(factor(isgood)~.,white.train,mindev=1e-7)
fit.tree.white.prune = prune.tree(fit.tree.white.big,best=30)
plot(fit.tree.white.big,type="uniform")
plot(fit.tree.white.prune,type="uniform")
text(fit.tree.white.prune)

##Predict##
yhat.white.big = predict(fit.tree.white.big,newdata = white.test,type="class")
yhat.white.prune = predict(fit.tree.white.prune,newdata = white.test,type="class")

##accuracy##
accuracy.white.big = sum(yhat.white.big==white.test$isgood)/nrow(white.test)
accuracy.white.prune = sum(yhat.white.prune==white.test$isgood)/nrow(white.test)
accuracy.white.big
accuracy.white.prune


####RF#####
install.packages("randomForest")
library(randomForest)

###Red###
fit.RF.red = randomForest(factor(isgood)~.,data=red.train,mtry = 3,importance=T)
yhat.RF.red = predict(fit.RF.red,newdata = red.test,type="class")
varImpPlot(fit.RF.red)

##accuracy##
accuracy.RF.red = sum(yhat.RF.red==red.test$isgood)/nrow(red.test)
accuracy.RF.red

###White###
fit.RF.white = randomForest(factor(isgood)~.,data=white.train,mtry = 3,importance=T)
yhat.RF.white = predict(fit.RF.white,newdata = white.test,type="class")
varImpPlot(fit.RF.white)

##accuracy##
accuracy.RF.white = sum(yhat.RF.white==white.test$isgood)/nrow(white.test)
accuracy.RF.white
