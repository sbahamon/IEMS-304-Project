####GAM####

install.packages("gam")
install.packages("pROC")
library(gam)
library(splines)
library(pROC)

###red##
##basic model
fit2.GAM.red = glm(isgood~volatile.acidity+total.sulfur.dioxide+density+sulphates+alcohol+bs(volatile.acidity)+bs(total.sulfur.dioxide)+bs(density)+bs(sulphates)+bs(alcohol),data=red.train,family = binomial)
summary(fit2.GAM.red)
#roc
plot.roc(red.train$isgood,fit2.GAM.red$fitted.values)

#accuracy

pred2.GAM.red = predict(fit2.GAM.red,newdata = red.train,type="response")

testAccuracy2.GAM.red = 0
c2.GAM.red = 0
accuracy = 0

for (i in 1:99){
  accuracy=sum(red.train$isgood==(pred2.GAM.red>(i*.01)))/nrow(red.train)
  
  if (accuracy > testAccuracy2.GAM.red){
    testAccuracy2.GAM.red = accuracy
    c2.GAM.red = i*.01
  }
}

yhat2.GAM.red = ifelse(predict(fit2.GAM.red,newdata = red.test,type="response")>=c2.GAM.red,1,0)
accuracy2.GAM.red = sum(red.test$isgood==(yhat2.GAM.red))/nrow(red.test)
accuracy2.GAM.red

###White###

##basic model
fit2.GAM.white = glm(isgood~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+alcohol+bs(fixed.acidity)+bs(volatile.acidity)+bs(citric.acid)+bs(residual.sugar)+bs(chlorides)+bs(free.sulfur.dioxide)+bs(total.sulfur.dioxide)+bs(density)+bs(pH)+bs(sulphates)+bs(alcohol),data=white.train,family = binomial)
#fit2.GAM.white = glm(isgood~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+alcohol+bs(volatile.acidity)+bs(residual.sugar)+bs(free.sulfur.dioxide)+bs(density)+bs(alcohol),data=white.train,family = binomial)

summary(fit2.GAM.white)

#roc
plot.roc(white.train$isgood,fit2.GAM.white$fitted.values)

#accuracy
pred2.GAM.white = predict(fit2.GAM.white,newdata = white.train,type="response")

testAccuracy2.GAM.white = 0
c2.GAM.white = 0
accuracy = 0

for (i in 1:99){
  accuracy=sum(white.train$isgood==(pred2.GAM.white>(i*.01)))/nrow(white.train)
  
  if (accuracy > testAccuracy2.GAM.white){
    testAccuracy2.GAM.white = accuracy
    c2.GAM.white = i*.01
  }
}

yhat2.GAM.white = ifelse(predict(fit2.GAM.white,newdata = white.test,type="response")>=c2.GAM.white,1,0)
accuracy2.GAM.white = sum(white.test$isgood==(yhat2.GAM.white))/nrow(white.test)
accuracy2.GAM.white

