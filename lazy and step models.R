#lazy red
rtrain = read.csv("red train.csv")
fitr1 = glm(isgood~.,data=rtrain,family=binomial)
summary(fit)
library(pROC)
plot.roc(rtrain$isgood,fit$fitted.values)

#lazy white
wtrain = read.csv("white train.csv")
fitw1 = glm(isgood~.,data=wtrain,family=binomial)
summary(fit)
plot.roc(wtrain$isgood,fit$fitted.values)

#step red
fitr1step = step(fitr1)
plot.roc(rtrain$isgood,fitr1step$fitted.values)

#step white
fitw1step = step(fitw1)
plot.roc(wtrain$isgood,fitw1step$fitted.values)

rtest = read.csv("red test.csv")
wtest = read.csv("white test.csv")

#accuracies
#lazy red
pred.lazy.red = predict(fitr1,newdata = rtrain,type="response")

testAccuracy.lazy.red = 0
c.lazy.red = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(rtrain$isgood==(pred.lazy.red>(i*.01)))/nrow(rtrain)
  
  if (accuracy > testAccuracy.lazy.red){
    testAccuracy.lazy.red = accuracy
    c.lazy.red = i*.01
  }
}

yhat.lazy.red = ifelse(predict(fitr1,newdata = rtest,type="response")>c.lazy.red,1,0)
accuracy.lazy.red = sum(rtest$isgood==(yhat.lazy.red))/nrow(rtest)
accuracy.lazy.red

#lazy white
pred.lazy.white = predict(fitw1,newdata = wtrain,type="response")

testAccuracy.lazy.white = 0
c.lazy.white = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(wtrain$isgood==(pred.lazy.white>(i*.01)))/nrow(wtrain)
  
  if (accuracy > testAccuracy.lazy.white){
    testAccuracy.lazy.white = accuracy
    c.lazy.white = i*.01
  }
}

yhat.lazy.white = ifelse(predict(fitw1,newdata = wtest,type="response")>c.lazy.white,1,0)
accuracy.lazy.white = sum(wtest$isgood==(yhat.lazy.white))/nrow(wtest)
accuracy.lazy.white

#step red
pred.step.red = predict(fitr1step,newdata = rtrain,type="response")

testAccuracy.step.red = 0
c.step.red = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(rtrain$isgood==(pred.step.red>(i*.01)))/nrow(rtrain)
  
  if (accuracy > testAccuracy.step.red){
    testAccuracy.step.red = accuracy
    c.step.red = i*.01
  }
}

yhat.step.red = ifelse(predict(fitr1step,newdata = rtest,type="response")>c.step.red,1,0)
accuracy.step.red = sum(rtest$isgood==(yhat.step.red))/nrow(rtest)
accuracy.step.red

#step white
pred.step.white = predict(fitw1step,newdata = wtrain,type="response")

testAccuracy.step.white = 0
c.step.white = 0
accuracy = 0 

for (i in 1:99){
  accuracy=sum(wtrain$isgood==(pred.step.white>(i*.01)))/nrow(wtrain)
  
  if (accuracy > testAccuracy.step.white){
    testAccuracy.step.white = accuracy
    c.step.white = i*.01
  }
}

yhat.step.white = ifelse(predict(fitw1step,newdata = wtest,type="response")>c.step.white,1,0)
accuracy.step.white = sum(wtest$isgood==(yhat.step.white))/nrow(wtest)
accuracy.step.white