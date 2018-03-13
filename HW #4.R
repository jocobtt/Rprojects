bod <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/4%20-%20BodyFat/Data/BodyFat.txt",header=TRUE)
head(bod)

das <- cor(bod)
co <- xtable(das)
print.xtable(co,type="html",file="co.html")
coef(bod)
attach(bod)
plot(bod)
library(xtable)
newobject<-xtable(bomlr)
print.xtable(newobject, type="html", file="filename.html")
#center model?

#cntr.bod <- bod
#for(i in 2:ncol(cntr.bod)){
#   cntr.bod[,i] <- cntr.bod[,i]-mean(cntr.bod[,i])
# }
plot(bod)　#graph better?
library(car)
avPlots(bomlr)
bomlr <- lm(brozek ~ .,data=bod)
summary(bomlr)
library(lmtest)
bptest(bomlr)
library(MASS)
br <-stdres(bomlr)
cooks.distance(br)
which(cooks.distance(br)>3)
ks.test(br, "pnorm")
par(mfrow=c(1,2))
hist(br,xlab="Standard Residuals")
plot(bomlr$fitted.values,bomlr$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)

confint(bomlr,level=.95)
edata <- data.frame(age=50,weight=203,height=67,neck=40.2,chest=114.8,abdom=108.1,
      hip=102.5,thigh=61.3,knee=41.1,ankle=24.7,biceps=34.1,forearm=31,wrist=18.3)
predict.lm(bomlr, newdata=edata)
predict.lm(bomlr,newdata=edata,interval="prediction",level=.95)
#cntr.edata <- edata
#for(i in 2:ncol(cntr.edata)){
#  cntr.edata[,i] <- cntr.edata[,i]-mean(cntr.edata[,i])
#}

n.cv <- 250 
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv) 
coverage <- rep(NA,n.cv)
width <- rep(NA,n.cv)
dv <- rep(NA,n.cv)

for(i in  1:n.cv){
  #step 1 - split into test and training sets
  obs.test <- sample(1:251,25)
  test.data <- cntr.bod[obs.test,]
  train.data <- cntr.bod[-obs.test,]
  #step 2 - fit model to training data
  mymod <- lm(brozek ~.,data=train.data)
  
  #step 3 - predict test data
  test.preds <- predict.lm(mymod,newdata=test.data,interval="prediction",level=.97)
  
  #4 calculate bias and RPMSE 
  bias[i] <- mean((test.preds[,1]-test.data$brozek))
  rpmse[i] <- sqrt(mean((test.preds[,1]-test.data$brozek)^2))
  cvg[i] <- mean(test.preds[,2]<test.data$brozek & test.preds[,3]>test.data$brozek)
  coverage <- mean(cvg)
  width[i] <-mean(test.preds[,3]-test.preds[,2])
  dv <- mean(width) 
  
}
par(mfrow=c(1,1))
hist(bias)
mean(bias)
hist(rpmse)
mean(rpmse)
coverage
dv


