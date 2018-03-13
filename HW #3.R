water <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/3%20-%20WaterRunoff/Data/water.txt",header=TRUE)
cor(water$Precip,water$Runoff)
cov(water$Precip,water$Runoff)
scatter.smooth(water$Precip,water$Runoff,pch=19,col="blue",
               xlab="Precipatation in Inches",ylab="Runoff Foot-Acres",main="Precipatation by Runoff")
slr <- lm(Runoff~Precip, data=water)
slr
summary(slr)
abline(slr,col="red",lty=1,lwd=3)
library(lmtest)
bptest(slr)
library(MASS)
st <-stdres(slr)
cooks.distance(slr)
which(cooks.distance(slr)>3)
ks.test(st, "pnorm")


hist(st,xlab="Standard Residuals for Percipatation",
     main="Histogram of SLR model Standard Residuals")
plot(slr$fitted.values,slr$residuals,xlab="SLR Fitted Values",ylab="SLR Residuals", 
     main="SLR Residuals vs. Fitted Values Scatterplot")
abline(0,0, lty=4)
#4 
summary(slr)
sp <- seq(-10,20,length=100)
jpred <- predict.lm(slr.centered,newdata=data.frame(Precip.cnt=sp,Runoff=1))
plot(water$Precip.cnt,water$Runoff,pch=19,col="blue",
     xlab="precipatation in inches",ylab="runoff in acre-feet",main="precipatation by runoff")
lines(sp,jpred)

#5
water$Precip.cnt <- water$Precip-mean(water$Precip)
slr.centered <- lm(Runoff~Precip.cnt, data=water)
slr.centered
summary(slr)
summary(slr.centered)
library(lmtest)
bptest(slr.centered)
library(MASS)
stc <-stdres(slr.centered)
cooks.distance(slr.centered)
which(cooks.distance(slr.centered)>3)
ks.test(stc, "pnorm")

n.cv <- 80
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv) 
coverage <- rep(NA,n.cv)
width <- rep(NA,n.cv)
dv <- rep(NA,n.cv)

for(i in  1:n.cv){
  #step 1 - split into test and training sets
  obs.test <- sample(1:43,4)
  test.data <- water[obs.test,]
  train.data <- water[-obs.test,]
  #step 2 - fit model to training data
  mymod.cnt <- lm(Runoff~Precip.cnt,data=train.data)
  
  #step 3 - predict test data
  test.preds <- predict.lm(mymod.cnt,newdata=test.data,interval="prediction",level=.97)
  
  #4 calculate bias and RPMSE 
  bias[i] <- mean((test.preds[,1]-test.data$Precip.cnt))
  rpmse[i] <- sqrt(mean((test.preds[,1]-test.data$Precip.cnt)^2))
  cvg[i] <- mean(test.preds[,2]<test.data$Precip.cnt & test.preds[,3]>test.data$Precip.cnt)
  coverage <- mean(cvg)
  width[i] <-mean(test.preds[,3]-test.preds[,2])
  dv <- mean(width) 
  
}
hist(bias)
mean(bias)
hist(rpmse)
mean(rpmse)
cvg
dv


#7
confint(slr.centered,level=.95)
#8
edata <- data.frame(Precip.cnt=c(4.5))
predict.lm(slr.centered, newdata=edata)
predict.lm(slr.centered,newdata=edata,interval="prediction",level=.95)



