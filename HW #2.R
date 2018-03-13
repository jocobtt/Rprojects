stp <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/2%20-%20StoppingDistance/Data/StoppingDistance.txt", header = TRUE)
dim(stp)
cor(stp$Speed, stp$Distance)
cov(stp$Speed, stp$Distance)
mean(stp$Distance)
mean(stp$Speed)
sd(stp$Speed)
sd(stp$Distance)
scatter.smooth(stp$Speed, stp$Distance, pch=19, col="blue",xlab="Speed(MPH)",
               ylab="Distance to stop(Feet)", main="Stopping Distance of car by speed")
slr <- lm(Distance~Speed, data=stp)
slr
names(slr)
abline(slr,col="red",lty=2,lwd=1)
coef(slr)
lsum <- summary(slr)
lsum
library(lmtest)
bptest(slr)
library(MASS)
st <-stdres(slr)
hist(st,xlab="Standard Residuals for Speed",main="Histogram of SLR Model Standard Residuals")
plot(slr$fitted.values,slr$residuals,xlab="SLR fitted values", ylab="SLR Residuals",main="SLR Residuals vs. Fitted Values Scatterplot")
abline(0,0, lty=4)
cooks.distance(slr)
which(cooks.distance(slr)>3)
cooks.distance(klr)
which(cooks.distance(klr)>3)
ks.test(st, "pnorm")
klr <- lm(sqrt(Distance)~sqrt(Speed),data = stp )
klr
kt <- stdres(klr)
summary(klr)
bptest(klr)
which(cooks.distance(klr)>3)
ks.test(kt,"pnorm")
#7
hist(kt,xlab="Standardized Residuals",main="Histogram of Standardized Residuals")
plot(klr$fitted.values,klr$residuals,xlab="Fitted Values",ylab="Residuals", main="Residuals vs. Fitted Values")
abline(0,0, lty=4)
#8
sp <- seq(0,40,length=62)
jpred <- predict.lm(klr,newdata=data.frame(Speed=sp,Distance=1))
ejpred <- (jpred)^2
plot(stp$Speed,stp$Distance,pch=19, col="blue",xlab="Speed(MPH)",
     ylab="Distance to stop(Feet)", main="Stopping Distance of car by speed" )
lines(sp,ejpred)
#9 
n.cv <- 100
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)


for(i in  1:n.cv){
  #step 1 - split into test and training sets
  obs.test <- sample(1:62,6)
  test.data <- stp[obs.test,]
  train.data <- stp[-obs.test,]
  #step 2 - fit model to training data
  my.model <- lm(sqrt(Distance)~sqrt(Speed),data=train.data)
  
  #step 3 - predict test data
  test.preds <- ((predict.lm(my.model,newdata=test.data))^2)
  
  #4 calculate bias and RPMSE 
  bias[i] <- mean((test.preds-test.data$Distance))
  rpmse[i] <- sqrt(mean((test.preds-test.data$Distance)^2))
}
hist(bias)
mean(bias)
hist(rpmse)
mean(rpmse)

redata <- data.frame(Speed=c(35,30))
tf <- predict.lm(slr,newdata=redata)
tf