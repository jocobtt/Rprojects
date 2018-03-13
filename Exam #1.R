pm <- read.table("https://mheaton.byu.edu/Courses/Stat330/Exams/Midterm1/Data/PM.txt",header=TRUE)
head(pm)
names(pm)
summary(pm)
cor(pm$Cars,pm$Particles)
cov(pm$Cars,pm$Particles)
plot(pm$Cars,pm$Particles,ylab="Particles",pch=19,xlab="Cars",
     main="Pollution Particles by Number of Cars",col="blue")
scatter.smooth(pm$Cars,pm$Particles,ylab="Particulate Matter (pollution units)",pch=19,xlab="Number of Cars",
             main="Particulate Matter by Number of Cars",col="red")
#data taken everyday 
#need to interpret sigma^2 for errors when explaining model from now own
#residuals are normal 
#pollution units 
#need to transform for variance-heteroskedasity and normality?
tmod <- lm(log(Particles) ~ Cars, data=pm)
summary(tmod)
library(lmtest)
bptest(tmod)
library(MASS)
tt <- stdres(tmod)
cooks.distance(tmod)
which(cooks.distance(tmod)>3)
ks.test(tt,"pnorm")
hist(tt)
curve(pnorm,from=-10, to=10,add=TRUE,col="red",lwd=3)
plot(tmod$fitted.values,tmod$residuals)
abline(0,0)
#need graph of transformed data with abline
#if think there is dependence plow forward anyway
#center model 
pm$Cars.cnt <- pm$Cars-mean(pm$Cars)
slr.cent <- lm(log(Particles) ~ Cars.cnt, data=pm) 
slr.cent
summary(slr.cent)
library(lmtest)
bptest(slr.cent)
library(MASS)
tc <-stdres(slr.cent)
cooks.distance(slr.cent)
which(cooks.distance(slr.cent)>3)
ks.test(tc, "pnorm")
hist(tc,xlab="Standard Residuals for Cars",
     main="Histogram of SLR model Standard Residuals")
plot(slr.cent$fitted.values,slr.cent$residuals,xlab="SLR Fitted Values",ylab="SLR Residuals", 
     main="SLR Residuals vs. Fitted Values Scatterplot")
abline(0,0)
#make sure to still keep in terms of transformation when you interpret r^2 etc. 
#graph to check 

sp <- seq(0,5000,length=1000)
cen <- sp-mean(pm$Cars)
jpred <- predict.lm(slr.cent,newdata=data.frame(Cars.cnt=cen,Particles=1))
ejpred <- exp(jpred)
par(mfrow=c(1,1))
plot(pm$Cars,pm$Particles,ylab="Particulate Matter (pollution units)",pch=19,xlab="Number of Cars",
     main="Particulate Matter by Number of Cars",col="red" )
lines(sp,ejpred,lty=2,lwd=3)

#check fit of model etc. 
n.c <- 100
bias <- rep(NA,n.c)
rpmse <- rep(NA,n.c)
cvg <- rep(NA,n.c)
width <- rep(NA,n.c)
dv <- rep(NA,n.c)

for(i in 1:n.c){
  #split test and training data
  ods.test <- sample(1:500,50)
  test.data <- pm[ods.test,]
  train.data <- pm[-ods.test,]
  #fit model to training dataset
  my.mod <- lm(log(Particles)~log(Cars),data=train.data)
  #predict test data need exponental-check if cant log for both cars.cnt and particles
  test.preds <- (exp(predict.lm(my.mod,newdata=test.data,interval="prediction",level=.97)))
  #calculate biase, rpmse, cvg, width, dv
  bias[i] <- mean((test.preds[,1]-test.data$Particles))
  rpmse[i] <- sqrt(mean((test.preds[,1]-test.data$Particles)^2))
  cvg[i] <- mean(test.preds[,2]<test.data$Particles & test.preds[,3]>test.data$Particles)
  coverage <- mean(cvg)
  width[i] <-mean(test.preds[,3]-test.preds[,2])
  dv <- mean(width) 
}
par(mfrow=c(1,2))
hist(rpmse)
hist(bias)
mean(bias)
mean(rpmse)
coverage 
dv

#value and confidence interval
exp(confint(slr.cent,level=.95))

#predict for cars-1800 

mean(pm$Cars)
crs <- data.frame(Cars.cnt=c(1800-1683))
(exp(predict.lm(slr.cent,newdata=crs)))
(exp(predict.lm(slr.cent,newdata=crs,interval="prediction", level=.95)))
