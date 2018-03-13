players <- read.table("https://mheaton.byu.edu/Courses/Stat330/ReviewAnalyses/MLR-Hitters/Data/BaseballSalary.txt",header=TRUE)
head(players)
summary(players)
players$CHits <- log(players$CHits)
players$CAtBat <- log(players$CAtBat)
players$Salary <- log(players$Salary)


#model assumptions
sal.mod <- lm(Salary~.,data=players)
summary(sal.mod)
library(lmtest)
bptest(sal.mod)
library(MASS)
br <-stdres(sal.mod)
cooks.distance(sal.mod)
which(cooks.distance(sal.mod)>3)
ks.test(br, "pnorm")
hist(br,xlab="Standard Residuals",main="br Histogram")
plot(sal.mod$fitted.values,sal.mod$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
#find best model-what parts are most valuable
library(car)
sal.mod <- lm(Salary ~.,data=players)
vif(sal.mod)

library(bestglm)
pla <- bestglm(players,IC="BIC",method="forward")
pla$BestModels
pla$Subsets
plot(0:(nrow(pla$Subsets)-1),pla$Subsets$BIC,type="b",pch=19,xlab="# of vars", ylab="BIC")

vs.res <- bestglm(players, IC="CV",method="backward",t=100)
vs.res$Subsets
plot(0:(nrow(vs.res$Subsets)-1),vs.res$Subsets$CV,type="b",pch=19,xlab="# of vars", ylab="CV")
best.lm <- vs.res$BestModel 
summary(best.lm)
confint(best.lm,level=.95)

#predict
plapred <- data.frame(AtBat=551,Hits=171,HmRun=13,Runs=94,RBI=83,Walks=94,Years=13,CAtBat=log(6090),CHits=log(1840),CHmRun=128,CRuns=969,CRBI=900,CWalks=917,League="N",PutOuts=1199,Assists=149,Errors=5)
prod <- predict.lm(best.lm,newdata=plapred)
a <- exp(prod)
a
pred <- predict.lm(best.lm,newdata=plapred,interval="prediction",level=.95)
suggest <- exp(pred)
suggest

# assumptions check for best model 
summary(sal.mod)
library(lmtest)
bptest(best.lm)
library(MASS)
br <-stdres(best.lm)
cooks.distance(sal.mod)
which(cooks.distance(best.lm)>3)
ks.test(br, "pnorm")
hist(br,xlab="Standard Residuals",main="br Histogram")
plot(best.lm$fitted.values,best.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
##added variable plots for linearity
library(car)
avPlots(best.lm)



# in class num 2 
cig <- read.table("https://mheaton.byu.edu/Courses/Stat330/ReviewAnalyses/MLR-CigaretteConsumption/Data/Cigarettes.txt",header=TRUE)
head(cig)
View(cig)
#cig$Income <- log(cig$Income) not needed
plot(cig)
#assumptions of original model
cig.lm <- lm(Sales~.,data=cig)
summary(cig.lm)
library(lmtest)
bptest(cig.lm)
library(MASS)
br <-stdres(cig.lm)
cooks.distance(cig.lm)
which(cooks.distance(cig.lm)>3)
ks.test(br, "pnorm")
hist(br,xlab="Standard Residuals",main="br Histogram")
plot(cig.lm$fitted.values,cig.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
#check what points to include
library(car)
vif(cig.lm)

library(bestglm)
vs.res <- bestglm(cig, IC="AIC",method="forward",t=100)
vs.res$Subsets
plot(0:(nrow(vs.res$Subsets)-1),vs.res$Subsets$CV,type="b",pch=19,xlab="# of vars", ylab="CV")
best.lm <- vs.res$BestModel 
summary(best.lm)
confint(best.lm,level=.95)
#predict
ut <- data.frame(Age=28.1,HS=62.6,Income=4493,Black=7,Female=50.8,Price=39.7)
prod <- predict.lm(best.lm,newdata=ut)
prod

pred <- predict.lm(best.lm,newdata=ut,interval="prediction",level=.95)
pred 
#increase price by 10%
dat <- data.frame(Age=28.1,HS=62.6,Income=4493,Black=7,Female=50.8,Price=39.7+3.97)
prad <- predict.lm(best.lm,newdata=dat)
prad

prud <- predict.lm(best.lm,newdata=dat,interval="prediction",level=.95)
prud 
#prediction accuracy 
n.cv <- 250 
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv) 
coverage <- rep(NA,n.cv)
width <- rep(NA,n.cv)
dv <- rep(NA,n.cv)

for(i in  1:n.cv){
  #step 1 - split into test and training sets
  obs.test <- sample(1:51,5)
  test.data <- cig[obs.test,]
  train.data <- cig[-obs.test,]
  #step 2 - fit model to training data
  mymod <- lm(Sales ~.,data=train.data)
  
  #step 3 - predict test data
  test.preds <- predict.lm(mymod,newdata=test.data,interval="prediction",level=.95)
  
  #4 calculate bias and RPMSE 
  bias[i] <- mean((test.preds[,1]-test.data$Sales))
  rpmse[i] <- sqrt(mean((test.preds[,1]-test.data$Sales)^2))
  cvg[i] <- mean(test.preds[,2]<test.data$Sales & test.preds[,3]>test.data$Sales)
  coverage <- mean(cvg)
  width[i] <-mean(test.preds[,3]-test.preds[,2])
  dv <- mean(width) 
  
}

hist(bias)
mean(bias)
hist(rpmse)
mean(rpmse)
coverage
dv

summary(best.lm)
library(lmtest)
bptest(best.lm)
library(MASS)
br <-stdres(best.lm)
cooks.distance(best.lm)
which(cooks.distance(best.lm)>3)
ks.test(br, "pnorm")
hist(br,xlab="Standard Residuals",main="br Histogram")
plot(best.lm$fitted.values,best.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
avPlots(best.lm) 
