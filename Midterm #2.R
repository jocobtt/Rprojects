farm <- read.table("https://mheaton.byu.edu/Courses/Stat330/Exams/Midterm2/Data/Farms3.txt",header=TRUE)
plot(farm)
num <- c("improvements","crpPct","productivity","tillable","acrePrice")
cnum <- farm[num]
random <- cor(cnum)
library(xtable)
newobject<-xtable(random)
print.xtable(newobject, type="html", file="filename.html")
head(farm)
#reorder
org <- c("improvements","tillable","financing","crpPct","productivity","NW","SC","SE","SW","WC","acrePrice")
farmm <- farm[org]
#log acreprice and model
#check that interaction is right
farmm$acrePrice <- log(farmm$acrePrice)
farms.lm <- lm(acrePrice ~improvements+tillable+financing+crpPct+SC+SE+SW+WC+productivity*NW, data=farmm)
summary(farms.lm)
library(lmtest)
bptest(farms.lm)
farm.st <- stdres(farms.lm)
cooks.distance(farms.lm)
which(cooks.distance(farms.lm)>3)
library(MASS)
ks.test(farm.st,"pnorm")
par(mfrow=c(1,2))
hist(farm.st,xlab="Standard Residuals",main="br Histogram")
plot(farms.lm$fitted.values,farms.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
par(mfrow=c(1,1))
#check collinearity
library(car)
vif(farms.lm)
#model choice
library(bestglm)
farm.glm <- bestglm(farmm,IC="AIC",method="backward")
farm.glm$BestModels
farm.glm$Subsets
plot(0:(nrow(farm.glm$Subsets)-1),farm.glm$Subsets$AIC,type="b",pch=19,xlab="# of vars", ylab="AIC")
best.lm <- farm.glm$BestModel 
summary(best.lm)
t <- confint(best.lm,level=.95)
object<-xtable(t)
print.xtable(object, type="html", file="filename.html")
#check assumptions
library(lmtest)
bptest(best.lm)
best.st <- stdres(best.lm)
cooks.distance(best.lm)
which(cooks.distance(best.lm)>3)
library(MASS)
ks.test(best.st,"pnorm")
par(mfrow=c(1,2))
hist(best.st,xlab="Standard Residuals",main="Histogram of Std. Res.")
plot(best.lm$fitted.values,best.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
par(mfrow=c(1,1))
library(car)
avPlots(best.lm)
#check prediction accuracy
#undo log transformation??
n.cv <- 250 
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
cvg <- rep(NA,n.cv) 
coverage <- rep(NA,n.cv)
width <- rep(NA,n.cv)
dv <- rep(NA,n.cv)
for(i in  1:n.cv){
  #step 1 - split into test and training sets
  obs.test <- sample(1:420,42)
  test.data <- farm[obs.test,]
  train.data <- farm[-obs.test,]
  #step 2 - fit model to training data
  mymod <- lm(log(acrePrice) ~improvements+tillable+crpPct+WC+productivity+NW,data=train.data)

  #step 3 - predict test data
  test.preds <- exp(predict.lm(mymod,newdata=test.data,interval="prediction",level=.95))
  
  #4 calculate bias and RPMSE 
  bias[i] <- mean((test.preds[,1]-test.data$acrePrice))
  rpmse[i] <- sqrt(mean((test.preds[,1]-test.data$acrePrice)^2))
  cvg[i] <- mean(test.preds[,2]<test.data$acrePrice & test.preds[,3]>test.data$acrePrice)
  coverage <- mean(cvg)
  width[i] <-mean(test.preds[,3]-test.preds[,2])
  dv <- mean(width) 
  
}
hist(bias)
mean(bias)
hist(rpmse)
mean(rpmse)
hist(coverage)
abline(v=mean(cvg),col="red",lwd=3)
coverage
dv
hist(dv)
#predict
newfarm <- data.frame(improvements=0,tillable=94,crpPct=0,productivity=96,NW="Yes",WC="No")
pred <- predict.lm(best.lm,newdata=newfarm,interval="prediction",level=.95)
suggest <- exp(pred)
suggest
#interaction 
srl <- lm(acrePrice~improvements+tillable+crpPct+WC+productivity*NW,data=farmm)
summary(srl)
hlr <- lm(acrePrice~improvements+tillable+crpPct+WC+productivity+NW, data=farmm)
summary(hlr)
anova(srl,hlr)
anova(hlr)
sho <- confint(srl,level=.95)
sho
library(xtable)
newobject<-xtable(sho)
print.xtable(newobject, type="html", file="filename.html")

##for best lm model if asked for interaction you take what they give you with the best glm and then include that interaction as well into you model that you got from your best glm test
#also if asked for interaction, plot that interaction on ggplot like region of wc or nw 


