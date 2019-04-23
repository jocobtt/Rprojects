setwd("~/Downloads")
pine <- read.csv("PineBeetle2.csv")
#make infested into int and 0 or 1
pine$Infested <- as.integer(pine$Infested)-1
head(pine$Infested)
str(pine)
#explore data 
par(mfrow=c(1,2))
boxplot(Precip~Infested,data=pine,xlab="Infested",ylab="Annual precip(inches)")
with(pine,scatter.smooth(August_max,Infested,xlab="August Max",
                         ylab="Infested",pch=19,ylim=c(0,1)))
plot(pine$Elev,pine$Infested)
plot(pine$Elev,jitter(pine$Infested,amount=.5))
par(mfrow=c(1,1))
#model selection
library(bestglm)
best.g <- bestglm(pine,family=binomial,method="exhaustive",IC="AIC")
bmod <- best.g$BestModel
bmod

#check assumptions-monotone in probability-jittered scatterplots
par(mfrow=c(1,2))
with(pine,scatter.smooth(Precip,Infested,xlab="Precip",
                         ylab="Infested",pch=19))
with(pine,scatter.smooth(January,Infested,xlab="January",
                         ylab="Infested",pch=19))
with(pine,scatter.smooth(August_max,Infested,xlab="August_Max",
                         ylab="Infested",pch=19,ylim=c(0,1)))
with(pine,scatter.smooth(Slope,Infested,xlab="Slope",
                         ylab="Infested",pch=19))
par(mfrow=c(1,1))
with(pine,scatter.smooth(Elev,Infested,xlab="Elev",
                         ylab="Infested",pch=19))


#transform and confidence interval
betta <- confint(bmod,level=.95)
betta
multipl.conf <- exp(betta)
conff <- 100*(multipl.conf-1)
conff
#print out table
newobject<-xtable(betta)
print.xtable(newobject, type="html", file="filename.html")

newobject<-xtable(conff)
print.xtable(newobject, type="html", file="filename.html")

#find cutoff
pred.probs <- predict.glm(bmod,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA,length=length(thresh))
for(i in 1:length(thresh)) {
  #if probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.probs>thresh[i],1,0)
  #calculate the pct where my classifcation not eq truth
  misclass[i] <-mean(my.classification!=pine$Infested)
}
#find threshold which minimizes misclassification
cutoff.val <- thresh[which.min(misclass)]
cutoff.val
plot(thresh,misclass,type="l",xlab="Cut Off",ylab="Misclassification")


#check fit of model on data-confusion matrix
pred.probs
pred.class <- (pred.probs > cutoff.val)
true.class <- pine$Infested
tab <- table(pred.class,true.class)
addmargins(table(pred.class,true.class))
tab
sens <- tab[2,2]/tab[2,3]
sens
spec <- tab[1,1]/tab[1,3]
spec
ppv <- tab[2,2]/tab[3,2]
ppv
npv <- tab[1,1]/tab[1,3]
npv
library(xtable)
newobject<-xtable(tab)
print.xtable(newobject, type="html", file="filename.html")
#pseudo r^2
up<-bmod$deviance
down <-bmod$null.deviance
psudor <- (1-(up/down))
psudor
#how model does in predicting
#assess predictive ability of model 
n.cv <- 500 
n.test <- round(.1*nrow(pine))
#set my threshold for classifying 
cutoff <- cutoff.val
#initialize matrices to hold CV results
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
#begin for loop
for(cv in 1:n.cv) {
  #seperate into test and training sets
  obs.test <- sample(1:2310,231)
  test.set <- pine[obs.test,]
  train.set <- pine[-obs.test,]
  #fit best model to training set 
  train.model <- glm(Infested~January+August_max+Slope+Elev+Precip+NC+SE+SW,data=train.set,family=binomial)
  #use fitted model to predict test set 
  pred.probs <- predict.glm(train.model,newdata=test.set,type="response")
  #classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,1,0)
  #create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Infested,levels=c(0,1)),factor(test.class,levels=c(0,1))))
  # pull of sensitivity,specificity,ppv and npv
  #using bracket notation
  sens[cv] <- conf.mat[2,2]/conf.mat[2,3]
  spec[cv] <- conf.mat[1,1]/conf.mat[1,3]
  ppv[cv] <- conf.mat[2,2]/conf.mat[3,2]
  npv[cv] <- conf.mat[1,1]/conf.mat[1,3]
  
}
mean(sens)
mean(spec)
mean(ppv)
mean(npv)

#predict for new plot
yr.one <- data.frame(Year=2018,January=-13.98,August_max=15.89,Precip=771.13,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.log.odds <- predict.glm(bmod,newdata=yr.one)
pred.log.odds
pred.prob <- exp(pred.log.odds)/(1+exp(pred.log.odds))
pred.prob
pred.probb <- predict.glm(bmod,newdata=yr.one,type="response")
pred.probb
yr.two <-data.frame(Year=2019,January=-17.8,August_max=18.07,Precip=788.54,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.probb <- predict.glm(bmod,newdata=yr.two,type="response")
pred.probb
yr.five <-data.frame(Year=2022,January=-15.99,August_max=18.23,Precip=732.32,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.probb <- predict.glm(bmod,newdata=yr.five,type="response")
pred.probb
yr.ten <- data.frame(Year=2027,January=-12.44,August_max=16.96,Precip=801.22,Elev=1901.95,SE="Yes",Slope=18.07,SW="No",NC="No")
pred.probb <- predict.glm(bmod,newdata=yr.ten,type="response")
pred.probb
