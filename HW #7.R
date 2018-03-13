dia <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/7%20-%20Diabetes/Data/Diabetes.txt",header=TRUE)
#delete observations of bmi, triceps, with zeros
topdrop <- which(dia$bmi==0 | dia$diastolic==0 | dia$glucose==0 | dia$triceps==0 | dia$insulin==0)
dia <- dia[-topdrop,]

#exploratory data analysis
mytable <- table(dia$glucose, dia$diabetes)
ftable(mytable)

plot(dia$glucose,jitter(dia$diabetes,amount=.3))
plot(dia$pedigree,jitter(dia$diabetes,amount=.3))
par(mfrow=c(1,2))
scatter.smooth(dia$glucose,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="Glucose")
scatter.smooth(dia$pedigree,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="pedigree")
par(mfrow=c(1,1))
boxplot(pregnant~diabetes,data=dia,xlab="Diabetes",ylab="Pregnant")
#side by side scatterplots and cross tabulations
library(bestglm)
best.dia <- bestglm(dia,family=binomial,method="exhaustive",IC="AIC")
best.dia
#fit model and confidence interval
modd <- glm(diabetes ~., data=dia,family=binomial)
mod <- best.dia$BestModel
mod
beta.conf <-confint(mod,level=.95)
beta.conf
multiplicative.conf <- exp(confint(mod,level=.95))
multiplicative.conf
percent.conf <- 100*(multiplicative.conf-1)
percent.conf
library(xtable)
newobject<-xtable(percent.conf)
print.xtable(newobject, type="html", file="filename.html")
par(mfrow=c(1,4))
scatter.smooth(dia$glucose,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="Glucose")
scatter.smooth(dia$bmi,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="BMI")
scatter.smooth(dia$pedigree,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="Pedigree")
scatter.smooth(dia$age,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="Age")
par(mfrow=c(1,1))
scatter.smooth(dia$pregnant,jitter(dia$diabetes,amount=.3),ylab="Diabetes",xlab="Pregnant")

#determine threshold that minimizes misclassification rate 
pred.probs <- predict.glm(mod,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA,length=length(thresh))
for(i in 1:length(thresh)) {
  #if probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.probs>thresh[i],1,0)
  #calculate the pct where my classifcation not eq truth
  misclass[i] <-mean(my.classification!=dia$diabetes)
}
#find threshold which minimizes misclassification
threshh <- thresh[which.min(misclass)]
threshh
plot(thresh,misclass,type="l",xlab="Cutoff",ylab="Missclass",main="Missclass Vs. Cutoff")
#create confusion matrix and pseudo R^2 
pred.probs
pred.class <- (pred.probs > threshold)
true.class <- dia$diabetes
table(pred.class,true.class)
tab <- addmargins(table(pred.class,true.class))
sens <- tab[2,2]/tab[2,3]
sens
spec <- tab[1,1]/tab[1,3]
spec
ppv <- tab[2,2]/tab[3,2]
ppv
npv <- tab[1,1]/tab[1,3]
npv
newobject<-xtable(tab)
print.xtable(newobject, type="html", file="filename.html")
#pseudo 
up<-mod$deviance
down <-mod$null.deviance
psudorsqr <- (1-(up/down))
psudorsqr
#assess predictive ability of model 
n.cv <- 500 
n.test <- round(.1*nrow(dia))
#set my threshold for classifying 
cutoff <- threshh
#initialize matrices to hold CV results
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
#begin for loop
for(cv in 1:n.cv) {
  #seperate into test and training sets
  obs.test <- sample(1:392,39)
  test.set <- dia[obs.test,]
  train.set <- dia[-obs.test,]
  #fit best model to training set 
  train.model <- glm(diabetes~glucose+bmi+pedigree+age,data=train.set,family=binomial)
  #use fitted model to predict test set 
  pred.probs <- predict.glm(train.model,newdata=test.set,type="response")
  #classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,1,0)
  #create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$diabetes,levels=c(0,1)),factor(test.class,levels=c(0,1))))
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
#predict 
patient <- data.frame(pregnant= 1, glucose= 90, diastolic=62, triceps= 18, 
                      insulin= 59, bmi= 25.1, pedigree= 1.268, age= 25)
pred.log.odds <- predict.glm(mod,newdata=patient)
pred.log.odds
pred.prob <- exp(pred.log.odds)/(1+exp(pred.log.odds))
pred.prob
pred.probb <- predict.glm(mod,newdata=patient,type="response")
pred.probb
exp(pred.probb)




