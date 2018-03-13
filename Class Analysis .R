cancer <- read.table("https://mheaton.byu.edu/Courses/Stat330/ReviewAnalyses/Logistic-BreastCancer/Data/BreastCancer.txt",header=TRUE)
head(cancer)
#change order 
myvars <- c("Adhes","BNucl","Chrom","Epith","Mitos","NNucl","Thick","UShap","USize","Malignant")
can <- cancer[myvars]
head(can)
as.numeric(as.factor(can$Malignant))
library(plyr)
can$Malignant <- revalue(can$Malignant, c("Yes"=1))
can$Malignant <- revalue(can$Malignant, c("No"=0))
head(can)
#1 & 2
mod <- bestglm(can,family=binomial,method="exhaustive",IC="AIC")
bestt <- mod$BestModel
bestt
beta.conf <-confint(bestt,level=.95)
beta.conf
multiplicative.conf <- exp(confint(bestt,level=.95))
multiplicative.conf
percent.conf <- 100*(multiplicative.conf-1)
percent.conf
#3 
pred.probs <- predict.glm(bestt,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA,length=length(thresh))
for(i in 1:length(thresh)) {
  #if probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.probs>thresh[i],1,0)
  #calculate the pct where my classifcation not eq truth
  misclass[i] <-mean(my.classification!=can$Malignant)
}
#find threshold which minimizes misclassification
thre <- thresh[which.min(misclass)]
thre
plot(thre,misclass,type="l",xlab="Cutoff",ylab="Missclass",main="Missclass Vs. Cutoff")

n.cv <- 500 
n.test <- round(.1*nrow(can))
#set my threshold for classifying 
cutoff <- thre
#initialize matrices to hold CV results
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
#begin for loop
for(cv in 1:n.cv) {
  #seperate into test and training sets
  obs.test <- sample(1:681,68)
  test.set <- can[obs.test,]
  train.set <- can[-obs.test,]
  #fit best model to training set 
  train.model <- glm(Malignant~Adhes+BNucl+Chrom+Mitos+NNucl+Thick+UShap,data=train.set,family=binomial)
  #use fitted model to predict test set 
  pred.probs <- predict.glm(train.model,newdata=test.set,type="response")
  #classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,1,0)
  #create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Malignant,levels=c(0,1)),factor(test.class,levels=c(0,1))))
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

#4 
pat <- data.frame(Adhes= 3, BNucl= 1, Chrom= 5, Epith= 8, Mitos= 1, NNucl= 8, Thick= 3,
                  UShap= 1)
pred.log.odds <- predict.glm(bestt,newdata=pat)
pred.log.odds
pred.prob <- exp(pred.log.odds)/(1+exp(pred.log.odds))
pred.prob
pred.probb <- predict.glm(bestt,newdata=pat,type="response")
pred.probb
#1- sens of those with malignity when we say that they do not have malignant cancer 


#epilepsy analysis 
ep <- read.table("https://mheaton.byu.edu/Courses/Stat330/ReviewAnalyses/Poisson-Epilepsy/Data/Epilepsy.txt",header=TRUE)
hist(ep$Seizures)
ep <- ep[-49,]
random <- c("Age","Progabide","Seizures")
reep <- ep[random]
library(bestglm) 
#1 
best.ep <- bestglm(reep,family=poisson,method="exhaustive",IC="AIC")
best.ep
ep.mod <- best.ep$BestModel
tri <- glm(Seizures~Age+Progabide,data=ep,family=poisson)
summary(tri)
#confidence interval
int <- confint(tri,level=.95)
int
tran <- (exp(int)-1)
tran
100*tran
#2- from above 
#3 
pred.ep <- data.frame(Age=19,Progabide=1)
pred.log.mean <- predict.glm(tri,newdata=pred.ep)
pred.log.mean
pred.mean <- predict.glm(tri,newdata=pred.ep,type="response")
pred.mean
#confidence interval
log.mean <- predict.glm(tri,newdata=pred.ep,se.fit=TRUE)
log.mean
int.low <- log.mean$fit - qnorm(.975)*log.mean$se.fit
int.low
int.up <- log.mean$fit + qnorm(.975)*log.mean$se.fit
int.up
interval <- c(exp(int.low),exp(int.up))
interval

