prof <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/6%20-%20RateMyProfessor/Data/RateMyProfessor.txt",header=TRUE)
one <- c("gender","numYears", "numRaters", "numCourses","pepper","easiness","raterInterest","disciplinePre.prof","disciplineSocSci",
         "disciplineSTEM","quality")
onet <- prof[one]
plot(onet)
two <- c("deptBiology","deptBusiness","deptChemistry","deptCommunication","deptCS","deptEducation","deptEnglish","deptGeology","deptHistory","deptKins")
twot <- prof[two]
plot(twot)
three <- c("deptLanguages","deptMath","deptMusic","deptPhilosophy","deptPhysics","deptPolySci","deptPsychology","deptReligion","deptSocialScience","deptSociology", "quality")
threet <- prof[three]
plot(threet)

myvars <- c("gender","numYears", "numRaters", "numCourses","pepper","easiness","raterInterest","disciplinePre.prof","disciplineSocSci",
            "disciplineSTEM", "deptBiology","deptBusiness","deptChemistry","deptCommunication","deptCS","deptEducation","deptEnglish","deptGeology","deptHistory","deptKins","deptLanguages","deptMath","deptMusic","deptPhilosophy",
            "deptPhysics","deptPolySci","deptPsychology","deptReligion","deptSocialScience","deptSociology", "quality")
proff <- prof[myvars]
head(proff)


#check assumptions

pro.lm <- lm(quality~.,data=prof)
summary(pro.lm)
library(lmtest)
bptest(pro.lm)
res <- stdres(pro.lm)
cooks.distance(pro.lm)
which(cooks.distance(pro.lm)>3)
library(MASS)
ks.test(res,"pnorm")
par(mfrow=c(1,2))
hist(res,xlab="Standard Residuals",main="br Histogram")
plot(pro.lm$fitted.values,pro.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
par(mfrow=c(1,1))
avPlots(pro.lm)
#check for colinearity
library(car)
vif(pro.lm)

prof.glm <- bestglm(proff,IC="AIC",method="exhaustive")
prof.glm$BestModels
prof.glm$Subsets
plot(0:(nrow(prof.glm$Subsets)-1),prof.glm$Subsets$AIC,type="b",pch=19,xlab="# of vars", ylab="BIC")
best.lm <- prof.glm$BestModel 
 summary(best.lm)
 confint(best.lm,level=.95)

pro.glm <- bestglm(proff,IC="AIC",method="forward")
pro.glm$BestModels
pro.glm$Subsets
plot(0:(nrow(pro.glm$Subsets)-1),pro.glm$Subsets$AIC,type="b",pch=19,xlab="# of vars", ylab="AIC")
bes.lm <- pro.glm$BestModel 
summary(bes.lm)
te <- confint(bes.lm,level=.95)
library(xtable)
newobject<-xtable(te)
print.xtable(newobject, type="html", file="filename.html")

#model assumptions
library(lmtest)
bptest(bes.lm)
es <- stdres(bes.lm)
cooks.distance(bes.lm)
which(cooks.distance(bes.lm)>3)
library(MASS)
ks.test(es,"pnorm")
par(mfrow=c(1,2))
hist(es,xlab="Standard Residuals",main="br Histogram")
plot(bes.lm$fitted.values,bes.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
par(mfrow=c(1,1))
avPlots(bes.lm)


#predict 
profd <- data.frame(numYears=4,numRaters= 20,numCourses= 2,pepper="no",disciplineSTEM="Yes",deptMath="Yes",easiness= 4.1,raterInterest=4.2,deptBusiness="No",deptPhysics="No",disciplinePre.prof="No",deptEnglish="No",deptGeology="No")      
proed <- predict.lm(bes.lm,newdata=profd,interval="prediction",level=.95)
proed
