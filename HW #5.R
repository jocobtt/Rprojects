life <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/5%20-%20LifeExpectancy/Data/LifeExpectancy2.txt",header=TRUE)
#data exploration
head(life)
summary(life)
names(life)
plot(life)
cor(life$LifeExp,life$PPGDP)
table(life)
Group <- relevel(Group, ref="other")
table(life)
plot(LifeExp,PPGDP, pch=unclass(Group))  # different symbol
legend("topleft", legend=levels(Group), pch=c(1:3))

#fitting model to data and testing model assumptions
dof <- lm(LifeExp~log(PPGDP)*Group,data=life)
summary(dof)
library(lmtest)
bptest(dof)
library(MASS)
br <-stdres(dof)
cooks.distance(dof)
which(cooks.distance(dof)>3)
ks.test(br, "pnorm")
par(mfrow=c(1,2))
hist(br,xlab="Standard Residuals",main="br Histogram")
plot(dof$fitted.values,dof$residuals,xlab="Fitted Values",ylab="Residuals",main="Resid v.Ftd Vals")
abline(0,0)
par(mfrow=c(1,1))

library(ggplot2)
#graph of data
ggplot(life,aes(y=LifeExp,x=log(PPGDP),color=Group))+ geom_point()


#full model
srl <- lm(LifeExp~log(PPGDP)*Group,data=life)
summary(srl)
#partial model
hlr <- lm(LifeExp~log(PPGDP), data=life)
summary(hlr)
#f-test for interaction between group and log(ppgdp)
anova(srl,hlr)
#confidence interval
confint(srl,level=.95)
#fitted graph
ggplot(life,aes(y=LifeExp,x=log(PPGDP),color=Group))+geom_point()+ geom_smooth(method="lm",formula=y~x,se=FALSE)

