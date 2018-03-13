load("/Users/JTBras/Downloads/Bikes.RData")
head(bikes)
#2
par(mfrow=c(1,2))
boxplot(cnt~season,data=bikes,col="blue",xlab="Season",ylab="# of bikes rented")
boxplot(cnt~yr,data=bikes,col="red",xlab="Year",ylab="# of bikes rented")
boxplot(cnt~holiday,data=bikes,col="maroon",xlab="Holiday",ylab="# of bikes rented")
boxplot(cnt~workingday,data=bikes,col="black",xlab="Workingday",ylab="# of bikes rented")
par(mfrow=c(1,1))
boxplot(cnt~weathersit,data=bikes,col="blue",xlab="Weather",ylab="# of bikes rented")
hist(bikes$cnt,xlab="# of bikes rented")
#3
library(bestglm)
best.bike <- bestglm(bikes,family=poisson,method="exhaustive",IC="AIC")
rmod <- best.bike$BestModel
rmod
#5
int <- confint(rmod,level=.95)
int
tran <- (exp(int)-1)
ri <- 100*tran
library(xtable)
newobject<-xtable(ri)
print.xtable(newobject, type="html", file="filename.html")

#5 assumptions are log linear and independent
#log linear 
#av plots? 
par(mfrow=c(1,3))
scatter.smooth(bikes$windspeed,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="windspeed")
scatter.smooth(bikes$hum,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="hum")
scatter.smooth(bikes$temp,log(bikes$cnt+1),ylab="log(cnt)+1",xlab="temp")


#6 
day <- data.frame(season= "Spring", yr="2012", holiday="No", workingday="Yes",
                  weathersit="Misty", temp=0.34, hum=0.80, windspeed=0.18)
l.mean <- predict.glm(rmod,newdata=day,se.fit=TRUE)
l.mean
exp(l.mean$fit)
int.low <- l.mean$fit - qnorm(.975)*l.mean$se.fit
int.low
int.up <- l.mean$fit + qnorm(.975)*l.mean$se.fit
int.up
interval <- c(exp(int.low),exp(int.up))
interval
#check interval if correct or not