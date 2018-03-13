#330 Windmill HW
windmill <- read.table("https://mheaton.byu.edu/Courses/Stat330/HomeworkAnalyses/1%20-%20Windmills/Data/Windmill.txt",header=TRUE)
names(windmill)
windmill$CSpd
head(windmill)
str(windmill)
dim(windmill)
cor(windmill$RSpd,windmill$CSpd)
cov(windmill$RSpd,windmill$CSpd)

plot(windmill$RSpd,windmill$CSpd,pch=19,col="blue",ylab="Candidate Wind Speed",xlab="Reference Wind Speed",
     main="Wind Speed by Location")
scatter.smooth(windmill$RSpd,windmill$CSpd,pch=19,col="blue",span=2/3,degree=1,family=c("symmetric","gaussian"),evaluation=50,
              xlab="Reference Wind Speed",ylab="Candidate Wind Speed",main="Wind Speed by Location")

slr <- lm(CSpd~RSpd, data=windmill)
slr
names(slr)
summary(slr)

abline(slr,col="red",lty=2,lwd=1)
coef(slr)
lsum <- summary(slr)
redata <- data.frame(RSpd=c(12))
predict.lm(slr,newdata=redata)

edata <- data.frame(RSpd=c(30))
predict.lm(slr,newdata=edata)