library(GGally)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(tidyr)
library(MASS)
library(lmtest)
library(nlme)
library(multcomp)
setwd("~/Stat_469/predictgls")
source("https://raw.githubusercontent.com/MJHeaton/predictgls/master/predictgls.R")
path <- "https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/2%20-%20Diagonal/InClassCaseStudy/Data/MovieRevenue.csv"
movie.rev <- read.csv(path, header = TRUE, sep = ',')
head(movie.rev)
str(movie.rev)

#exploratory data analysis
# 1) Draw a scatterplot of DomesticGross by ProductionBudget. Add a smooth line to gauge how linear the relationship is.
ggplot(data = movie.rev, aes(x = ProductionBudget, y = DomesticGross)) + geom_point() + geom_smooth()
# calculate the correlation
cor(movie.rev$ProductionBudget, movie.rev$DomesticGross)

# 2) Convert ReleaseDate to a date object, extract the month and plot side-by-side boxplots of  DomesticGross for each month.
# domestic gross by month 

movie.rev$ReleaseDate <- as.Date(movie.rev$ReleaseDate)
movie.rev$Month <- month(movie.rev$ReleaseDate)
# fix this plot 
ggplot(data = movie.rev, aes(x = Month, y = DomesticGross)) + geom_boxplot(aes(group = Month))

# 3) Draw a scatterplot of log(DomesticGross) by log(ProductionBudget). Add a smooth line to gauge how linear the relationship is.
ggplot(data = movie.rev, aes(x = log(ProductionBudget), y = log(DomesticGross))) + geom_point() + geom_smooth()

# analysis with an MLR 
# 1) Fit a MLR model and build a 95% confidence interval for the effect of ProductionBudget on DomesticGross (this is the first attempt at answering research question #1).
movie.rev$Month <- as.factor(movie.rev$Month)
movie.model <- lm(DomesticGross ~ ProductionBudget + Month, data = movie.rev)
summary(movie.model)
confint(movie.model, level = .95)

# 2) Get a prediction for each movie in your dataset and identify the 5 movies that were the most above the predicted value. Also identify the 5 movies that were the most below the predicted value.
predRev <- predict.lm(movie.model)

# top 5 highest
movie.rev[order(movie.rev$DomesticGross - predRev, decreasing = TRUE)[1:5], ]

# top 5 lowest
movie.rev[order(movie.rev$DomesticGross - predRev, decreasing = FALSE)[1:5], ]

# 3) Show that the assumptions of the MLR model are not met (and hence statistical inference using an MLR model are not valid) by drawing a scatterplot of fitted vs. standardized residuals 
# and a histogram of the standardized residuals.

# check normality
movie.residz <- stdres(movie.model)
ks.test(movie.residz, 'pnorm')
ggplot() + geom_histogram(aes(x = movie.residz))

# check linearity 
ggplot(data = movie.rev, aes(x = ProductionBudget, y = DomesticGross)) + geom_point() + geom_smooth()

#check equal variance 
bptest(movie.model)
ggplot() + geom_point(aes(x = fitted(movie.model), y = residuals(movie.model)))

# 4) Try 2 or 3 transformations of DomesticGross and/or ProductionBudget and show that these transformations are not going to fix the assumptions of the MLR model.
# try log-log transformation
log.movie <- lm(log(DomesticGross) ~ log(ProductionBudget) + Month, data = movie.rev)
bptest(log.movie)
ggplot() + geom_point(aes(x = fitted(log.movie), y = residuals(log.movie)))

# try sqrt(DomesticGross) transformation
sqrt.model <- lm(sqrt(DomesticGross) ~ ProductionBudget + Month, data = movie.rev)
bptest(sqrt.model)
ggplot() + geom_point(aes(x = fitted(sqrt.model), y = residuals(sqrt.model)))

# Fitting a linear model with heteroskedasticity 
# 1) Fit a heteroskedastic MLR model to the movie data where log(DomesticGross) is the response,  
# log(ProductionBudget) is the explanatory variable and we use log(ProductionBudget) is the covariate in an exponential variance function.
# Identify the estimates of βˆ, θˆ and s.

gls.movie <- gls(model = log(DomesticGross) ~ log(ProductionBudget), data = movie.rev, 
                 weights = varExp(form= ~ log(ProductionBudget)), method = 'ML')

summary(gls.movie)
gls.movie$coefficients  # beta hat
gls.movie$modelStruct # theta hat
gls.movie$sigma  # sigma

# Validating your Heteroskedastic MLR model 
# 1) Check the L-I-N-E assumptions using the standardized residuals from your heterogeneous MLR fit in the previous subsection.

# check linearity
ggplot(data = movie.rev, aes(x = log(ProductionBudget), y = log(DomesticGross))) + geom_point() + geom_smooth()

# check normality 
ggplot() + geom_histogram(aes(x = resid(gls.movie, type = "pearson")))

# check equal variance 
ggplot() + geom_point(aes(x = fitted(gls.movie), y = resid(gls.movie, type = "pearson")))

# 2) Modify the cross-validation code from the birth weight analysis to run a cross-validation of your 
# heterogeneous MLR using the predictgls() function. Report the bias, RPMSE, coverage and width of prediction intervals.

# pull out production budget and domestic gross into its own dataset 
cv.df <- movie.rev %>% dplyr::select(3:4)
set.seed(808)
n.cv <- 100
n.test <- round(nrow(movie.rev) * .25)
rpmse <- rep(x = NA, times = n.cv)
bias <- rep(x = NA, times = n.cv)
wid <- rep(x = NA, times = n.cv)
cov <- rep(x = NA, times = n.cv)
for (cv in 1:n.cv) {
  # Select test observations
  test.obs <- sample(1:nrow(movie.rev), size = n.test)
  test.data <- cv.df[test.obs, ]
  train.data <- cv.df[-test.obs, ]
  
  # fit model to training set 
  test.mod <- gls(model = log(DomesticGross) ~ log(ProductionBudget), data = train.data, 
                  weights = varExp(form = ~ log(ProductionBudget)), method = 'ML')
  
  # generate predictions 
  predd <- predictgls(test.mod, newdframe = test.data)
  
  #upper and lower prediction intervals
  pred.low <- exp(predd$Prediction - qt(1-.05/2, df = nrow(train.data) - length(coef(test.mod))) * predd$SE.pred)
  pred.up <- exp(predd$Prediction + qt(1-.05/2, df = nrow(train.data) - length(coef(test.mod))) * predd$SE.pred)
  # calculate bias
  bias[cv] <- mean(exp(predd[, 3]) - test.data[['DomesticGross']])
  
  # calculate rpmse
  rpmse[cv] <- (exp(predd[, 3]) - test.data[['DomesticGross']])^2 %>% mean() %>% sqrt()
  
  # calculate coverage 
  cov[cv] <- ((test.data[['DomesticGross']] > pred.low) & (test.data[['DomesticGross']] < pred.up)) %>% mean()
  
  # calculate width
  wid[cv] <- (pred.up - pred.low) %>% mean()
}
# calculate our mean bias value
mean(bias)

# calculate our mean rpmse value 
mean(rpmse)

# calculate our mean coverage value
mean(cov)

# calculate our mean width value 
mean(wid)

# 3) 
# For each movie in your dataset, construct a 99% prediction interval for  DomesticGross (don’t forget to backtransform your prediction). Identify the top 5
# movies where the observed DomesticGross was above the interval. Also identify the bottom 5 movies where the observed DomesticGross was below the interval (this answers research question #3).
prediction.gls <- predictgls(gls.movie, movie.rev)

exp.prediction <- exp(prediction.gls$Prediction)
movie.upper <- exp(prediction.gls$Prediction + qt(1-.01/2, df = nrow(movie.rev) - length(coef(gls.movie))) * prediction.gls$SE.pred)

movie.lower <- exp(prediction.gls$Prediction - qt(1-.01/2, df = nrow(movie.rev) - length(coef(gls.movie))) * prediction.gls$SE.pred)
# top 5 
movie.rev[order(movie.rev$DomesticGross - movie.upper, decreasing = TRUE)[1:5], ]
# bottom 5 
movie.rev[order(movie.rev$DomesticGross - movie.lower, decreasing = FALSE)[1:5], ]

# hypothesis testing and confidence intervals under heteroskedasticity 
# 1) Carry out a hypothesis test that βPB=0. Report the p-value and draw an appropriate conclusion.
summary(gls.movie)$tTable

# 2) Carry out a hypothesis test that βPB=1. Report the p-value and draw an appropriate conclusion.
glht(gls.movie, linfct = matrix(c(0, 1), nrow = 1), rhs = 1, alternative = "greater") %>% summary()

# 3) Construct a 95% confidence interval for βPB (this answers research question #1).
confint(gls.movie, level = .95)[2, ]

# 4) Construct a 95% confidence interval for θ in your variance function. Draw a conclusion 
# about the variability of log(DomesticGross) as a function of  log(ProductionBudget) (this answer research question #2).
intervals(gls.movie, level = 0.95)$varStruct
