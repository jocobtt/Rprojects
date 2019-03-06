library(ggplot2)
library(GGally)
library(multcomp)
library(tidyr)
library(nlme)
library(dplyr)
library(astsa)
library(splines)
library(multcomp)


climate <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/1%20-%20TimeSeries/InClassCaseStudy/Data/AnnAvgGlobalClimate.txt', header = TRUE, sep = ' ')
head(climate)
str(climate)

##########################
##         EDA         ##
##########################
# 1 create new yrmon variable 
climate$YrMon <- climate$Year + (climate$Month - 0.5)/12

# 2 Draw a time series plot of the temperature anomalies with YrMon along the x-axis with a smooth curve overlaid to emphasize the non-linear aspect of the data.

ggplot(data = climate, aes(x = YrMon, y = AnnAnom)) + geom_line() + geom_smooth()

# 3 Calculate and draw the ACF of raw temperature nomalies to show the strong seasonal correlation in the data
# ( you may have to increase lag.max to show 2 or 3 seasonal cycles)
reg.mod <- lm(AnnAnom ~  YrMon , data = climate)
clim.acf <- acf(climate$AnnAnom, lag.max = 36)

acf.frame <- data.frame(Lag = clim.acf$lag, our.ACF = clim.acf$acf)
# fix this part 
ggplot(data = acf.frame, aes(x = Lag, y = our.ACF)) + geom_col()

###########################################
# fitting splines and time series models #
###########################################

# 1 Using bs(), define an X matrix for a linear spline for YrMon with a knot location at 1975.
# Specify the boundaries to be the minimum of YrMon and 60 months past the maximum of  YrMon (because research question #2 asks us to predict forward 60 months past the end of our data).
# You can use, Boundary.knots = c(min(YrMon), max(YrMon)+60*(1/12)) where one month is 1/12.

x.spline <- bs(x = climate$YrMon, knots = 1975, degree = 1, Boundary.knots = c(min(climate$YrMon), max(climate$YrMon) + 60 * (1 / 12)))

# 2 Using the X you created via linear spline, fit a linear model to fit a linear spline to the annual temperature anomalies (i.e. do lm(y~X, data=)).
# Create a plot of the fitted regression line on top of a time series plot to verify that the linear spline fits the data well.

spline.mod <- lm(AnnAnom ~ x.spline, data = climate)
summary(spline.mod)

# plot regression line 
ggplot(data = climate, aes(x = YrMon, y = AnnAnom)) + geom_line()  + geom_line(data = climate, aes(x = climate$YrMon, y = predict.lm(spline.mod)), col = 'blue')
ggplot(data = climate, aes(x = YrMon, y = AnnAnom)) + geom_line()  + geom_smooth(method = lm, formula = y~ bs(x, knots = 1975, degree = 1), se = FALSE)

# 3 Draw an ACF of the residuals from your model in #2 to verify that there is indeed still temporal correlation in the residuals that we will need to model.
autocorf <- acf(spline.mod$residuals, lag.max = 36)
autodf <- data.frame(Lag = autocorf$lag, auto.ACF = autocorf$acf )
ggplot(data=autodf, aes(x=Lag, y=auto.ACF)) + geom_col()


# 4 Using sarima() choose which p, d, q, P, D, Q to use by comparing AIC or BIC values for the following models.
# Don’t forget that one seasonal cycle is 12 months so S=12 and make sure to use your linear spline as the xreg= value (hint: try to set this up in a for-loop).

ts.models <- matrix(c(0,0,0,0,0,0,
                      1,0,0,0,0,0,
                      0,0,1,0,0,0,
                      0,0,0,1,0,0,
                      0,0,0,0,0,1,
                      1,0,0,1,0,0,
                      0,0,1,0,0,1,
                      1,0,1,1,0,1,
                      0,1,0,0,0,0,
                      1,1,0,0,0,0,
                      0,1,1,0,0,0,
                      0,1,0,1,0,0,
                      0,1,0,0,0,1,
                      1,1,0,1,0,0,
                      0,1,1,0,0,1,
                      1,1,1,1,0,1,
                      0,0,0,0,1,0,
                      1,0,0,0,1,0,
                      0,0,1,0,1,0,
                      0,0,0,1,1,0,
                      0,0,0,0,1,1,
                      1,0,0,1,1,0,
                      0,0,1,0,1,1,
                      1,0,1,1,1,1
                      ), ncol=6, byrow=TRUE)

AIC.vals <- rep(NA, nrow(ts.models))
for (m in 1:nrow(ts.models)) {
  my.model <- sarima(climate$AnnAnom, p = ts.models[m,1],
                     d=ts.models[m,2],
                     q=ts.models[m,3],
                     P=ts.models[m,4],
                     D=ts.models[m,5],
                     Q=ts.models[m,6],
                     S=12,
                     xreg=bs(climate$YrMon, knots = 1975, degree = 1), details=FALSE)
  AIC.vals[m] <- my.model$AIC
}
ts.models[which.min(AIC.vals), ]

# best one is p, q, P, Q and d all set to 1, w/ BIC
best.mod <- sarima(climate$AnnAnom, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12, xreg = bs(climate$YrMon, knots = 1975, degree=1), details = FALSE)

# 5 Fit your chosen model and examine the model estimates via the ttable object within a  sarima() fit.
best.mod$ttable

#######################
# Model Validation  #
#######################

# 1 Verify your assumptions of independence, normality and equal variance by drawing an ACF of the decorrelated residuals, a fitted values vs.
# residual plot and a histogram (or density plot) of the decorrelated residuals.

fit <- resid(best.mod$fit) - climate$AnnAnom
# normality 
ggplot() + geom_histogram(aes(x= resid(best.mod$fit)))
# equal variance
ggplot() + geom_point(aes(y = resid(best.mod$fit), x = fit))
# check independence 
timeacf <- acf(resid(best.mod$fit), lag.max = 12)
timedf <- data.frame(time.lag = timeacf$lag, time.ACF = timeacf$acf )
ggplot(data=timedf, aes(x=time.lag, y=time.ACF)) + geom_col()

# 2 Validate your predictions by performing a cross valdation. Split the last 60 time periods in your data into a test set and use the remaining as a training set
# (note you’ll have to split your X matrix too). Fit your best model to the training set and predict the values in the test set. Calculate RPMSE and coverage.


# split into test and train data sets
test.data <- climate[-(1:(nrow(climate)-60)), ]
train.data <- climate[1:(nrow(climate)-60), ]
x.spline.train <- x.spline[1:(nrow(climate)-60), ]
x.spline.test <- x.spline[-(1:(nrow(climate)-60)), ]

# get predictions
preds <- sarima.for(train.data$AnnAnom, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12, xreg = x.spline.train, n.ahead = 60, newxreg = x.spline.test)

# make prediction intervals
lower.preds <- preds$pred - qt(1-.05/2, df = nrow(train.data) - nrow(best.mod$ttable)) * preds$se
  
upper.preds <- preds$pred + qt(1-.05/2, df = nrow(train.data) - nrow(best.mod$ttable)) * preds$se
# calculate coverage 
cov <- ((test.data[['AnnAnom']] > lower.preds) & (test.data[['AnnAnom']] < upper.preds)) %>% mean()
cov
# calculate RPMSE
rpmse <- (preds$pred - test.data[['AnnAnom']])^2 %>% mean() %>% sqrt()
rpmse

###############################
# Statistical Inference      #
###############################

# 1 Identify the p-value for a test that H0:β2=0 vs HA:β2>0. Also, calculate a 95% confidence interval for β2.
best.mod$ttable['2', 'p.value']

lower.beta <- best.mod$ttable['2', 'Estimate'] - qt(1-.05/2, df = nrow(climate) - nrow(best.mod$ttable)) * best.mod$ttable['2', 'SE']

upper.beta <- best.mod$ttable['2', 'Estimate'] + qt(1-.05/2, df = nrow(climate) - nrow(best.mod$ttable)) * best.mod$ttable['2', 'SE']

CI <- c(lower.beta, upper.beta)
CI
# 2 Predict the temperature anomalies forward 60 months (5 years). To do this, you will have to set up your linear spline forward in time.
# If X is your linear spline from bs(), a basic outline for doing this is as follows:

pred.yrmon <- max(climate$YrMon) + seq(1/12, 60 * (1/12), by=1/12)
Xpred <- predict(x.spline, newx = pred.yrmon)
my.for <- sarima.for(climate$AnnAnom, p = 1, d = 1, q = 1, P = 1, D = 0, Q = 1, S = 12, xreg = x.spline, n.ahead = 60, newxreg = Xpred)

lower.for <- my.for$pred - qt(1 - .05/2, df = nrow(train.data) - nrow(best.mod$ttable)) * my.for$se

upper.for <- my.for$pred + qt(1 - .05/2, df = nrow(train.data) - nrow(best.mod$ttable)) * my.for$se
c(lower.for, my.for$pred, upper.for)



