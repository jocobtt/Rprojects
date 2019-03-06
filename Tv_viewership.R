library(ggplot2)
library(GGally)
library(multcomp)
library(tidyr)
library(nlme)
library(dplyr)
library(astsa)
library(splines)
library(multcomp)

tv_df <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/1%20-%20TimeSeries/HWCaseStudy/Data/Viewership.txt', sep = ' ', header = TRUE)
head(tv_df)
anova(tv_df)
###########################
# log transform Viewers 
###########################
tv_df$Viewers <- log(tv_df$Viewers)
head(tv_df)
##################################
# EDA - comment on any potential relationships between log(viewers) and shownum 
##################################
ggplot(data = tv_df, aes(x = ShowNum, y = Viewers)) + geom_line() + ylab('log(Viewers)')

ggplot(data = tv_df, aes(x = ShowNum, y = Viewers)) + geom_line() + geom_smooth() + ylab('log(Viewers)')
ggplot(data = tv_df, aes(x = ShowNum, y = Viewers)) + geom_line() + geom_smooth() + ylab('log(Viewers)')


ggpairs(tv_df)

######################################
# 2 fit a linear regression model to log(viewers) using shownum as the explanatory variable 
# is there a temporal correlation in residuals which should be accounted for in model. 
# discuss what this temporal correlation means for viewership 
######################################
# fit normal model w/o spline and then fit with spline and compare the two 
fir_model <- lm(Viewers ~ ShowNum, data = tv_df)

autto <- acf(fir_model$residuals, lag.max = 30)
autodf <- data.frame(Lag = autto$lag, auto_ACF = autto$acf )
ggplot(data=autodf, aes(x=Lag, y=auto_ACF)) + geom_col()

our_spline <- matrix(tv_df$ShowNum, ncol=1)

tv_linear <- lm(Viewers ~ our_spline, data = tv_df)
summary(tv_linear)
ggplot(data = tv_df, aes(x = ShowNum, y = Viewers)) + geom_line() + geom_smooth(method=lm)

autocorf <- acf(tv_linear$residuals, lag.max = 20)
autodf <- data.frame(Lag = autocorf$lag, auto_ACF = autocorf$acf )
ggplot(data=autodf, aes(x=Lag, y=auto_ACF)) + geom_col()

########################################
# 3 Fixing d=0 and D=1, determine appropriate values of p, q, P, Q in your time series model
# (note you should be able to figure out the seasonal cycle value S).
# Only consider p∈{0,1,2}, q∈{0,1,2}, P∈{0,1} and Q∈{0,1}. Discuss how you came to choose your specific values.
########################################
# add more values - need around 36
pdq.models <- matrix(c(0,0,0,0,1,0,
                       1,0,0,0,1,0,
                       0,0,1,0,1,0,
                       0,0,0,1,1,0,
                       0,0,0,0,1,1,
                       1,0,1,1,1,1,
                       2,0,1,1,1,1,
                       1,0,2,1,1,1,
                       0,0,2,0,1,0,
                       2,0,0,0,1,0,
                       2,0,1,0,1,0,
                       2,0,0,1,1,0,
                       2,0,0,0,1,1,
                       0,0,2,0,1,0,
                       1,0,2,0,1,0,
                       0,0,2,1,1,0,
                       0,0,2,0,1,1
                       ), ncol = 6, byrow = TRUE)
# p=1 is how it should be 
# maybe need more considerations/can read in the permutations in a more code like way
aic.pdqs <- rep(NA, nrow(pdq.models))
for (m in 1:nrow(pdq.models)) {
  my.model <- sarima(tv_df$Viewers, p = pdq.models[m,1],
                     d=pdq.models[m,2],
                     q=pdq.models[m,3],
                     P=pdq.models[m,4],
                     D=pdq.models[m,5],
                     Q=pdq.models[m,6],
                     S=10,
                     xreg=bs(tv_df$ShowNum, knots = 60, degree = 1), details=FALSE)
  aic.pdqs[m] <- my.model$AIC
}
pdq.models[which.min(aic.pdqs), ]


#######################################
# 4 Write down your selected time series regression model in terms of population parameters including your specification for the time series component of the residuals.
# Explain the meaning of any parameters in your model (including the time series components). Explain how statistical inference for your model can be used to predict the viewership moving forward.
#######################################
### write model here
###
###

#######################################
# 5 Fit your chosen time series model and validate any model assumptions you used.
#######################################
our_pdq <- sarima(tv_df$Viewers, p = 2, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 10, 
                  xreg = our_spline, details = FALSE)


our_pdq$ttable

# check assumptions 
fit <- resid(our_pdq$fit) - tv_df$Viewers

# check normality 
ggplot() + geom_density(aes(x = resid(our_pdq$fit)))

# check equal variance 
ggplot() + geom_point(aes(y = resid(our_pdq$fit), x = fit))

# check independence
tvacf <- acf(resid(our_pdq$fit), lag.max = 20)
tvdf <- data.frame(tv_lag = tvacf$lag, tv_ACF = tvacf$acf )
ggplot(data=tvdf, aes(x=tv_lag, y=tv_ACF)) + geom_col()

###################################
# 6 Perform a cross-validation of predictions generated from your model for the most recent season of shows. Report the quality of your predictions in terms of RPMSE.
# Provide a plot of your predictions along with observed viewership and 95% prediction interval limits.
###################################


# split into test and train data sets
test_data <- tv_df[-(1:(nrow(tv_df)-10)),]
train_data <- tv_df[1:(nrow(tv_df)-10),]
x_spline_train <- our_spline[1:(nrow(tv_df)-10),]
x_spline_test <- our_spline[-(1:(nrow(tv_df)-10)),]

# get predictions
preds <- sarima.for(train_data$Viewers, p = 1, d = 0, q = 1, P = 1, D = 1, Q = 1, S = 10, xreg = x_spline_train, n.ahead = 10, newxreg = x_spline_test)
# make prediction intervals
lower_preds <- preds$pred - qt(1-.05/2, df = nrow(train_data) - nrow(our_pdq$ttable)) * preds$se

upper_preds <- preds$pred + qt(1-.05/2, df = nrow(train_data) - nrow(our_pdq$ttable)) * preds$se
matrix(c(lower_preds, preds$pred, upper_preds), ncol = 3, byrow = TRUE)

# calculate coverage 
cov <- ((test_data[['Viewers']] > lower_preds) & (test_data[['Viewers']] < upper_preds)) %>% mean()
cov
# calculate RPMSE
rpmse <- (preds$pred - test_data[['Viewers']])^2 %>% mean() %>% sqrt()
rpmse

#graph - take from the sarima.for function 

##################################
# 7 Determine if viewership is increasing or decreasing. Support your conclusions with appropriate hypothesis tests and confidence intervals.
##################################
our_pdq$ttable

# should we use 1 or 2 as our coeffiecient?
lower_lim <- our_pdq$ttable['ar1', 'Estimate'] - qt(1-.05/2, df = nrow(tv_df) - nrow(our_pdq$ttable)) * our_pdq$ttable['ar1', 'SE']

upper_lim <- our_pdq$ttable['ar1', 'Estimate'] + qt(1-.05/2, df = nrow(tv_df) - nrow(our_pdq$ttable)) * our_pdq$ttable['ar1', 'SE']

CI <- c(lower_lim, our_pdq$ttable['ar1', 'Estimate'], upper_lim)
CI

#################################
# 8 Season 8 is already in production. Forecast the log(Viewers) forward for season 8. Comment on how executives would be able to use these forecasts to gauge if the show should continue into a ninth season.
#################################
Xpred <- matrix(max(tv_df$ShowNum) + 1:10, nrow = 10)
srm.for <- sarima.for(tv_df$Viewers, p = 2, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 10, xreg = our_spline, n.ahead = 10, newxreg = Xpred)

lower.pred <- srm.for$pred - qt(1-.05/2, df = nrow(tv_df) - nrow(our_pdq$ttable)) * srm.for$se

upper.pred <- srm.for$pred + qt(1-.05/2, df = nrow(tv_df) - nrow(our_pdq$ttable)) * srm.for$se

matrix(c(lower.pred, srm.for$pred, upper.pred), ncol = 3, byrow = TRUE)

# i'll do first 4 
# dana does last 4 