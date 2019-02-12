library(ggplot2)
library(GGally)
library(multcomp)
library(tidyr)
library(MASS)
library(nlme)
library(dplyr)
library(gridExtra)
library(lmtest)
source("https://raw.githubusercontent.com/MJHeaton/predictgls/master/predictgls.R")
path <- "https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/2%20-%20Diagonal/HWCaseStudy/Data/FoodExpenses.txt"
food.exp <- read.table(path, sep = ' ', header = TRUE)
head(food.exp)
str(food.exp)
# 1) Create exploratory plots and calculate summary statistics from the data. Comment on any potential relationships you see between Income and EatingOut.
# income vs eating out
ggplot(data = food.exp, aes(x = Income, y = EatingOut)) + geom_point() + geom_smooth() 

# correlation of income vs eating out
cor(food.exp$Income, food.exp$EatingOut)
par(mfrow = c(1,2))
hist(food.exp$Income, xlab = 'Income', main = 'Income')
hist(food.exp$EatingOut, xlab = 'EatingOut', main = 'EatingOut')


# 2) Using a homoskedastic linear model, fit a regression model to EatingOut using Income as the explanatory variable. Determine if the equal variance assumption is met.
# If it not met, discuss what impact the violation of this assumption could have on an analysis on the relationship between income and food expenditure.

hskd.model <- lm(EatingOut ~ Income, data = food.exp)
summary(hskd.model)

# check assumptions 
# linearity 
ggplot(data = food.exp, aes(x = Income, y = EatingOut)) + geom_point() + geom_smooth() 

# check normality 
ks.test(stdres(hskd.model), "pnorm")
ggplot() + geom_histogram(aes(x = stdres(hskd.model)))

# check equal variance
bptest(hskd.model)
ggplot() + geom_point(aes(y = residuals(hskd.model), x = fitted(hskd.model)))


# 3 code for our model in matrix and vector form 



# 4 Fit your model from #3 to EatingOut. Validate the model L-I-N-E assumptions so you will be confident that the statistical inference you perform below will be correct.
food.model <- gls(model = EatingOut ~ Income, data = food.exp, weights = varExp(form = ~ Income),
                  method = "ML")
summary(food.model)
food.model$coefficients
# check linearity 
ggplot(data = food.exp, aes(x = Income, y = EatingOut)) + geom_point() + geom_smooth() 

# check normality 
ks.test(resid(food.model, type = "pearson"), "pnorm")
ggplot() + geom_histogram(aes(x = resid(food.model, type = "pearson")))

# check equal variance 
ggplot() + geom_point(aes(y = resid(food.model, type = "pearson"), x = fitted(food.model)))

# 5 Validate your predictions based on your model in #3 via cross-validation (any of leave-one-out, Monte Carlo or K-fold). Report your model RPMSE and coverage.
# Additionally, show your predictions and 95% prediction interval bounds on a scatterplot of income vs. food expenditure.

n.cv <- 500
n.test <- round(nrow(food.exp)*.25, 0)
bias <- rep(x = NA, times = n.cv)
rpmse <- rep(x = NA, times = n.cv)
cov <- rep(x = NA, times = n.cv)
wid <- rep(x = NA, times = n.cv)
for (cv in 1:n.cv) {
  # split into test and training sets
  test.observations <- sample(1:nrow(food.exp), size = n.test)
  test.data <- food.exp[test.observations, ]
  train.data <- food.exp[-test.observations, ]
  # fit model 
  test.mod <- gls(EatingOut ~ Income, data = train.data,
                  weights = varExp(form = ~ Income), method = "ML")
  # generate predictions 
  predd <- predictgls(test.mod, newdframe = test.data)
  
  #upper and lower prediction intervals
  pred.low <- predd$Prediction - qt(1-.05/2, df = nrow(train.data)-length(coef(test.mod))) * predd$SE.pred
  pred.up <- predd$Prediction + qt(1-.05/2, df = nrow(train.data)-length(coef(test.mod))) * predd$SE.pred
  # calculate bias
  bias[cv] <- mean(predd[, 3] - test.data[['EatingOut']])
  
  # calculate rpmse
  rpmse[cv] <- (predd[, 3] - test.data[['EatingOut']])^2 %>% mean() %>% sqrt()
  
  # calculate coverage 
  cov[cv] <- ((test.data[['EatingOut']] > pred.low) & (test.data[['EatingOut']] < pred.up)) %>% mean()
  
  # calculate width
  wid[cv] <- (pred.up - pred.low) %>% mean()
    
}

mean(bias)
mean(rpmse)
mean(cov)
mean(wid)

pred_food <- predictgls(food.model, newdframe = food.exp)
pred_food_up <- pred_food$Prediction + qt(1-.05/2, df = nrow(food.exp)-length(coef(food.model))) * pred_food$SE.pred
pred_food_low <- pred_food$Prediction - qt(1-.05/2, df = nrow(food.exp)-length(coef(food.model))) * pred_food$SE.pred

# plot prediction interval bounds on scatter plot of income vs food expenditure 
ggplot(data = food.exp, aes(x = Income, y = EatingOut)) + geom_point() + geom_line(data = pred_food, aes(x = pred_food$Income, y = pred_food$Prediction)) + 
    geom_line(data = pred_food, aes(x = pred_food$Income, y = pred_food_low), col = 'green') + geom_line(data = pred_food, aes(x = pred_food$Income, y = pred_food_up), col = 'green')

# 6 Report βˆinc along with a 95% confidence interval for the model in #4. Report any variance p
# arameters (including the variance function parameters) along with appropriate 95% confidence intervals. Correctly interpret all intervals in context.
food.model$coefficients[2]
confint(food.model)[2,]
food.model$sigma

# find our theta value and its 95% CI 
intervals(food.model, .95)[2]

# find our sigma value and its 95% CI
intervals(food.model, .95)[3]

# 7 Economists with the National Restaurant Association (which, perhaps unfortunately, shares its acronym with another institution),
# hypothesize that a “healthy” restaurant economy should see increases of about $0.50 or more per week for each $1000 increase in income. 
# Using your heteroskedastic model, test if the economy is “healthy” for restaurant owners. State your hypotheses, p-value and an appropriate conclusion.

glht(food.model, linfct = matrix(c(0,1), nrow = 1), rhs = .5, alternative = "less") %>% summary()



# 8 Predict how much you will be spending at restaurants for your desired income level upon graduation (meaning at your first job).
# Report a 95% prediction interval and interpret the interval in context.
new.job <- data.frame(Income = 75)
pred_me <- predictgls(food.model, newdframe = new.job)
pred_me
upper_pred <- pred_me$Prediction + qt(1-.05/2, df = nrow(food.exp) - length(coef(food.model))) * pred_me$SE.pred
upper_pred
lower_pred <- pred_me$Prediction - qt(1 - .05/2, df= nrow(food.exp) - length(coef(food.model))) * pred_me$SE.pred
lower_pred

