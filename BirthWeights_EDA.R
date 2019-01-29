library(ggplot2)
library(GGally)
library(dplyr)
library(MASS)
library(tidyr)
library(car)
library(lmtest)
library(multcomp)
# put all the libraries I use at the top 
df <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/1%20-%20IID/InClassCaseStudy/Data/BirthWeights.txt', header =  TRUE, sep = ' ')
head(df)
str(df)

# move birthweight to end of dataset 
df <- dplyr::select(df, -BirthWeight, BirthWeight)

# 1
# Scatterplot of BirthWeight by Mage
ggplot(data = df, mapping = aes(x = Mage, y = BirthWeight)) + geom_point()

# Side-by-side boxplots of BirthWeight for each category in Race
ggplot(data = df,mapping = aes(x = Race, y = BirthWeight)) + geom_boxplot()

# A scatterplot of BirthWeight by Gage where the dots are colored according to Gen
ggplot(data = df,mapping = aes(x = Gage, y = BirthWeight)) + geom_point(aes(colour = Gen))

# The correlation between BirthWeight and Mage.
cor(df$BirthWeight, df$Mage)

# A pairs plot of all the variables in the BirthWeight dataset.
ggpairs(df)

# 2 Fitting a Linear model 

# model w/out using lm 
X.matrix <- model.matrix(BirthWeight ~ ., data = df)

y.matrix <- df$BirthWeight %>% as.matrix()
# calculate coefficients & model
modd <- t(solve(t(X.matrix) %*% X.matrix) %*% t(X.matrix) %*% y.matrix)
modd

coeff <- solve(t(X.matrix) %*% X.matrix) %*% t(X.matrix) %*% y.matrix
coeff

# variance of the residuals
n <- nrow(df)
P <- ncol(df) + 1  # plus 1 to include the intercept 
top <- t(y.matrix - (X.matrix %*% coeff)) %*% (y.matrix - (X.matrix %*% coeff))
s2 <- top / (n - P - 1)
s2

# verify model using lm()
weight.mod <- lm(BirthWeight ~ . , data = df)

round(weight.mod$coefficients, 0) == round(coeff, 0)

# variance of residuals
(summary(weight.mod)$sigma)^2

# coefficients
weight.mod$coefficients

# Without the use of lm() calculate the fitted values Xβ̂ 
xB <- X.matrix %*% coeff
t(xB)

# Verify your calculations by pulling off the fitted values from an lm() object.
round(weight.mod$fitted.values, 0) == round(t(xB), 0)

# Without the use of lm() calculate the residuals y−Xβ̂  
residz <- t(y.matrix - (X.matrix %*% coeff))
residz  

# Verify your calculations by pulling off the residuals from an lm() object.
round(weight.mod$residuals, 0) == round(residz, 0)

# Identify your model R2 from the summary() output.
summary(weight.mod)$r.squared

# 3
# Construct added variable plots and assess if the linearity assumption is OK for this data.
avPlots(weight.mod)

# Construct a histogram of the standardized residuals and run a KS-test to see if the normality assumption is OK for this data.
ggplot() + geom_histogram(mapping = aes(x = stdres(weight.mod)))

# run ks test
st.dres <- stdres(weight.mod)
ks.test(st.dres, 'pnorm')

# Draw a scatterplot of the fitted values vs. standardized residuals and run a BP-test to see if the equal variance assumption is OK for this data.
ggplot() + geom_point(mapping = aes(x = fitted(weight.mod), y = resid(weight.mod))) + geom_abline(intercept = 0, slope = 0)

# bp test
bptest(weight.mod)

# 4 - predictions

# Without using predict.lm(), calculate your point prediction of the birth weight for a baby with Mage=26, Gage=37, Race="hisp" and  Gen="Female"
# using the formula ŷ new=xnewβ̂  where β̂  is the maximum likelihood estimate that you calculated above. Confirm that this is what predict.lm() is doing to get the point prediction.

new.row <- data_frame(Mage = 26, Gage = 37, Race = 'hisp', Gen = 'Female')
new.row

new.row.matr <- c(1, 26, 37, 1, 0, 0, 0)

y.hat <- t(new.row.matr) %*% coeff
y.hat

# confirm w/ predict.lm()
predict.lm(weight.mod, new_data = new.row)

# Using predict.lm(), get a prediction of the birth weight for a baby with Mage=26, Gage=37, Race="hisp" and Gen="Female" and an associated 99% prediction interval.
predict.lm(object = weight.mod, newdata = new.row, level = .99, interval = 'prediction')


# cross validation
set.seed(1)
n.cv <- 100
n.test <- round(nrow(df) * .10, 0)
rpmse <- rep(x = NA, times = n.cv)
bias <- rep(x = NA, times = n.cv)
wid <- rep(x = NA, times = n.cv)
cov <- rep(x = NA, times = n.cv)
for (cv in 1:n.cv) {
  # Select test observations
  test.obs <- sample(1:nrow(df), size = n.test)
  test.data <- df[test.obs, ]
  train.data <- df[-test.obs, ]
  
  # fit model to training set 
  test.mod <- lm(BirthWeight ~ . , data = train.data)
  
  # generate predictions 
  preds <- predict.lm(test.mod, newdata = test.data, interval = 'prediction')
  
  # calculate bias
  bias[cv] <- mean(preds[, 'fit'] - test.data[['BirthWeight']])
  
  # calculate rpmse
  rpmse[cv] <- (preds[, 'fit'] - test.data[['BirthWeight']])^2 %>% mean() %>% sqrt()
  
  # calculate coverage 
  cov[cv] <- ((test.data[['BirthWeight']] > preds[, 'lwr']) & (test.data[['BirthWeight']] < preds[, 'upr'])) %>% mean()
  
  # calculate width
  wid[cv] <- (preds[, 'upr'] - preds[, 'lwr']) %>% mean()
}
# calculate our mean bias value
mean(bias)

# histogram of bias 
ggplot() + geom_histogram(mapping = aes(x = bias))

# calculate our mean rpmse value 
mean(rpmse)

# histogram of rpmse
ggplot() + geom_histogram(mapping = aes(x = rpmse))

# calculate our mean coverage value
mean(cov)

# density plot of coverage
ggplot() + geom_density(mapping = aes(x = cov))

# calculate our mean width value 
mean(wid)

# density plot of coverage 
ggplot() + geom_density(mapping = aes(x = wid))

# 4. hypothesis test and confidence intervals

# Using lm() construct the t−statistic and p-value for the test H0:βMage=0.
summary(weight.mod)$coefficients[2, c(3, 4)]

# Using confint() and lm(), build a 90% confidence interval for βMage.
confint(weight.mod, level = .9)[2, ]

# Using anova(), conduct a F test that race has no effect on birth weight (note: this answers primary research question # 2)
# whole model is weight.mod

# reduce model
red.mod <- lm(BirthWeight ~ Mage + Gage + Gen, data = df)

# run anova on whole model vs reduce model
anova(weight.mod, red.mod)

# Using glht(), conduct a t test and 94% confidence interval for the difference in average birth weight of babies 
# born with explantory variables Mage=24, Gage=40, Race="white" and Gen="Male" 
# and babies born with explanatory variables Mage=34, Gage=33,  Race="white" and Gen="Male"
diff.matr <- t(c(1, 24, 40, 0, 0, 1, 1) - c(1, 34, 33, 0, 0, 1, 1))
my.test <- glht(weight.mod, linfct = diff.matr, alternative = 'two.sided')
summary(my.test)

# 94% confidence interval of the difference between the two babies
confint(my.test, level = .94)
