library(ggplot2)
library(GGally)
library(dplyr)
library(MASS)
library(tidyr)
library(car)
library(lmtest)
library(multcomp)
library(caret)
library(GGally)
library(ggpubr)
setwd('~/Downloads')
salary <- read.csv('Salary.csv', sep=',', header=TRUE)
salary <- dplyr::select(salary, -Salary, Salary)
head(salary)
str(salary)
#1) Create exploratory plots and calculate summary statistics from the data. 
# Comment on any potential relationships you see from these exploratory plots.
ggpairs(salary, cardinality_threshold = 16)

# salary vs GPA
ggplot(data=salary, aes(GPA, Salary, col = Gen)) + geom_point() + geom_smooth()
ggplot(data=salary, aes(GPA, Salary)) + geom_point() + geom_smooth()
ggplot(data=salary, aes(GPA, Salary, col = Gen)) + geom_point()


cor(salary$Salary, salary$GPA)

# salary vs MajorCategory
hist(salary$Salary, main = "Histogram of Salary", xlab = "Salary")
hist(salary$GPA, main = "Histogram of GPA", xlab = "GPA")

# major category vs salary- this graph could be improved
ggplot(data=salary, aes(x = MajorCategory, y = Salary, color = MajorCategory)) + geom_boxplot(aes(group = MajorCategory)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
# gender vs salary
ggplot(data = salary, aes(x = Gen, y = Salary, col = Gen)) + geom_boxplot()



# 3 - Using first principles (i.e. DON’T use lm() but you can check your answer with lm()), calculate βˆ and report the estimates in a table. I
# Interpret the coefficient for 1 categorical explanatory variable and the coefficient for GPA. Also calculate the estimate of the residual variance (or standard deviation) and R2 (you can use lm() to get R2).
check_model <- lm(Salary ~ . , salary)

X_matrix <- model.matrix(Salary ~ . , data=salary)
X_matrix
y_matrix <- salary$Salary %>% as.matrix()
y_matrix
# make our model 
salary.model <- t(solve(t(X_matrix) %*% X_matrix) %*% t(X_matrix) %*% y_matrix)
salary.model

# calculate coeffecients 
sal.coeff <- solve(t(X_matrix) %*% X_matrix) %*% t(X_matrix) %*% y_matrix
xtable(sal.coeff)

# calculate residual variance 
P <- length(sal.coeff) - 1 
n <- nrow(salary)
top <- t(y_matrix - (X_matrix %*% sal.coeff)) %*% (y_matrix - (X_matrix %*% sal.coeff ))

resid_var <- top / (n - P - 1)
resid_var
check_residuals <- (summary(check_model)$sigma)^2 
round(check_residuals, 0) == round(resid_var, 0)

summary(check_model)

# calculate r-squared value
summary(check_model)$r.squared

# 4
# One common argument is that some disciplines have greater biases (in terms of lower salaries) towards women than others. 
# To verify this, check for interactions between major and gender by (i) drawing side-by-side boxplots of salary for each major category and gender combination 
# and (ii) running an appropriate hypothesis test (either t or F) to check for significance. Comment on potential gender discrimination from your boxplot. 
# For you hypothesis test, state your hypotheses, report an appropriate test statistic, p-value and give your conclusion.

# part i- draw side by side box-plots w/ gender vs salary 
ggplot(data=salary, aes(x = Gen, y = Salary)) + geom_boxplot(aes(group = Gen))

# draw side by side box-plots w/ discipline vs salary
# make the x labels look better 
ggplot(data = salary, aes(x = MajorCategory, y = Salary,col=Gen)) + geom_boxplot()

# part ii 
interaction_model <- lm(Salary ~ MajorCategory + Gen + GPA + MajorCategory * Gen, data = salary)

anova(interaction_model, check_model)

#5 check assumptions

#check linearity 
avPlots(interaction_model)

#check normality 
stan_residz <- stdres(interaction_model)
ks.test(stan_residz, 'pnorm')
ggplot() + geom_histogram(aes(x = stan_residz))

# check equal variance 
bptest(interaction_model)
ggplot() + geom_point(aes(x = fitted(interaction_model), y = resid(interaction_model))) + geom_abline(intercept = 0, slope = 0)


#6 - Calculate 97% confidence intervals for the coefficients for GPA, Gender and one major category. Interpret each interval.
confint(interaction_model, level=.97)

#7 For the Computers and Mathematics major category, perform a general linear hypothesis test that women, on average, earn less salary than men 
# (for the same GPA). State your hypotheses, p-value and conclusion. If this test is significant, report and estimate a 95% confidence interval 
# for how much more men earn than women in that major category.
mvw_matrix <- t(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.81, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 
                - c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3.81, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

difference_test <- glht(interaction_model, linfct = mvw_matrix, alternative = 'two.sided', level = .95)

summary(difference_test)
confint(difference_test)
# 8 Using predict.lm() and your fitted model, predict your salary and report an associated 95% prediction interval. Interpret this interval in context.
dana_data <- data_frame(MajorCategory = 'Computers & Mathematics', Gen = 'F', GPA = 3.81)

dana_prediction <- predict.lm(interaction_model, newdata = dana_data, interval= 'prediction', level = .95)
dana_prediction

# 9 If we wish to use our model for prediction as we did in #8, we should verify how accurate our predictions are via cross-validation. 
# Conduct a leave-one-out cross validation of the salary data. Report your average RPMSE along with the average prediction interval width. 
# Comment on whether you think your predictions are accurate or not.

set.seed(919)
n.cv <- nrow(salary)
n.test <- 1
rpmse <- rep(x = NA, times = n.cv)
bias <- rep(x = NA, times = n.cv)
wid <- rep(x = NA, times = n.cv)
cov <- rep(x = NA, times = n.cv)
for (cv in 1:n.cv) {
  # Select test observations
  test.obs <- sample(1:nrow(salary), size = n.test)
  test.data <- salary[test.obs, ]
  train.data <- salary[-test.obs, ]
  
  
  # fit model to training set 
  test.mod <- lm(Salary ~ MajorCategory + Gen + GPA + MajorCategory * Gen , data = train.data)
  
  # generate predictions 
  preds <- predict.lm(test.mod, newdata = test.data, interval = 'prediction')
  
  # calculate bias
  bias[cv] <- mean(preds[, 'fit'] - test.data[['Salary']])
  
  # calculate rpmse
  rpmse[cv] <- (preds[, 'fit'] - test.data[['Salary']])^2 %>% mean() %>% sqrt()
  
  # calculate coverage 
  cov[cv] <- ((test.data[['Salary']] > preds[, 'lwr']) & (test.data[['Salary']] < preds[, 'upr'])) %>% mean()
  
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


# 1-4 - me 
# 5-9 dana 



