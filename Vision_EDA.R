# Macular Degeneration EDA
library(cowplot)
library(ggplot2)
library(GGally)
library(nlme)
library(car)
library(multcomp)
library(tidyverse)
library(mvtnorm)
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R')

ARMD <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/2%20-%20Longitudinal/InClassCaseStudy/Data/ARMD.txt',
                   header = TRUE, sep = ' ')
head(ARMD)
str(ARMD)
ggpairs(ARMD)
#############################
#         EDA               #
#############################
# 1 change trt to factor within the dataset
ARMD$Trt <- as.factor(ARMD$Trt)

# 2 scatter plot of baseline vs vision
ggplot(data = ARMD, aes(x = Baseline, y = Vision)) + geom_point()

# scatter plot of time by vision colored by trt
ggplot(data = ARMD, aes(x = Time, y = Vision, col = Trt)) + geom_point() + geom_smooth(method = lm, aes(group = Trt))

###########################
# Analysis with an MLR    #
###########################
# 1 Verify that the residuals of an independent MLR with a baseline effect and an interaction between 
# Time and Trt are indeed correlated by calculating the 4×4 correlation matrix Rˆ between residuals of the same person but between visits (see page 14 of the slides). 
# fit a model 
armd.mlr <- lm(Vision ~ Time + Baseline + Trt + Time*Trt, data = ARMD)
summary(armd.mlr)
r.hat <- matrix(armd.mlr$residuals, nrow = 50, ncol=4, byrow=TRUE)
visits.r <- cor(r.hat)
visits.r
###########################
# Longitudal MLR model &  #
# iterative optimization  #
###########################

# 1 Fit a linear regression model for Vision using a linear effect for Baseline,  Time and Trt as well as the interaction of Time and Trt.
# In your model use a block diagonal general symmetric correlation structure within Subject but independent between Subject (i.e. form=~1|Subject or  form=~1:4|Subject).
# Identify the constrained estimates of the general correlation structure along with any βˆ coefficients and the estimate of the variance parameter σ̂ 2.
# check if this is right or not
gensym.mod <- gls(model = Vision ~ Baseline + Time*Trt, data = ARMD, 
               correlation = corSymm(form = ~1:4|Subject), method = "ML")

# beta_hat 
gensym.mod$coefficients
# sigma_hat squared
coef(gensym.mod$modelStruct, unconstrained = FALSE)



# 2# Simulate 100 values of y_i from rnorm(100, mean = 17, sd = sqrt(5)), write a function in R that evalueates the log-liklihood of mu and then maximizes this 
# function using optim() from a starting value of mu_hat = 13. Verify that the iterative optimization routine returns a value near y_bar. 
# use dnrom(y_i, mean = mu, sd = sqrt(5), log = TRUE)
# fix this 
y.i <- rnorm(100, mean = 17, sd = sqrt(5))

l.u <- function(mu=13) {
  sum(dnorm(x = y.i, mu, sqrt(5), log = TRUE))
}

opts <- optim(par = c(13), fn = l.u, method = "BFGS", control = list(fnscale = -1))
opts$par

# 3 
X.matrix <- model.matrix(Vision ~ Baseline + Time*Trt, data = ARMD)

log.betas <- function(beta){
  dmvnorm(x=ARMD$Vision, mean=X.matrix%*%beta, sigma=(6.807^2)*diag(nrow(X.matrix)), log=TRUE)
}

y.bar.i <- mean(ARMD$Vision)
beta.opts <- optim(par = c(y.bar.i,0,0,0,0), fn = log.betas, method = "BFGS", control = list(fnscale = -1))
beta.opts
coef(armd.mlr)
# 4 Using myFunction below (which is bimodal), use optim() to find the maximum with starting values of -4 and 4.
# Show that when you start at -4, you get stuck in a local mode and fail to find the global maximum.

myFunction <- function(x){
  0.25*dnorm(x,-2,1)+0.75*dnorm(x,2,1)
}
# do for loop to calculate -4:4 values here

optim(c(-4), fn = myFunction, method = "BFGS", control = list(fnscale=-1))
optim(c(4), fn = myFunction, method = "BFGS", control = list(fnscale=-1))


############################
# Validating Longitudinal  #
# MLR assumptions          #
############################

# 1 check linearity assumption
car::avPlots(armd.mlr)

# 2 check independence assumption by decorrelating residuals and looking at the sample correlation matrix of w/in subject residuals
sres <- stdres.gls(gensym.mod)
s.resids <- matrix(sres, nrow = 50, ncol = 4, byrow = TRUE)
cor(s.resids)
# 3 check normality w/ histogram of decorrelated residuals 
ggplot() + geom_histogram(aes(x = sres))

# 4 check equal variance w/ decorrelated residuals vs fitted values 
ggplot() + geom_point(aes(y = sres, x = fitted(gensym.mod)))


###########################
# Statistical Inference   # 
###########################
# 1 Use a general linear hypothesis test on your longitudinal MLR to test if patients on the treatment have a significantly higher vision score at 52 than those not on the treatment.
glht(gensym.mod, linfct = t(c(0,0,0,1,52)), rhs = 0, alternative = "greater") %>% summary()
glht(gensym.mod, linfct = t(c(0,0,0,1,52)), rhs = 0, alternative = "two.sided") %>% confint()



# 2 Use a general linear hypothesis test on your longitudinal MLR to test if the treatment stops vision loss over time.
glht(gensym.mod, linfct = t(c(0,0,1,0,1)), rhs = 0, alternative = "less") %>% summary()
glht(gensym.mod, linfct = t(c(0,0,1,0,1)), rhs = 0, alternative = "two.sided") %>% confint()


# 3 Use a general linear hypothesis test on your longitudinal MLR to test if patients on/off the drug with a baseline of 29 are at risk of going legally blind.
# first a' 
glht(gensym.mod, linfct = t(c(1,29,52,0,0)), rhs = 20, alternative = "less") %>% summary()
glht(gensym.mod, linfct = t(c(1,29,52,0,0)), rhs = 20, alternative = "two.sided") %>% confint()

# second a'
glht(gensym.mod, linfct = t(c(1,29,52,1,52)), rhs=20, alternative = "less") %>% summary()
glht(gensym.mod, linfct = t(c(1,29,52,1,52)), rhs=20, alternative = "two.sided") %>% confint()

