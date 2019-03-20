library(cowplot)
library(ggplot2)
library(GGally)
library(nlme)
library(car)
library(multcomp)
library(tidyverse)
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R')

card <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/2%20-%20Longitudinal/HWCaseStudy/Data/Tachycardia.txt', sep = ' ', header = TRUE)
head(card)
card$HEARTRTE <- log(card$HEARTRTE)
str(card)

# 1 EDA - look at relationship between HEARTRTE and various explanatory variables
cor(card)[,11]
# heartrate vs glucose
ggplot(data = card, aes(x = GLUCOSE, y = HEARTRTE)) + geom_point() + geom_smooth()
# heartrate vs diabp
ggplot(data = card, aes(x = DIABP, y = HEARTRTE)) + geom_point() + geom_smooth()
#heartrate vs sex
ggplot(data = card, aes(x = SEX, y = HEARTRTE, group = SEX)) + geom_boxplot()
# heartrate vs sysbp 
ggplot(data = card, aes(x = SYSBP, y = HEARTRTE)) + geom_point() + geom_smooth()

# 2 find a MLR model w/ linear effect of all variables except randind and period. 
model_df <- card %>% dplyr::select(2:12)
ind_mlr <- lm(HEARTRTE ~ ., data = model_df)

resid_mat <- matrix(ind_mlr$residuals, nrow = 1734, ncol = 3, byrow = TRUE)
cor(resid_mat)

# 3 fit a longitudinal MLR model with AR1, MA1 and general symmetric correlation matrix w/in each patient but independent across patients. 
# compare model fits using AIC 
# general symmetric model
long_mod_gen <- gls(HEARTRTE ~ . -PERIOD -RANDID, data = card, correlation = corSymm(form = ~1:3|RANDID), method = 'ML')
summary(long_mod_gen)
AIC(long_mod_gen)
# ar1 model 
long_mod_ar <- gls(HEARTRTE ~ . -PERIOD -RANDID, data = card, correlation = corAR1(form = ~PERIOD|RANDID), method = 'ML')
summary(long_mod_ar)
AIC(long_mod_ar)
# ma1 model 
long_mod_ma <- gls(HEARTRTE ~ . -PERIOD -RANDID, data = card, correlation = corARMA(form=~PERIOD|RANDID, p = 0, q = 1), method = 'ML')
summary(long_mod_ma)
AIC(long_mod_ma)

# 4 write out the model in terms of parameters. explain and interpret any parameters associated with the model


# 5 fit your longitudinal model and validate any assumptions you made to fit the model 
best_mod <- gls(HEARTRTE ~ . -PERIOD -RANDID, data = card, correlation = corSymm(form = ~1:3|RANDID), method = 'ML')
summary(best_mod)
# sigma_hat squared
coef(best_mod$modelStruct$corStruct, unconstrained = FALSE)
# beta hat 
best_mod$coefficients
# sigma hat 
best_mod$sigma

#  check linearity assumption
car::avPlots(ind_mlr)

#  check independence assumption by decorrelating residuals and looking at the sample correlation matrix of w/in subject residuals
sres <- stdres.gls(best_mod)
s_resids <- matrix(sres, nrow = 5202, ncol = 3, byrow = TRUE)
cor(s_resids)
#  check normality w/ histogram of decorrelated residuals 
ggplot() + geom_histogram(aes(x = sres))

# check equal variance w/ decorrelated residuals vs fitted values 
ggplot() + geom_point(aes(y = sres, x = fitted(best_mod)))

# 6 Is DIABETES a risk factor for Tachycardia? Justify your answer and explain any effect of DIABETES on heart rate (include uncertainty in your conclusions).
# check to see if matrix is correct
summary(best_mod)$tTable["DIABETES","p-value"]
summary(best_mod)$tTable["DIABETES", "Value"]
confint(best_mod, level = 0.95)["DIABETES",]
# 7 What is the expected difference in heart rate for a female patient with at age 35 who is a smoker vs. an older female of 45 but not a smoker (assume the other characteristics are the same)?
# What does this say about the effect of smoking?
# smoker
a_transpose <- t(c(1,0,2,0,35,0,0,1,0,0,0,0,0) - c(1,0,2,0,45,0,0,0,0,0,0,0,0))
glht(best_mod, linfct = a_transpose, rhs = 0, alternative = "two.sided") %>% summary()
glht(best_mod, linfct = a_transpose, rhs = 0, alternative = "two.sided") %>% confint()
# i do 1-4 
# dana does 5-7

