library(cowplot)
library(ggplot2)
library(GGally)
library(nlme)
library(car)
library(multcomp)
library(tidyverse)
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R')

pm_exp <- read.table("https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/3%20-%20Project/Data/BreathingZonePM.txt", header = TRUE, sep = ' ')
head(pm_exp)
str(pm_exp)
pm_exp$Aerosol <- log(pm_exp$Aerosol)
pm_exp$Stationary <- log(pm_exp$Stationary)
pm_exp$ID <- as.factor(pm_exp$ID)
###########################
#          EDA            #
###########################
ggpairs(pm_exp)
# aerosol vs stationary - make the lowest y value be 0 not any lower
ggplot(data = pm_exp, aes(x = Stationary, y = Aerosol)) + geom_point() + geom_smooth()

# activity vs aerosol - make it so activity is numerical title 
ggplot(data = pm_exp, aes(x = Activity, y = Aerosol, group = Activity)) + geom_boxplot()

# correlation of values
cor(pm_exp$Aerosol, pm_exp$Stationary)
cor(pm_exp$Aerosol, pm_exp$Minute)

###########################
# statistical model       #
###########################

# statistical model - MLR for linarity assumption

mlr_pm <- lm(Aerosol ~ Stationary+ID+Minute+Activity, data = pm_exp)
summary(mlr_pm)
length(mlr_pm$residuals)/length(unique(pm_exp$ID))
cor_mat <- matrix(mlr_pm$residuals, nrow = 60, ncol = 118, byrow = TRUE)
cor(cor_mat)
# histogram of correlation matrix 
hist(cor(cor_mat))
# heatmap of correlation matrix 
heatmap(cor(cor_mat))

# statistical model - longitudinal models 
# try an ar(1) model 
ar_gls <- gls(Aerosol ~ Stationary+Activity*ID, data = pm_exp, correlation = corAR1(form = ~Minute|ID),method = 'ML')
summary(ar_gls)$tTable
confint(ar_gls)[1:9,]
  # ar(1) model is the best model 
AIC(ar_gls)

# try an ma(1) model
ma_gls <- gls(Aerosol ~ Stationary*ID, data = pm_exp, correlation = corARMA(form = ~Minute|ID, p = 0, q = 1), method = 'ML')
summary(ma_gls)
AIC(ma_gls)

###########################
# model  validation       #
###########################
# check linearity 
car::avPlots(mlr_pm)

# check independence 
srs <- stdres.gls(ar_gls)
mat_srs <- matrix(srs, nrow = 60, ncol = 118, byrow = TRUE)
hist(mat_srs)
heatmap(mat_srs)
# check normality 
ggplot() + geom_histogram(aes(x = srs))

# check equal variance 
ggplot() + geom_point(aes(y = srs, x = fitted(ar_gls)))

###########################
#    analysis results     #
###########################

# is stationary measurement alone a good explainer of pm exposure 
int_mod2 <- gls(Aerosol ~ Activity*ID, data = pm_exp, correlation = corAR1(form = ~Minute|ID),method = 'ML')
anova(int_mod2, ar_gls)

# do activities in addition to stationary measurement, explain PM exposure?
# is so what activities on average lead to higher pm exposure?
# yes - playing on floor and homework 
summary(ar_gls)$tTable
ar_gls$fitted


# are effects of activites/stationary child specific? if so discuss how much variability there is in the effects from child to child? 
# hypothesis test on interaction part 
# do this with a glht model 
a.glht <- matrix(0, nrow = length(coef(ar_gls)), ncol = 1)
a.glht[names(coef(ar_gls)) %in% c("Stationary", "Activity:ID")] <- 1
glht(ar_gls, linfct = t(a.glht), rhs = 0, alternative = "two.sided") %>% summary()
glht(ar_gls, linfct = t(a.glht), rhs = 0, alternative = "two.sided") %>% confint()




# i'll do 2, 3, split 4 

# dana do 1, 0, 5, split 4
