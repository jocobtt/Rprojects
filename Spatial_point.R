library(ggplot2)
library(GGally)
library(geoR)
library(ggmap)
library(car)
library(nlme)
library(multcomp)
library(tidyverse)
library(mvtnorm)
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R')
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/predictgls.R')

surface <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/1%20-%20PointReference/InClassCaseStudy/Data/SurfaceTemps.txt', header = TRUE, sep = ' ')
head(surface)
str(surface)
surface.full <- surface[complete.cases(surface), ]
surface.na <- surface[is.na(surface$Temp) == TRUE, ]

##########################
#          EDA           #
##########################
# side by side boxplots of temp by land cover type

ggplot(data = surface, aes(x = Surface, y = Temp)) + geom_boxplot()

# heat map of temperature over houston 
ggplot(data = surface, mapping = aes(x = Lon, y = Lat, fill = Temp)) + geom_raster() + scale_fill_distiller(palette = 'Spectral', na.value = NA)
# make above heatmap fit on map of dallas
register_google(key = 'my_key')
bb <- make_bbox(lon = Lon, lat = Lat, data = surface)
my_map <- get_map(location = bb, zoom = 11, maptype = 'satellite')
ggmap(my_map) + geom_raster(data = surface, aes(x = Lon, y = Lat, fill = Temp), alpha = 0.8) + scale_fill_distiller(palette = 'Spectral', na.value = NA) + coord_cartesian()

# fit a lm() of temp by land cover type and plot a map of the residuals to convince yourself that the residuals are correlated
temp.lm <- lm(Temp ~ Surface, data = surface)
summary(temp.lm)
resid.mat <- matrix(temp.lm$residuals, nrow = 715, ncol = 5, byrow = TRUE)
ggplot() + geom_raster(aes(x = resid.mat, y = cor(resid.mat)))
heatmap(cor(resid.mat))
hist(cor(resid.mat))
# draw a variogram of the residuals from an lm() fit of temperature to land cover type 
variog_func <- variog(coords = surface.full, data = temp.lm$residuals)
plot(variog_func)

##########################
#   Spatial MLR model    #
##########################

# choose among an expoential spehrical and gaussian correlation structure (w/ nuggests)


exp_mod <- gls(model = Temp ~ Surface, data = surface.full, correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
AIC(exp_mod)

sph_mod <- gls(model = Temp ~ Surface, data = surface.full, correlation = corSpher(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
AIC(sph_mod)

gaus_mod <- gls(model = Temp ~ Surface, data = surface.full, correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
AIC(gaus_mod)

# take best model and calculate beta hat, and variance parameter 
const_est <- coef(gaus_mod$modelStruct$corStruct, unconstrained = FALSE)
const_est

beta_hat <- gaus_mod$coefficients
beta_hat

variance <- gaus_mod$sigma
variance

##########################################
#   Model Validation and prediction      #
##########################################

# check linearity 
car::avPlots(temp.lm)
# check independence 
decor_resid <- stdres.gls(gaus_mod)
mat_resid <- matrix(decor_resid, nrow = 715, ncol = 5, byrow = TRUE)
hist(cor(mat_resid))
# check normaility assumption 
ggplot() + geom_histogram(aes(x = decor_resid))

# check equal variance 
ggplot() + geom_point(aes(x = fitted(gaus_mod), y = decor_resid))

# clock how long it takes to fit the spatial model with system.time()
system.time({
  gaus_mod <- gls(model = Temp ~ Surface, data = surface.full, correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
  
})

# run 50 cv studies to assess predictive accuracy of of model in terms of biase, rpmse, coverage and width use a progress bar to 
# track the progress of the loop. compare these estimates to the predictions under an uncorrelated model with lm()
set.seed(303)
n.cv <- 50 
n.test <- .1*nrow(surface)
bias <- rep(x = NA, times = n.cv)
rpmse <- rep(x = NA, times = n.cv)
cov <- rep(x = NA, times = n.cv)
wid <- rep(x = NA, times = n.cv)
bias.lm <- rep(x = NA, times = n.cv)
rpmse.lm <- rep(x = NA, times = n.cv)
cov.lm <- rep(x = NA, times = n.cv)
wid.lm <- rep(x = NA, times = n.cv)
prog_b <- txtProgressBar(min = 0, max = n.cv, style = 3)
for(cv in 1:n.cv) {
  # cv code 
  # split into test and training sets
  test.observations <- sample(1:nrow(surface.full), size = n.test)
  test.data <- surface.full[test.observations, ]
  train.data <- surface.full[-test.observations, ]
  # fit model - gls
  test.mod <- gls(Temp ~ Surface, data = train.data,
                  correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), method = "ML")
  # fit model - lm 
  test.mod.lm <- lm(Temp ~ Surface, data = train.data)
  # generate predictions - gls
  predd <- predictgls(test.mod, newdframe = test.data)
  # predictions for lm 
  preds.lm <- predict.lm(test.mod.lm, newdata = test.data, interval = 'prediction')
  #upper and lower prediction intervals - gls
  pred.low <- predd$Prediction - qt(1-.05/2, df = nrow(train.data)-length(coef(test.mod))) * predd$SE.pred
  pred.up <- predd$Prediction + qt(1-.05/2, df = nrow(train.data)-length(coef(test.mod))) * predd$SE.pred
  
  # calculate bias - gls
  bias[cv] <- mean(predd[, 3] - test.data[['Temp']])
  bias.lm[cv] <- mean(preds.lm[,'fit'] - test.data[['Temp']])
  # calculate rpmse - gls
  rpmse[cv] <- (predd[, 3] - test.data[['Temp']])^2 %>% mean() %>% sqrt()
  rpmse.lm[cv] <- (preds.lm[, 'fit'] - test.data[['Temp']])^2 %>% mean() %>% sqrt()
  # calculate coverage - gls
  cov[cv] <- ((test.data[['Temp']] > pred.low) & (test.data[['Temp']] < pred.up)) %>% mean()
  cov.lm[cv] <- ((test.data[['Temp']] > preds.lm[, 'lwr']) & (test.data[['Temp']] < preds.lm[, 'upr'])) %>% mean()
  # calculate width - gls
  wid[cv] <- (pred.up - pred.low) %>% mean()
  wid.lm[cv] <- (preds.lm[, 'upr'] - preds.lm[, 'lwr']) %>% mean()
  # update progressbar
  setTxtProgressBar(prog_b, cv)
}
close(prog_b)
##########################
# Statistical Inference  #
##########################

# use an F-test to see if temperatures are different across any of the land-cover types 
red_mod <- gls(model = Temp ~ 1, data = surface.full, correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
anova(gaus_mod, red_mod)
# create confidence intervals for each effect of land cover and determine which land cover types result in increased temperatures. 
gaus_mod$coefficients
confint(gaus_mod, level = .95)
# perform a GLHT to construct a confidence interval of the difference in temperature between savannah and urban land covers 
dif_sav <- t(c(1, 0, 0, 1, 0) - c(1, 0, 0, 0, 1))
sav_ur <- glht(gaus_mod, linfct = matrix(dif_sav, nrow = 1), rhs = 0, alternative = 'two.sided') %>% confint()
sav_ur

# create and map predictions of the temperature at each location that was impeded by cloud cover 

predict_na <- predictgls(gaus_mod, surface.na)
missingpreds <- predictgls(gaus_mod, newdframe = surface.na)$Prediction
surface$Temp[which(is.na(surface$Temp))] <- missingpreds 
ggplot(surface, aes(x = Lon, y = Lat, fill = Temp)) + geom_raster() + scale_fill_distiller(palette = "Spectral", na.value = NA)
# plot on top of google maps
register_google(key = 'my_key')
bb <- make_bbox(lon = Lon, lat = Lat, data = predict_na)
my_map <- get_map(location = bb, zoom = 11, maptype = 'satellite')
ggmap(my_map) + geom_raster(data = surface, aes(x = Lon, y = Lat, fill = Temp), alpha = 0.8) + scale_fill_distiller(palette = 'Spectral', na.value = NA) + coord_cartesian()


