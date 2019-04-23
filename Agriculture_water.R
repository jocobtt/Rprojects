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


water_hold <- read.table('https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/1%20-%20PointReference/HWCaseStudy/Data/WaterHoldingCapacity.txt', header = TRUE, sep = ' ')
head(water_hold)
str(water_hold)
View(water_hold)
water.full <- water_hold[complete.cases(water_hold), ]
water.na <- water_hold[is.na(water_hold$WHC) == TRUE, ]
########################
#         EDA          #
########################
# create exploratory plots of the data

ggplot(data = water.full, aes(x = EC, y = WHC)) + geom_point()
ggplot(data = water.full, aes(x = Yield, y = WHC)) + geom_point()
ggpairs(water.full)

########################
#     fit mlr          #
########################
# fit an independent MLR model w/ linear effect between yield, ec, and whc. 

water_lm <- lm(WHC ~ Yield + EC, data = water_hold)
summary(water_lm)
# make a matrix of the residuals to explore correlation 
cor_mat <- matrix(water_lm$residuals, nrow = 89, ncol = 3, byrow = TRUE)
hist(cor(cor_mat))
# plot the variogram 
variog_func <- variog(coords = water.full, data = water_lm$residuals)
plot(variog_func)
########################
#     fit gls          #
########################
# fit a spatial model using exponential, spherical, and gaussian with a nugget effect. 

water_gaus <- gls(model = WHC ~ Yield + EC, data = water.full, 
                  correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
AIC(water_gaus)
# water_exp is best has best aic value
water_exp <- gls(model = WHC ~ Yield + EC, data = water.full, 
                 correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
AIC(water_exp)

water_spherical <- gls(model = WHC ~ Yield + EC, data = water.full, 
                       correlation = corSpher(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
AIC(water_spherical)

# write out model

########################
# validate assumptions #
########################

# check linearity 
avPlots(water_lm)
# check independence 
exp_decor <- stdres.gls(water_exp)
resid_mat <- matrix(exp_decor, nrow = 89, ncol = 3, byrow = TRUE)
hist(cor(resid_mat))
# check normality 
ggplot() + geom_histogram(aes(x = exp_decor))
# check equal variance 
ggplot() + geom_point(aes(x = fitted(water_exp), y = exp_decor))
#########################
#   hypothesis test     #
#########################
# hypothesis test locations with higher yield have higher WHC. 

red_mod <- gls(model = WHC ~ EC, data = water.full, correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), method = 'ML')
anova(water_exp, red_mod)

water_exp$coefficients
confint(water_exp, level = .95)
#########################
#     Predictions       #
#########################
# predict WHC at all locations where WHC is missing. provide a plot of our predictions 
pred_water <- predictgls(water_exp, newdframe =  water.na)
water_hold$Temp[which(is.na(water_hold$Temp))] <- pred_water 
ggplot(water_hold, aes(x = Lon, y = Lat, fill = WHC)) + geom_raster() + scale_fill_distiller(palette = "Spectral", na.value = NA)



# extra mapping 
register_google(key = 'my_key')
bb <- make_bbox(lon = Lon, lat = Lat, data = water_hold)
my_map <- get_map(location = bb, zoom = 11, maptype = 'satellite')
ggmap(my_map) + geom_raster(data = surface, aes(x = Lon, y = Lat, fill = WHC), alpha = 0.8) + scale_fill_distiller(palette = 'Spectral', na.value = NA) + coord_cartesian()


