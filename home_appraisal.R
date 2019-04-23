library(ggplot2)
library(GGally)
library(geoR)
library(ggmap)
library(car)
library(nlme)
library(multcomp)
library(tidyverse)
library(mvtnorm)
library(MASS)
library(lmtest)
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/stdres.gls.R')
source('https://raw.githubusercontent.com/MJHeaton/glstools/master/predictgls.R')


url <- 'https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/3%20-%20Project/Data/HousingPrices.csv'
homez <- read.table(url, sep = ',', header = TRUE)
head(homez)
str(homez)


homez.full <- homez[complete.cases(homez), ]
str(homez.full)
homez.na <- homez[is.na(homez$Price) == TRUE, ]

###########################
#           EDA           #
###########################
# year remodeled vs price
ggplot(data = homez.full, aes(x = Year.Remod.Add, y = Price)) + geom_point()
# liv area vs price
ggplot(data = homez.full, aes(x = Gr.Liv.Area, y = Price)) + geom_point()
# boxplot of house style vs price
ggplot(data = homez.full, aes(x = House.Style, y = Price)) + geom_boxplot() + labs(x='House Style')
# do this for the residuals too
ggplot(data = homez.full, aes(x = Lon, y = Lat, color = Price)) + geom_point() + scale_color_distiller(palette = "RdBu", na.value = "white")



#############################
#      Statistical model    #
#############################
# try lm model and see what issues are in the data 
mlr_price <- lm(Price ~ . -Lon -Lat, data = homez.full)
summary(mlr_price)

# check independence - looks ok.. idk if this is right though 
resid.mat <- matrix(mlr_price$residuals, nrow = 465, ncol = 11, byrow = TRUE)
hist(cor(resid.mat))

ggplot(data = homez.full, aes(x = Lon, y = Lat, color = mlr_price$residuals)) + geom_point() + scale_color_distiller(palette = "RdBu", na.value = "white") + labs(color = "Residuals")

# plot variogram 
myVariogram <- variog(coords = homez.full[,c("Lon", "Lat")] , data = mlr_price$residuals)
plot(myVariogram)   # no independence 

# check normality 
ks.test(rstandard(mlr_price), 'pnorm')
# check equal variance - equal variance is off 
bptest(mlr_price)

# write out model in mathematical detail 
# & write how you will justify any assumptions model uses 

# try gls models
exp_mod <- gls(model = Price ~ . -Lon -Lat, data = homez.full, 
               correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), 
               weights = varExp(form = ~Gr.Liv.Area), method = 'ML')
AIC(exp_mod)
sph_mod <- gls(model = Price ~ . -Lon -Lat, data = homez.full, 
               correlation = corSpher(form = ~ Lon + Lat, nugget = TRUE), 
               weights = varExp(form = ~ Gr.Liv.Area), method = 'ML')
AIC(sph_mod)

gaus_mod <- gls(model = Price ~ . -Lon -Lat, data = homez.full, 
                correlation = corGaus(form = ~ Lon + Lat, nugget = TRUE), 
                weights = varExp(form = ~ Gr.Liv.Area), method = 'ML')
AIC(gaus_mod)

#############################
#     Model Validation      #
#############################
# check linearity 
avPlots(mlr_price)

# check independence 
st_exp <- stdres.gls(exp_mod)
stresid_mat <- matrix(st_exp, nrow = 465, ncol = 11, byrow = TRUE)
hist(cor(stresid_mat))

# fix coloring for the graphs 
ggplot(data = homez.full, mapping = aes(x = Lon, y = Lat, fill = st_exp)) + geom_point() + scale_fill_distiller(palette = 'RdBu', na.value = NA)

# check normality 
qplot(st_exp, geom = 'histogram')

# check equal variance - this still looks a bit off
qplot(fitted(exp_mod), st_exp, geom = 'point')

# how well does model fit data 
y_hat <- predictgls(exp_mod, newdframe = homez.full)
rsq <- (cor(homez.full$Price, y_hat$Prediction))^2
rsq


# validate how well model does at predicting 
set.seed(303)
n.cv <- 50 
n.test <- .1*nrow(homez)
bias <- rep(x = NA, times = n.cv)
rpmse <- rep(x = NA, times = n.cv)
cov <- rep(x = NA, times = n.cv)
wid <- rep(x = NA, times = n.cv)

for(cv in 1:n.cv) {
  # split into test and training sets
  test.observations <- sample(1:nrow(homez.full), size = n.test)
  test.data <- homez.full[test.observations, ]
  train.data <- homez.full[-test.observations, ]
  # fit model - gls
  test.mod <- gls(Price ~ . -Lon -Lat , data = train.data,
                  correlation = corExp(form = ~ Lon + Lat, nugget = TRUE), 
                  weights = varExp(form = ~ Gr.Liv.Area), method = "ML")
  # generate predictions - gls
  predd <- predictgls(test.mod, newdframe = test.data)
  #upper and lower prediction intervals - gls
  pred.low <- predd$Prediction - qt(1-.05/2, df = nrow(train.data)-length(coef(test.mod))) * predd$SE.pred
  pred.up <- predd$Prediction + qt(1-.05/2, df = nrow(train.data)-length(coef(test.mod))) * predd$SE.pred
  
  # calculate bias - gls
  bias[cv] <- mean(predd$Prediction - test.data[['Price']])
  # calculate rpmse - gls
  rpmse[cv] <- (predd$Prediction - test.data[['Price']])^2 %>% mean() %>% sqrt()
  # calculate coverage - gls
  cov[cv] <- ((test.data[['Price']] > pred.low) & (test.data[['Price']] < pred.up)) %>% mean()
  # calculate width - gls
  wid[cv] <- (pred.up - pred.low) %>% mean()
}
hist(rpmse)
mean(rpmse)

hist(cov)
mean(cov)

hist(wid)
mean(wid)

hist(bias)
mean(bias)

#############################
#     analysis results      #
#############################
# how well do home characteristics explain sale price 
# use r squared here too or rpmse 
cor(fitted(exp_mod), homez.full$Price)^2

# what factors increase the sale price of a home 
summary(exp_mod)
confint(exp_mod, level = .95)


# does variability of sale price increase w/ size of home (as given by living area)
exp_mod$coefficients
intervals(exp_mod, level = 0.95)


# what is your predicted/appraised sale price for homes in the dataset that do not have a sale price 
missingpreds <- predictgls(exp_mod, newdframe = homez.na)$Prediction
homez$Price[which(is.na(homez$Price))] <- missingpreds 
ggplot(data = homez, aes(x = Lon, y = Lat, color = Price)) + geom_point() + scale_color_distiller(palette = "RdBu", na.value = "white")


