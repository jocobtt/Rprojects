library(rgeos)
library(ggplot2)
library(rgdal)
library(broom)
library(GGally)
library(tidyverse)
library(spdep)
library(ngspatial)
library(pbapply)
library(car)

setwd('~/Stat_469')
Datazip <- readOGR(dsn = 'HoustonHeat', layer = 'HoustonHeat')
Datazip@data$RR <- log(Datazip@data$RR)
# convert shape file to dataframe for ggplot 

class(Datazip)
Datazip@data$id <- rownames(Datazip@data)
Data.df <- tidy(Datazip, region = 'id')
Data.df <- merge(Data.df, Datazip@data, by = 'id')

# exploratory plots 
ggplot(data = Data.df, aes(x = long, y = lat, group = group, fill = RR)) + geom_polygon(color = NA) + scale_fill_distiller(palette = 'RdBu')

ggpairs(Datazip@data[,-ncol(Datazip@data)])

# fit independent MLR model 
ind_mod <- lm(RR ~ . -id, data = Datazip@data)
summary(ind_mod)
# map residuals on 
Data.df$lmResids <- resid(ind_mod)[match(Data.df$id, Datazip@data$id)]
ggplot(data=Data.df, aes(x = long, y = lat, group = group, fill = lmResids)) + geom_polygon(color = NA) + scale_fill_distiller(palette = 'RdBu')
# do moran test 
moran.test(x = resid(ind_mod), listw = nb2listw(poly2nb(Datazip)))
# do geary test
geary.test(x = resid(ind_mod), listw = nb2listw(poly2nb(Datazip)))

# saw spatial correlation so write out a Car model 
'''y = xbeta + epsilon 
epsilon ~ CAR(sigma^2)
epsilon_i |(conditional) epsilon_-i ~ N(epsilonbar_i, sigma^2/n_i)
epsilonbar_i = 1/n_i sum(j neighbors i) e_j
ni = # of neighbors of observation i 
beta = (b_o, b_1, b_2...)
'''

# fit the CAR model 
A <- nb2mat(poly2nb(Datazip), style = 'B')
isSymmetric(A)  # values are symmetric but names dont match up 
colnames(A) <- rownames(A)
heat.car <- sparse.sglmm(RR ~ . -id, data = Datazip@data, A = A, method = 'RSR',
                         attractive = 250, minit = 5000, maxit = 5000, 
                         x = TRUE, verbose = TRUE)
# validate assumptions made to fit the model 
# linearity 
avPlots(ind_mod)
# independence 
Data.df$decorResids <- resid(heat.car)[match(Data.df$id, Datazip@data$id)]
ggplot(data=Data.df, aes(x = long, y = lat, group = group, fill = decorResids)) + geom_polygon(color = NA) + scale_fill_distiller(palette = 'RdBu')

moran.test(x = resid(heat.car), listw = nb2listw(poly2nb(Datazip)))
# do geary test
geary.test(x = resid(heat.car), listw = nb2listw(poly2nb(Datazip)))
# check normality 
qplot(resid(heat.car), geom = 'histogram')
ks.test(resid(heat.car), 'pnorm')
# check equal variance 
qplot(fitted(heat.car), resid(heat.car), geom = 'point')

# conf/credible intervals 
summary(heat.car)

# map correlated residuals 
corResids <- heat.car$M%*%t(heat.car$gamma.sample) %>% rowMeans()
Data.df$corResids <- corResids[match(Data.df$id, Datazip@data$id)]
ggplot(data=Data.df, aes(x = long, y = lat, group = group, fill = corResids)) + geom_polygon(color = NA) + scale_fill_distiller(palette = 'RdBu')


