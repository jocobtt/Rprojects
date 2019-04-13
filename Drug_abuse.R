library(rgeos)
library(ggplot2)
library(rgdal)
library(broom)
library(GGally)
library(tidyverse)
library(spdep)
library(ngspatial)
library(pbapply)
# reading in and handling the shape file 
# download the file and set working directory to where downloaded zip file is
shpData <- readOGR(dsn = 'Drug', layer = 'DrugAbuse')
class(shpData)
shpData@data$id <- rownames(shpData@data)
shpData.df <- tidy(shpData, region = 'id')
shpData.df <- merge(shpData.df, shpData@data, by = 'id')

str(shpData.df)
head(shpData.df)
# 1 create a pairs plot to assess the relationship between log(RR) and the explantory variables

head(shpData.df)
# ggpairs plot for explanatory variables vs RR
ggal_df <- shpData.df %>% select(PctWhite, MedInc, Pct18to34, RR)
ggpairs(ggal_df)

# 2 create a choropleth map of log(RR)
ggplot(data=shpData.df, aes(x = long, y = lat, group = group, fill = log(RR))) + geom_polygon(color='black') + scale_fill_distiller(palette = 'RdBu')

# 3 Fit a lm() of log(RR) using PctWhite, MedInc and Pct18to34 as explanatory variables.
# Perform a Moran’s I test on the residuals to see if there is spatial correlation in the residuals.
rr_mod <- lm(log(RR) ~ PctWhite + MedInc + Pct18to34, data = shpData@data)
summary(rr_mod)

# perform morans test 
moran.test(x = resid(rr_mod), listw = nb2listw(poly2nb(shpData)))

# 4 Perform a Geary’s C test on your residuals from #3 above to double check if there is spatial correlation in the residuals.
geary.test(x = resid(rr_mod), listw = nb2listw(poly2nb(shpData)))

# 5 Map the residuals from the lm() fit to see if there is spatial correlation.
shpData.df$lmResids <- resid(rr_mod)[match(shpData.df$id, shpData@data$id)]
ggplot(data=shpData.df, aes(x = long, y = lat, group = group, fill = lmResids)) + geom_polygon(color='black') + scale_fill_distiller(palette = 'RdBu')


# Spatial MLR model fitting 

# 1 Fit a CAR model to the drug abuse data using PctWhite, MedInc and Pct18to34 as explanatory variables but do NOT specify the attractive option (use the default value).
# This will throw an error but print out how many degrees of freedom you are allowed to use.
ca <- nb2mat(poly2nb(shpData), style = 'B')
colnames(ca) <- rownames(ca)
no_attrac <- sparse.sglmm(formula = log(RR) ~ PctWhite + MedInc + Pct18to34, data = shpData@data, 
                          A = ca, verbose = TRUE, method = 'RSR', x = TRUE)

# 2 Fit a CAR model to the drug abuse data using PctWhite, MedInc and Pct18to34 as explanatory variables. Use the highest degree of “attractive” spatial correlation that you found in #1 above and maxit=10000.
attrac_mod <- sparse.sglmm(formula = log(RR) ~ PctWhite + MedInc + Pct18to34, data = shpData@data, attractive = 18, 
                           A = ca, maxit = 10000, verbose = TRUE, method = 'RSR', x = TRUE)
# validating spatial MLR model assumptions and predictions 

# 1 Check the assumption of linearity using added-variable plots from an independent model (note: correlation doesn’t change the linearity at all so you can just fit an independent model and look at the added-variable plots).
car::avPlots(rr_mod)
# 2 Check the assumption of independence by decorrelating residuals and performing Moran’s I or Geary’s C tests to make sure there is no more spatial correlation.
dec_resid <- resid(attrac_mod)
moran.test(x = dec_resid, listw = nb2listw(poly2nb(shpData)))

# 3 Draw a choropleth map of the decorrelated residuals to visually verify that the residuals are no longer spatially correlated.
shpData.df$decor <- resid(attrac_mod)[match(shpData.df$id, shpData@data$id)]

ggplot(data=shpData.df, aes(x = long, y = lat, group = group, fill = decor)) + geom_polygon(color='black') + scale_fill_distiller(palette = 'RdBu')

# 4 Check the assumption of normality by drawing a histogram of the decorrelated residuals.
ggplot() + geom_histogram(aes(x = dec_resid))
# 5 Check the assumption of equal variance by plotting the decorrelated residuals vs. the fitted values.
ggplot() + geom_point(aes(x = fitted(attrac_mod), y = resid(attrac_mod)))


# Statistical Inference 

# 1 Print out the summary of the CAR model fit and identify the estimates and 95% credible intervals of your explanatory variables.

summary(attrac_mod)
# 2 Create a choropleth map of the spatially correlated residuals to identify states that, after accounting for the explanatory variables, have an elevated level of risk.
spatialResidz <- attrac_mod$M%*%t(attrac_mod$gamma.sample)
spatialResidz <- rowMeans(spatialResidz)
shpData.df$spatialResidz <- spatialResidz[match(shpData.df$id, shpData@data$id)]
ggplot(data=shpData.df, aes(x = long, y = lat, group = group, fill = spatialResidz)) + geom_polygon(color='black') + scale_fill_distiller(palette = 'RdBu')
