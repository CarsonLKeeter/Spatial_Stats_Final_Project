setwd("C:/Users/keete/Documents/Fall 2019/Spatial Stats/Final Project/Loa")


## Libraries 

library(geoR)
library(tidyverse)
library(readxl)
library(sf)
library(sp)
library(mvtnorm)
library(gstat)
library(ggfortify)
library(maps)
library(maptools)
library(sf)
library(tmap)
library(geostatsp)
library(RColorBrewer)

##### Loa Loa 


##### OLS model 
model <- lm(
  perc_pos ~ max_NDVI + cap_dist + cap2_dist + elevation,
  data = loa
)

summary.lm(model)

autoplot(
  model,
  smooth.colour = NA
) + 
  theme_classic(
    
  )
  

##### Obtain residuals 
loa$resid <- resid(model)

##### Create spatial points dataframe 
loa_sp <- SpatialPoints(cbind(loa$lon, loa$lat), proj4string = CRS("+proj=longlat"))

loa_sp <- spTransform(loa_sp, CRS("+init=epsg:26391"))


##### Cutoff and for variogram
cutoff <- .65*max(
  dist(
    cbind(
      loa$lon,
      loa$lat
    )
  )
)

bins <- 15

###### Variogram/Covariogram (with scale variables)
variogram <- variogram(
  scale(perc_pos) ~ 1, 
  locations = ~lon + lat,
  data = loa,
  cutoff = cutoff,
  width = cutoff/bins
)

plot(
  variogram,
  pch = 16, 
  cex = 1.5
)

covariogram <- variogram(
  scale(perc_pos) ~ 1, 
  locations = ~lon + lat,
  data = loa,
  cutoff = cutoff,
  width = cutoff/bins,
  covariogram = T,
  map = T
)

plot(
  covariogram,
  contour = T
)


##### Spatial Models

##### More spatial points dataframes
d <- data.frame(
  st_coordinates(
    loa_sf
  ),
  loa[, -1:-2]
)

loa_model_sp  <- SpatialPointsDataFrame(
  coords = d[, 1:2],
  data = d,
  proj4string = CRS(
    "+proj=utm +zone=30"
  )
)

###### Anisotropic Model (with scaled variables)
fit_aniso <- lgm(
  scale(perc_pos) ~ scale(max_NDVI) + scale(elevation) + cap_dist + cap2_dist, 
  data = loa_model_sp,                
  grid = 100,                    
  shape = .5,
  fixShape = FALSE,   
  nugget = .5,
  fixNugget = FALSE,  
  aniso = TRUE,
  reml = TRUE
) 

summary(fit_aniso)

AIC(fit_aniso)

##### Isotropic model (with scaled variables)
fit_iso <- lgm(
  scale(perc_pos) ~ scale(max_NDVI) + scale(elevation)+ cap_dist + cap2_dist ,
  data = loa_model_sp,                
  grid = 100,                    
  shape = .5,
  fixShape = FALSE,   
  nugget = .5,
  fixNugget = FALSE,  
  aniso = FALSE,
  reml = TRUE,
  boxcox = TRUE
) 

summary(fit_iso)

AIC(fit_iso)

##### Non-normal Spatial Model 
library(spaMM)

spamm_fit <- fitme(
  scale(perc_pos) ~ scale(max_NDVI) + scale(elevation) +
    Matern(1|lon + lat),
  fixed = list(
    nu = .5
  ),
  method = "REML",
  data = loa
)

spamm_fit

AIC(spamm_fit)


##### Figures for An- and Isotropic Model (Predicted SD and Predicted Prev.)
par(mfcol=c(2,2), mai=c(0.5,0.5,0.5,0.5))

plot(fit_aniso$predict[["krigeSd"]],main='Anisotropic Prediction SDs')

plot(fit_iso$predict[["krigeSd"]],main='Isotropic Prediction SDs')

plot(fit_aniso$predict[["predict"]],main='Anisotropic Predicted Prevalence')

plot(fit_iso$predict[["predict"]],main='Isotropic Predicted Prevalence')


##### Figure for non-normal model 

filled.mapMM(
  spamm_fit,
  nlevels = 10,
  gridSteps = 50,
  add.map = TRUE,
  axes = F,
  yrange = c(
    min_lat - correction, 
    max_lat + correction
  ),
  xrange = c(
    min_lon - correction, 
    max_lon +  correction
    )
  )




