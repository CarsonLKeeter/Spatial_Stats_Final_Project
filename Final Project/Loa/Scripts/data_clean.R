setwd("C:/Users/keete/Documents/Fall 2019/Spatial Stats/Final Project/Loa")

#### Data Clean & Variable Creation #### 

## Libraries 

library(geoR)
library(tidyverse)
library(readxl)
library(sf)
library(sp)
library(gstat)
library(ggfortify)
library(maps)
library(maptools)
library(sf)
library(tmap)
library(geostatsp)
library(RColorBrewer)
library(ggthemes)
library(geosphere)
library(spaMM)

#### Read in data
loa <- read.table("loaloa.txt", header = F)

#### Rename variables
names(loa) <- c(
  "lon", 
  "lat",
  "number_tested",
  "number_positives",
  "elevation",
  "max_NDVI",
  "sd_NDVI"
)

summary(loa)

##### Create % positive variable 
names(loa)

##### Create Distance from major cities variable

loa <- loa %>% 
  mutate(perc_pos = (number_positives/number_tested)*100) %>% 
  rowwise() %>% 
  mutate(cap_dist = distHaversine(c(lon, lat), c(11.501346, 3.844119)),  # Yaounde, Cam.
         cap_dist = cap_dist/1000,
         cap2_dist = distHaversine(c(lon, lat), c(9.786072, 4.061536)),  # Douala, Cam.
         cap2_dist = cap2_dist/1000,
         cap_dist = as.numeric(cap_dist),
         cap2_dist = as.numeric(cap2_dist)
  )


summary(loa)
max_lon <- max(loa$lon)
min_lon <- min(loa$lon)

max_lat <- max(loa$lat)
min_lat <- min(loa$lat)

correction <- .25
### Note: Yaounde is capital and 1st largest at 1.9 mil. 
###       Douala is 2nd largest city at 1.8 mil. (as of 2005 Census)

##### Spatial points dataframes

loa_sp <- SpatialPoints(cbind(loa$lon, loa$lat), proj4string = CRS("+proj=longlat"))

loa_sp <- spTransform(loa_sp, CRS("+init=epsg:26391"))    # Needed for maps

loa_sf <- st_as_sf(
  loa,
  coords = c("lon", "lat"),
  crs = CRS("+proj=utm +zone=30")
)

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

### End of data clean ### 